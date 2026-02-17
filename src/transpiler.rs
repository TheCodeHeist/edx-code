use crate::{
  error::{EdxRuntimeError, EdxTranspilationError},
  parser::{Expression, Node},
  stdlib::StdLib,
  tokenizer::{OperatorType, Token},
};

use std::collections::HashSet;

use miette::Result;

pub struct PythonTranspiler {
  ast: Node,
  closure_stack: Vec<String>,
  lines: Vec<String>,
  imported_modules: HashSet<String>, // To keep track of which modules we need to import at the top of the generated Python code
}

impl PythonTranspiler {
  pub fn new(ast: Node) -> Self {
    Self {
      ast,
      closure_stack: Vec::new(),
      lines: Vec::new(),
      imported_modules: HashSet::new(),
    }
  }

  pub fn transpile(&mut self) -> Result<String> {
    // Using DFS to traverse and execute the AST
    let body = match self.ast.clone() {
      Node::Program { body } => body,
      _ => panic!("Program node expected at the root of AST"),
    };

    for node in body {
      self.translate_node(node)?;
    }

    let mut output = String::new();
    for module in &self.imported_modules {
      output.push_str(&format!("import {}\n", module));
    }
    output.push_str(&self.lines.join("\n"));

    Ok(output)
  }

  fn translate_node(&mut self, node: Node) -> Result<()> {
    match node {
      Node::VariableDeclaration { name, value } => {
        let translated_value = self.translate_expression(value)?;
        self
          .lines
          .push(format!("{}{} = {}", self.indent(), name, translated_value));
      }
      Node::ArrayElementAssignment {
        array_name,
        indices,
        value,
      } => {
        let translated_value = self.translate_expression(value)?;
        let mut translated_array_access = array_name;

        for idx_expr in indices {
          let translated_idx = self.translate_expression(idx_expr)?;
          translated_array_access.push_str(&format!("[{}]", translated_idx));
        }

        self.lines.push(format!(
          "{}{} = {}",
          self.indent(),
          translated_array_access,
          translated_value
        ));
      }
      Node::SendIO { message, device } => {
        let translated_message = self.translate_expression(message)?;

        match device.as_str() {
          "DISPLAY" => self
            .lines
            .push(format!("{}print({})", self.indent(), translated_message)),
          _ => return Err(EdxTranspilationError {
            message: format!("Unsupported device: {}", device),
            help: "Currently, only the DISPLAY device is supported for output. Make sure you are using 'DISPLAY' as the device in your SEND statement.".into(),
          })?,
        }
      }
      Node::ReceiveIO {
        variable_name,
        input_type,
        device,
      } => match device.as_str() {
        "KEYBOARD" => {
          let input_func = match input_type.as_str() {
            "INTEGER" => "int(input())",
            "REAL" => "float(input())",
            "STRING" => "input()",
            "BOOLEAN" => "input().lower() in ['true', '1', 'yes', 'y']",
            _ => {
              return Err(EdxTranspilationError {
                message: format!("Unsupported input type: {}", input_type),
                help: "Currently, only INTEGER, REAL, STRING, and BOOLEAN input types are supported for keyboard input. Make sure you are using one of these types in your RECEIVE statement.".into(),
              })?
            }
          };

          self
            .lines
            .push(format!("{}{} = {}", self.indent(), variable_name, input_func));
        }
        _ => return Err(EdxRuntimeError {
          message: format!("Unsupported device: {}", device),
          help: "Currently, only the KEYBOARD device is supported for input. Make sure you are using 'KEYBOARD' as the device in your RECEIVE statement.".into(),
        })?,
      },
      Node::IfStatement { condition, body, else_body }  => {
        let translated_condition = self.translate_expression(condition)?;

        let this_level_indent = self.indent();
        
        self.lines.push(format!("{}if {}:", this_level_indent, translated_condition));
        self.closure_stack.push("if".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        if let Some(else_body) = else_body {
          self.lines.push(format!("{}else:", this_level_indent));
          for stmt in else_body {
            self.translate_node(stmt)?;
          }
        }

        self.closure_stack.pop();
      }
      Node::WhileLoop { condition, body } => {
        let translated_condition = self.translate_expression(condition)?;
        self.lines.push(format!("{}while {}:", self.indent(), translated_condition));
        self.closure_stack.push("while".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::PostConditionalLoop { body, condition } => {
        self.lines.push(format!("{}while True:", self.indent()));
        self.closure_stack.push("while".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        let translated_condition = self.translate_expression(condition)?;
        self.lines.push(format!("{}if not ({}):", self.indent(), translated_condition));
        self.lines.push(format!("{}break", self.indent().repeat(2)));

        self.closure_stack.pop();
      }
      Node::CountControlledLoop { count, body } => {
        let translated_count = self.translate_expression(count)?;
        self.lines.push(format!("{}for _ in range({}):", self.indent(), translated_count));
        self.closure_stack.push("for".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::ForLoop { variable_name, start, end, step, body } => {
        let translated_start = self.translate_expression(start)?;
        let translated_end = self.translate_expression(end)?;
        let translated_step = if let Some(step_expr) = step {
          self.translate_expression(step_expr)?
        } else {
          "1".to_string() // Default step is 1
        };

        self.lines.push(format!(
          "{}for {} in range({}, {}, {}):",
          self.indent(),
          variable_name,
          translated_start,
          translated_end,
          translated_step
        ));
        self.closure_stack.push("for".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::ForEachLoop { variable_name, iterable, body } => {
        let translated_iterable = self.translate_expression(iterable)?;
        self.lines.push(format!(
          "{}for {} in {}:",
          self.indent(),
          variable_name,
          translated_iterable
        ));
        self.closure_stack.push("for".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::ContinueStatement => {
        self.lines.push(format!("{}continue", self.indent()));
      }
      Node::BreakStatement => {
        self.lines.push(format!("{}break", self.indent()));
      }
      Node::ProcedureDeclaration { name, parameters, body } => {
        let param_list = parameters.join(", ");
        self.lines.push(format!("{}def {}({}):", self.indent(), name, param_list));
        self.closure_stack.push("def".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::FunctionDeclaration { name, parameters, body } => {
        let param_list = parameters.join(", ");
        self.lines.push(format!("{}def {}({}):", self.indent(), name, param_list));
        self.closure_stack.push("def".to_string());

        for stmt in body {
          self.translate_node(stmt)?;
        }

        self.closure_stack.pop();
      }
      Node::ProcedureCall { name, args } => {
        let translated_args: Vec<String> = args
          .into_iter()
          .map(|arg| self.translate_expression(arg))
          .collect::<Result<Vec<String>>>()?;

        self.lines.push(format!("{}{}({})", self.indent(), name, translated_args.join(", ")));
      }
      Node::ReturnStatement { value } => {
        if let Some(expr) = value {
          let translated_value = self.translate_expression(expr)?;
          self.lines.push(format!("{}return {}", self.indent(), translated_value));
        } else {
          self.lines.push(format!("{}return", self.indent()));
        }
      }
      n => {
        return Err(EdxTranspilationError {
          message: format!("Unsupported node type: {}", n),
          help: "This error indicates that the transpiler does not yet support a certain language construct used in your code. Please check the documentation for supported features and make sure you are using only those constructs in your EDX program.".into(),
        })?
      }
    }

    Ok(())
  }

  fn translate_expression(&mut self, expr: Expression) -> Result<String> {
    // This function will recursively translate an expression into its Python equivalent and return it as a string. The returned string will represent the Python code for the given expression, which can then be used in the transpiled output.

    match expr {
      Expression::IntConst(token) => {
        if let Token::IntegerNumeric(value) = token {
          Ok(value.to_string())
        } else {
          Err(EdxTranspilationError {
            message: "Expected IntegerNumeric token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::RealConst(token) => {
        if let Token::RealNumeric(value) = token {
          Ok(value.to_string())
        } else {
          Err(EdxTranspilationError {
            message: "Expected RealNumeric token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::StrConst(token) => {
        if let Token::StringLiteral(value) = token {
          Ok(format!("{:?}", value)) // Using debug formatting to ensure proper escaping of special characters in the string literal
        } else {
          Err(EdxTranspilationError {
            message: "Expected StringLiteral token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::BoolConst(token) => {
        if let Token::Boolean(value) = token {
          // In Python, boolean literals are capitalized (True/False), so we need to convert the value accordingly

          let python_bool = if value { "True" } else { "False" };
          Ok(python_bool.to_string())
        } else {
          Err(EdxTranspilationError {
            message: "Expected Boolean token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::Identifier(token) => {
        if let Token::Identifier(name) = token {
          Ok(name)
        } else {
          Err(EdxTranspilationError {
            message: "Expected Identifier token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::ArrayConst(elements) => {
        let translated_elements: Vec<String> = elements
          .into_iter()
          .map(|elem| self.translate_expression(elem))
          .collect::<Result<Vec<String>>>()?;

        Ok(format!("[{}]", translated_elements.join(", ")))
      }
      Expression::ArrayAccess { name, indices } => {
        let mut translated_name = format!("{}", name);

        for idx_expr in indices {
          let translated_idx = self.translate_expression(idx_expr)?;
          translated_name.push_str(&format!("[{}]", translated_idx));
        }

        Ok(translated_name)
      }
      Expression::FunctionCall { name, args } => {
        // Function calls return a value, so we need to evaluate the function and return its result

        // First, we check if the function being called is a standard library function. If it is, we can call it directly without needing to look it up in the global variables.
        let std = StdLib::import(); // Ensure the standard library is imported so we can access its functions

        match std.get_function_ref(name.clone()) {
          Some(func) => {
            let translated_args: Vec<String> = args
              .into_iter()
              .map(|arg| self.translate_expression(arg))
              .collect::<Result<Vec<String>>>()?;

            let (python_func, module_name) = std.get_python_equivalent(&func, translated_args);
            if let Some(module) = module_name {
              self.imported_modules.insert(module);
            }
            return Ok(format!(
              "{}",
              python_func
            ));
          }
          None => {}
        };

        // If it's not a standard library function, we need to look it up in the global variables to get its definition and parameters
        // For now, we'll just assume it's a user-defined function and translate it as a normal function call. In a complete implementation, we would need to check if the function exists in the global variables and handle it accordingly.
        let translated_args: Vec<String> = args
          .into_iter()
          .map(|arg| self.translate_expression(arg))
          .collect::<Result<Vec<String>>>()?;

        Ok(format!("{}({})", name, translated_args.join(", ")))
      }
      Expression::BinaryOperation {
        operator,
        left,
        right,
      } => {
        let left_val = self.translate_expression(*left)?;
        let right_val = self.translate_expression(*right)?;

        match (left_val, right_val, operator) {
          // Handle Array Access
          (l, r, OperatorType::LBRACKET) => Ok(format!("{}[{}]", l, r)),

          // Numeric addition
          (l, r, OperatorType::ADD) => Ok(format!("({} + {})", l, r)),

          // Numeric subtraction or negation
          (l, r, OperatorType::SUB) => {
            if l == "0" {
              Ok(format!("(-{})", r))
            } else {
              Ok(format!("({} - {})", l, r))
            }
          }

          // Numeric multiplication
          (l, r, OperatorType::MUL) => Ok(format!("({} * {})", l, r)),

          // Numeric exponentiation
          (l, r, OperatorType::POW) => Ok(format!("({} ** {})", l, r)),

          // Numeric division (always results in RealVar)
          (l, r, OperatorType::DIV) => Ok(format!("({} / {})", l, r)),

          // Numeric Integer division (always results in IntVar)
          (l, r, OperatorType::IDIV) => Ok(format!("({} // {})", l, r)),

          // Numeric modulus (always results in IntVar)
          (l, r, OperatorType::MOD) => Ok(format!("({} % {})", l, r)),

          // String concatenation
          (l, r, OperatorType::CAT) => Ok(format!("({} + {})", l, r)),

          // Boolean AND
          (l, r, OperatorType::AND) => Ok(format!("({} and {})", l, r)),

          // Boolean OR
          (l, r, OperatorType::OR) => Ok(format!("({} or {})", l, r)),

          // BOOLEAN NOT (unary)
          (_, r, OperatorType::NOT) => Ok(format!("(not {})", r)),

          // Compare operations
          (l, r, OperatorType::LT) => Ok(format!("({} < {})", l, r)),
          (l, r, OperatorType::GT) => Ok(format!("({} > {})", l, r)),
          (l, r, OperatorType::LTE) => Ok(format!("({} <= {})", l, r)),
          (l, r, OperatorType::GTE) => Ok(format!("({} >= {})", l, r)),
          (l, r, OperatorType::EQ) => Ok(format!("({} == {})", l, r)),
          (l, r, OperatorType::NEQ) => Ok(format!("({} != {})", l, r)),

          (lhs, rhs, op) => Err(EdxRuntimeError {
            message: format!(
              "Unsupported operation or operand types: {} {} {}",
              lhs, op, rhs
            ),
            help: "Make sure you are using supported operand types and operators.".into(),
          })?,
        }
      }
    }
  }

  fn indent(&self) -> String {
    "  ".repeat(self.closure_stack.len())
  }
}
