use std::{
  collections::HashMap,
  io::{self, Write},
};

use crate::{
  error::EdxRuntimeError,
  parser::{Expression, Node},
  tokenizer::{OperatorType, Token},
};

use miette::Result;

#[derive(Clone)]
pub enum Reference {
  RealVar(f64),
  IntVar(i64),
  StrVar(String),
  BoolVar(bool),
  ArrayVar(Vec<Reference>),
  Procedure {
    name: String,
    parameters: Vec<String>,
    body: Vec<Node>,
  },
  Function {
    name: String,
    parameters: Vec<String>,
    body: Vec<Node>,
  },
  ControlContinuation, // Used to signal a continue statement in loops
  ControlBreak,        // Used to signal a break statement in loops
}

impl Reference {
  pub fn type_name(&self) -> &'static str {
    match self {
      Reference::RealVar(_) => "RealVar",
      Reference::IntVar(_) => "IntVar",
      Reference::StrVar(_) => "StrVar",
      Reference::BoolVar(_) => "BoolVar",
      Reference::ArrayVar(_) => "ArrayVar",
      Reference::Procedure { .. } => "Procedure",
      Reference::Function { .. } => "Function",
      Reference::ControlContinuation => "ControlContinuation",
      Reference::ControlBreak => "ControlBreak",
    }
  }

  pub fn try_into_bool(&self) -> Result<bool> {
    match self {
      Reference::BoolVar(b) => Ok(*b),
      Reference::IntVar(v) => Ok(*v != 0),
      Reference::RealVar(v) => Ok(*v != 0.0),
      Reference::StrVar(v) => Ok(!v.is_empty()),
      Reference::ArrayVar(v) => Ok(!v.is_empty()),
      _ => Err(EdxRuntimeError {
        message: format!(
          "Cannot convert type '{}' to boolean for condition evaluation",
          self.type_name()
        ),
        help: "Ensure the expression in the condition evaluates to a boolean or a type that can be coerced to boolean".into(),
      }.into())
    }
  }
}

impl std::fmt::Display for Reference {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Reference::RealVar(v) => write!(f, "RealVar({})", v),
      Reference::IntVar(v) => write!(f, "IntVar({})", v),
      Reference::StrVar(v) => write!(f, "StrVar({})", v),
      Reference::BoolVar(v) => write!(f, "BoolVar({})", v),
      Reference::ArrayVar(v) => write!(f, "ArrayVar({})", v.len()),
      Reference::Procedure { name, .. } => write!(f, "Procedure({})", name),
      Reference::Function { name, .. } => write!(f, "Function({})", name),
      Reference::ControlContinuation => write!(f, "ControlContinuation"),
      Reference::ControlBreak => write!(f, "ControlBreak"),
    }
  }
}

pub enum Scope {
  Program,
  #[allow(dead_code)]
  Procedure(String), // Procedure name
  #[allow(dead_code)]
  Function(String), // Function name
}

pub struct Runtime {
  ast: Node,
  global_vars: HashMap<String, Reference>,
  local_vars: HashMap<String, Reference>,
  execution_stack: Vec<Scope>,
}

impl Runtime {
  pub fn new(ast: Node) -> Self {
    Runtime {
      ast,
      global_vars: HashMap::new(),
      local_vars: HashMap::new(),
      execution_stack: Vec::new(),
    }
  }

  pub fn execute(&mut self) -> Result<()> {
    // Using DFS to traverse and execute the AST
    let body = match self.ast.clone() {
      Node::Program { body } => {
        self.execution_stack.push(Scope::Program);

        body
      }
      _ => panic!("Program node expected at the root of AST"),
    };

    for node in body {
      self.execute_node(node.clone())?;
    }

    // println!("Execution completed.");
    // println!("Global Variables: {:?}", self.global_vars);
    // println!("Local Variables: {:?}", self.local_vars);

    Ok(())
  }

  fn execute_node(&mut self, node: Node) -> Result<Option<Reference>> {
    // This function executes a single node and returns an optional Reference for nodes that produce a value (like function calls)
    // println!("Executing node: {}", node); // Debug print to trace execution

    match node {
      Node::VariableDeclaration { name, value: expr } => {
        // println!("Declaring variable: {} with initial value: {}", name, expr);

        let evaluated_value = self.evaluate_expression(expr)?;

        match self.execution_stack.last() {
          Some(Scope::Program) => self.global_vars.insert(name, evaluated_value),
          _ => self.local_vars.insert(name, evaluated_value),
        };
      }
      Node::ArrayElementAssignment {
        array_name,
        index,
        value,
      } => {
        // println!("Assigning value to array element: {}[{:?}] = {}", array_name, index, value);

        let evaluated_value = self.evaluate_expression(value)?;

        // Look up the array variable
        let array_ref = match self.local_vars.get(&array_name) {
          Some(var) => var.clone(),
          None => match self.global_vars.get(&array_name) {
            Some(var) => var.clone(),
            None => {
              return Err(EdxRuntimeError {
                message: format!("Undefined variable: {}", array_name),
                help: "Make sure the variable is declared before it is used.".into(),
              })?;
            }
          },
        };

        // Ensure it's an array variable
        let mut array = match array_ref {
          Reference::ArrayVar(arr) => arr,
          _ => return Err(EdxRuntimeError {
            message: format!("Variable '{}' is not an array", array_name),
            help: "Make sure you are using the correct variable name and that it is declared as an array.".into(),
          })?,
        };

        // Evaluate index expressions
        let mut current_array = &mut array;
        for (i, idx_expr) in index.iter().enumerate() {
          let idx_value = self.evaluate_expression(idx_expr.clone())?;
          let idx_int = match idx_value {
            Reference::IntVar(v) => v,
            _ => {
              return Err(EdxRuntimeError {
                message: format!("Array index must evaluate to an integer, got {}", idx_value),
                help: "Make sure the array index is an integer.".into(),
              })?;
            }
          };

          if idx_int < 0 || (idx_int as usize) >= current_array.len() {
            return Err(EdxRuntimeError {
              message: format!("Array index out of bounds: {}", idx_int),
              help: "Make sure the array index is within the bounds of the array.".into(),
            })?;
          }

          if i == index.len() - 1 {
            // Last index - assign the value
            current_array[idx_int as usize] = evaluated_value.clone();
          } else {
            // Intermediate index - navigate to the next level of the array
            match &current_array[idx_int as usize] {
              Reference::ArrayVar(_) => {
                current_array = match &mut current_array[idx_int as usize] {
                  Reference::ArrayVar(arr) => arr,
                  _ => panic!("Wow, how did we even get here? We just checked this!"),
                };
              }
              _ => {
                return Err(EdxRuntimeError {
                  message: format!(
                    "Expected array at index {} but found {}",
                    idx_int, current_array[idx_int as usize].type_name()
                  ),
                  help: "Make sure all intermediate indices in a multi-dimensional array assignment refer to arrays.".into(),
                })?;
              }
            }
          }
        }

        // Update the variable in the appropriate scope
        match self.execution_stack.last() {
          Some(Scope::Program) => self
            .global_vars
            .insert(array_name, Reference::ArrayVar(array)),
          _ => self
            .local_vars
            .insert(array_name, Reference::ArrayVar(array)),
        };
      }
      Node::SendIO { message, device } => {
        let evaluated_message = self.evaluate_expression(message)?;
        match device.as_str() {
          "DISPLAY" => match evaluated_message {
            Reference::StrVar(s) => println!("{}", s),
            Reference::IntVar(i) => println!("{}", i),
            Reference::RealVar(r) => println!("{}", r),
            Reference::BoolVar(b) => println!("{}", b),
            Reference::ArrayVar(arr) => {
              let arr_str = arr
                .into_iter()
                .map(|elem| match elem {
                  Reference::StrVar(s) => s,
                  Reference::IntVar(i) => i.to_string(),
                  Reference::RealVar(r) => r.to_string(),
                  Reference::BoolVar(b) => b.to_string(),
                  Reference::ArrayVar(_) => "[Nested Array]".to_string(), // Placeholder for nested arrays
                  Reference::Procedure {
                    name, parameters, ..
                  } => format!("{} ({})", name, parameters.join(", ")),
                  Reference::Function {
                    name, parameters, ..
                  } => format!("{} ({})", name, parameters.join(", ")),
                  Reference::ControlContinuation => "ControlContinuation".to_string(),
                  Reference::ControlBreak => "ControlBreak".to_string(),
                })
                .collect::<Vec<String>>()
                .join(", ");
              println!("[{}]", arr_str);
            }
            Reference::Procedure {
              name, parameters, ..
            } => {
              println!("{} ({})", name, parameters.join(", "))
            }
            Reference::Function {
              name, parameters, ..
            } => {
              println!("{} ({})", name, parameters.join(", "))
            }
            Reference::ControlContinuation => println!("ControlContinuation"),
            Reference::ControlBreak => println!("ControlBreak"),
          },
          _ => return Err(EdxRuntimeError {
            message: format!("Unsupported device: {}", device),
            help: "Currently, only the DISPLAY device is supported for output. Make sure you are using 'DISPLAY' as the device in your SEND statement.".into(),
          })?,
        };
      }
      Node::ReceiveIO {
        variable_name,
        input_type,
        device,
      } => match device.as_str() {
        "KEYBOARD" => {
          io::stdout().flush().unwrap(); // Ensure prompt is displayed before input

          let mut input = String::new();
          io::stdin().read_line(&mut input).unwrap();
          let input = input.trim();

          let value = match input_type.as_str() {
            "INTEGER" => {
              let parsed_value = match input.parse::<i64>() {
                Ok(v) => v,
                Err(_) => {
                  return Err(EdxRuntimeError {
                    message: format!("Invalid input for INTEGER type: {}", input),
                    help: "Ensure you are entering a valid integer value.".into(),
                  })?;
                }
              };
              Reference::IntVar(parsed_value)
            }
            "REAL" => {
              let parsed_value = match input.parse::<f64>() {
                Ok(v) => v,
                Err(_) => {
                  return Err(EdxRuntimeError {
                    message: format!("Invalid input for REAL type: {}", input),
                    help: "Ensure you are entering a valid real number.".into(),
                  })?;
                }
              };
              Reference::RealVar(parsed_value)
            }
            "STRING" => Reference::StrVar(input.to_string()),
            _ => return Err(EdxRuntimeError {
              message: format!("Unsupported input type: {}", input_type),
              help: "Supported input types are INTEGER, REAL, and STRING. Make sure you are using one of these types in your RECEIVE statement.".into(),
            })?,
          };

          match self.execution_stack.last() {
            Some(Scope::Program) => self.global_vars.insert(variable_name, value),
            _ => self.local_vars.insert(variable_name, value),
          };
        }
        _ => return Err(EdxRuntimeError {
          message: format!("Unsupported device: {}", device),
          help: "Currently, only the KEYBOARD device is supported for input. Make sure you are using 'KEYBOARD' as the device in your RECEIVE statement.".into(),
        })?,
      },
      Node::IfStatement {
        condition,
        body,
        else_body,
      } => {
        let condition_value = self.evaluate_expression(condition)?;

        match condition_value.try_into_bool() {
          Ok(true) => {
            for stmt in body {
              // Can potentially return early from the if statement if we encounter a return statement in the body
              match self.execute_node(stmt.clone()) {
                Ok(val) => {
                  if val.is_some() {
                    return Ok(val); // Return value from if body to caller
                  }
                }
                Err(err) => return Err(err)?,
              }
            }
          }
          Ok(false) => {
            if let Some(else_body) = else_body {
              for stmt in else_body {
                match self.execute_node(stmt.clone()) {
                  Ok(val) => {
                    if val.is_some() {
                      return Ok(val); // Return value from else body to caller
                    }
                  }
                  Err(err) => return Err(err)?,
                }
              }
            }
          }
          Err(e) => {
            return Err(EdxRuntimeError {
              message: format!("Condition expression did not evaluate to a boolean: {}", e),
              help: "Ensure the condition evaluates to a boolean value (TRUE or FALSE).".into(),
            })?;
          }
        }
      }
      Node::WhileLoop { condition, body } => loop {
        let mut need_to_break = false; // Flag to track if we need to break out of the loop
        let mut need_to_continue = false; // Flag to track if we need to continue to the next iteration of the loop

        let condition_value = self.evaluate_expression(condition.clone())?;

        match condition_value.try_into_bool() {
          Ok(true) => {
            for stmt in body.clone() {
              let stmt_result = self.execute_node(stmt.clone())?;
              match stmt_result {
                Some(val) => {
                  // Check for control flow signals (continue/break) from the loop body execution, otherswise return any value produced by the loop body to the caller
                  match val {
                    Reference::ControlBreak => {
                      need_to_break = true;
                      break; // Break out of the loop entirely
                    }
                    Reference::ControlContinuation => {
                      need_to_continue = true;
                      break; // Skip to the next iteration of the loop
                    }
                    other_val => return Ok(Some(other_val)), // Return value from loop body to caller
                  }
                }
                None => continue,
              }
            }
          }
          Ok(false) => break, // Condition is false, exit the while loop
          Err(e) => {
            return Err(EdxRuntimeError {
              message: format!("Condition expression did not evaluate to a boolean: {}", e),
              help: "Ensure the condition evaluates to a boolean value (TRUE or FALSE).".into(),
            })?;
          }
        }

        if need_to_continue {
          continue; // Continue to the next iteration of the while loop
        }
        if need_to_break {
          break; // Break out of the while loop entirely
        }
      },
      Node::PostConditionalLoop { body, condition } => loop {
        for stmt in body.clone() {
          let stmt_result = self.execute_node(stmt.clone())?;
          match stmt_result {
            Some(val) => {
              return Ok(Some(val)); // Return value from loop body to caller
            }
            None => continue,
          }
        }

        let condition_value = self.evaluate_expression(condition.clone())?;

        match condition_value.try_into_bool() {
          Ok(true) => break,
          Ok(false) => continue,
          Err(e) => {
            return Err(EdxRuntimeError {
              message: format!("Condition expression did not evaluate to a boolean: {}", e),
              help: "Ensure the condition evaluates to a boolean value (TRUE or FALSE).".into(),
            })?;
          }
        }
      },
      Node::CountControlledLoop { count, body } => {
        let count_value = self.evaluate_expression(count)?;

        let iterations = match count_value {
          Reference::IntVar(v) => v,
          Reference::RealVar(v) => v as i64,
          _ => {
            return Err(EdxRuntimeError {
              message: format!(
                "Count expression for count-controlled loop must evaluate to a numeric type, got {}",
                count_value.type_name()
              ),
              help: "Ensure the count expression evaluates to an integer or real value.".into(),
            })?;
          }
        };

        for _ in 0..iterations {
          for stmt in body.clone() {
            let stmt_result = self.execute_node(stmt.clone())?;
            match stmt_result {
              Some(val) => {
                return Ok(Some(val)); // Return value from loop body to caller
              }
              None => continue,
            }
          }
        }
      }
      Node::ForLoop {
        variable_name,
        start,
        end,
        step,
        body,
      } => {
        let start_value = self.evaluate_expression(start)?;
        let end_value = self.evaluate_expression(end)?;
        let step_value = match step {
          Some(expr) => self.evaluate_expression(expr)?,
          None => Reference::IntVar(1), // Default step is 1
        };

        let (start_num, end_num, step_num) = match (start_value, end_value, step_value) {
          (Reference::IntVar(s), Reference::IntVar(e), Reference::IntVar(st)) => (s, e, st),
          (Reference::RealVar(s), Reference::RealVar(e), Reference::RealVar(st)) => {
            (s as i64, e as i64, st as i64)
          }
          _ => {
            return Err(EdxRuntimeError {
              message: "Start, end, and step expressions must evaluate to numeric types".into(),
              help: "Ensure all expressions in the for loop evaluate to integer or real values."
                .into(),
            })?;
          }
        };

        for i in (start_num..=end_num).step_by(step_num as usize) {
          match self.execution_stack.last() {
            Some(Scope::Program) => self
              .global_vars
              .insert(variable_name.clone(), Reference::IntVar(i)),
            _ => self
              .local_vars
              .insert(variable_name.clone(), Reference::IntVar(i)),
          };

          for stmt in body.clone() {
            let stmt_result = self.execute_node(stmt.clone())?;
            match stmt_result {
              Some(val) => {
                return Ok(Some(val)); // Return value from loop body to caller
              }
              None => continue,
            }
          }
        }
      }
      Node::ForEachLoop {
        variable_name,
        iterable,
        body,
      } => {
        let iterable_value = self.evaluate_expression(iterable)?;

        let elements = match iterable_value {
          Reference::ArrayVar(arr) => arr,
          Reference::StrVar(str) => str
            .chars()
            .map(|c| Reference::StrVar(c.to_string()))
            .collect(),
          _ => {
            return Err(EdxRuntimeError {
              message: format!(
                "Iterable expression for foreach loop must evaluate to an array or string, got {}",
                iterable_value.type_name()
              ),
              help: "Ensure the iterable expression evaluates to an array or string.".into(),
            })?;
          }
        };

        for elem in elements {
          match self.execution_stack.last() {
            Some(Scope::Program) => self.global_vars.insert(variable_name.clone(), elem.clone()),
            _ => self.local_vars.insert(variable_name.clone(), elem.clone()),
          };

          for stmt in body.clone() {
            let stmt_result = self.execute_node(stmt.clone())?;
            match stmt_result {
              Some(val) => {
                return Ok(Some(val)); // Return value from loop body to caller
              }
              None => continue,
            }
          }
        }
      }
      Node::ContinueStatement => {
        // In a real implementation, we would need to handle this properly by breaking out of the current loop iteration and moving to the next one.
        // For this simplified version, we can just return a special Reference to signal a continue statement was encountered, and the loop execution logic can check for this and handle it accordingly.
        return Ok(Some(Reference::ControlContinuation));
      }
      Node::BreakStatement => {
        // Similar to continue, we can return a special Reference to signal a break statement was encountered, and the loop execution logic can check for this and handle it accordingly by breaking out of the loop entirely.
        return Ok(Some(Reference::ControlBreak));
      }
      Node::ProcedureDeclaration {
        name,
        parameters,
        body,
      } => {
        let procedure_ref = Reference::Procedure {
          name: name.clone(),
          parameters: parameters.clone(),
          body: body.clone(),
        };

        self.global_vars.insert(name, procedure_ref);
      }
      Node::FunctionDeclaration {
        name,
        parameters,
        body,
      } => {
        let function_ref = Reference::Function {
          name: name.clone(),
          parameters: parameters.clone(),
          body: body.clone(),
        };

        self.global_vars.insert(name, function_ref);
      }
      Node::ProcedureCall { name, args } => {
        let procedure_ref = match self.global_vars.get(&name) {
          Some(Reference::Procedure {
            name,
            parameters,
            body,
          }) => {
            if parameters.len() != args.len() {
              return Err(EdxRuntimeError {
                message: format!(
                  "Argument count mismatch for procedure '{}': expected {}, got {}",
                  name,
                  parameters.len(),
                  args.len()
                ),
                help: "Ensure the number of arguments matches the procedure's parameter count."
                  .into(),
              })?;
            }
            Reference::Procedure {
              name: name.clone(),
              parameters: parameters.clone(),
              body: body.clone(),
            }
          }
          _ => {
            return Err(EdxRuntimeError {
              message: format!("Undefined procedure: {}", name),
              help: "Make sure the procedure is declared before it is called.".into(),
            })?;
          }
        };

        self.execution_stack.push(Scope::Procedure(name.clone()));
        if let Reference::Procedure {
          parameters, body, ..
        } = procedure_ref
        {
          for (param_name, arg_expr) in parameters.iter().zip(args.iter()) {
            let arg_value = self.evaluate_expression(arg_expr.clone())?;
            self.local_vars.insert(param_name.clone(), arg_value);
          }

          for stmt in body {
            self.execute_node(stmt)?;
          }
        }
        self.execution_stack.pop();

        self.local_vars.clear(); // Clear local variables after procedure call
      }
      Node::ReturnStatement { value } => {
        // This node is used to return a value from a function. We need to evaluate the expression and return it to the caller.
        // Once we encounter a return statement, we should jump back to the caller and pass the return value up the call stack.

        let return_value = match value {
          Some(expr) => Some(self.evaluate_expression(expr)?),
          None => None,
        };
        return Ok(return_value); // Return the value to the caller
      }
      n => todo!("Execution for this node type is not implemented yet {}", n),
    }

    Ok(None)
  }

  fn evaluate_expression(&mut self, expr: Expression) -> Result<Reference> {
    match expr {
      Expression::IntConst(token) => {
        if let Token::IntegerNumeric(value) = token {
          Ok(Reference::IntVar(value))
        } else {
          Err(EdxRuntimeError {
            message: "Expected IntegerNumeric token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::RealConst(token) => {
        if let Token::RealNumeric(value) = token {
          Ok(Reference::RealVar(value))
        } else {
          Err(EdxRuntimeError {
            message: "Expected RealNumeric token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::StrConst(token) => {
        if let Token::StringLiteral(value) = token {
          Ok(Reference::StrVar(value))
        } else {
          Err(EdxRuntimeError {
            message: "Expected StringLiteral token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::BoolConst(token) => {
        if let Token::Boolean(value) = token {
          Ok(Reference::BoolVar(value))
        } else {
          Err(EdxRuntimeError {
            message: "Expected Boolean token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::Identifier(token) => {
        if let Token::Identifier(name) = token {
          match self.local_vars.get(&name) {
            Some(var) => Ok(var.clone()),
            None => match self.global_vars.get(&name) {
              Some(var) => Ok(var.clone()),
              None => Err(EdxRuntimeError {
                message: format!("Undefined variable: {}", name),
                help: "Make sure the variable is declared before it is used.".into(),
              })?,
            },
          }
        } else {
          Err(EdxRuntimeError {
            message: "Expected Identifier token".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::ArrayConst(elements) => {
        let evaluated_elements: Vec<Reference> = elements
          .into_iter()
          .map(|e| match self.evaluate_expression(e) {
            Ok(val) => val,
            Err(err) => {
              panic!("Error evaluating array element: {}", err);
            }
          })
          .collect();
        Ok(Reference::ArrayVar(evaluated_elements))
      }
      Expression::FunctionCall { name, args } => {
        // Function calls return a value, so we need to evaluate the function and return its result

        // Look up the function definition
        let function_ref = match self.global_vars.get(&name) {
          Some(Reference::Function {
            name,
            parameters,
            body,
          }) => {
            if parameters.len() != args.len() {
              return Err(EdxRuntimeError {
                message: format!(
                  "Argument count mismatch for function '{}': expected {}, got {}",
                  name,
                  parameters.len(),
                  args.len()
                ),
                help: "Ensure the number of arguments matches the function's parameter count."
                  .into(),
              })?;
            }
            Reference::Function {
              name: name.clone(),
              parameters: parameters.clone(),
              body: body.clone(),
            }
          }
          _ => {
            return Err(EdxRuntimeError {
              message: format!("Undefined function: {}", name),
              help: "Make sure the function is declared before it is called.".into(),
            })?;
          }
        };

        // Execute the function body in a new scope
        self.execution_stack.push(Scope::Function(name.clone()));

        if let Reference::Function {
          parameters, body, ..
        } = function_ref
        {
          for (param_name, arg_expr) in parameters.iter().zip(args.iter()) {
            let arg_value = self.evaluate_expression(arg_expr.clone())?;
            self.local_vars.insert(param_name.clone(), arg_value);
          }

          let mut return_value: Option<Reference> = None;
          for stmt in body {
            let stmt_result = self.execute_node(stmt.clone())?;
            match stmt_result {
              Some(val) => {
                return_value = Some(val); // Capture the return value from the function body execution
                break;
              }
              None => continue,
            }
          }
          self.execution_stack.pop(); // Pop the function scope after execution
          Ok(return_value.unwrap_or(Reference::IntVar(0))) // Default return value if none provided
        } else {
          self.execution_stack.pop(); // Pop the function scope if we fail to execute
          Err(EdxRuntimeError {
            message: "Expected a function reference".into(),
            help: "This error indicates an issue with the internal state of the interpreter. Please report this to the developers.".into(),
          })?
        }
      }
      Expression::BinaryOperation {
        operator,
        left,
        right,
      } => {
        let left_val = self.evaluate_expression(*left)?;
        let right_val = self.evaluate_expression(*right)?;

        match (left_val, right_val, operator) {
          // Handle Array Access
          (Reference::ArrayVar(arr), Reference::IntVar(index), OperatorType::LBRACKET) => {
            if index < 0 || (index as usize) >= arr.len() {
              panic!("Array index out of bounds: {}", index);
            }
            Ok(arr[index as usize].clone())
          }

          // Numeric addition
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::ADD) => {
            Ok(Reference::IntVar(l + r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::ADD) => {
            Ok(Reference::RealVar(l + r))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::ADD) => {
            Ok(Reference::RealVar(l as f64 + r))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::ADD) => {
            Ok(Reference::RealVar(l + r as f64))
          }

          // Numeric subtraction
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::SUB) => {
            Ok(Reference::IntVar(l - r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::SUB) => {
            Ok(Reference::RealVar(l - r))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::SUB) => {
            Ok(Reference::RealVar(l as f64 - r))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::SUB) => {
            Ok(Reference::RealVar(l - r as f64))
          }

          // Numeric Negation (unary)
          (_, Reference::IntVar(r), OperatorType::SUB) => Ok(Reference::IntVar(-r)),
          (_, Reference::RealVar(r), OperatorType::SUB) => Ok(Reference::RealVar(-r)),

          // Numeric multiplication
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::MUL) => {
            Ok(Reference::IntVar(l * r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::MUL) => {
            Ok(Reference::RealVar(l * r))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::MUL) => {
            Ok(Reference::RealVar(l as f64 * r))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::MUL) => {
            Ok(Reference::RealVar(l * r as f64))
          }

          // Numeric exponentiation
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::POW) => {
            Ok(Reference::IntVar(l.pow(r as u32)))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::POW) => {
            Ok(Reference::RealVar(l.powf(r)))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::POW) => {
            Ok(Reference::RealVar((l as f64).powf(r)))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::POW) => {
            Ok(Reference::RealVar(l.powf(r as f64)))
          }

          // Numeric division (always results in RealVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::DIV) => {
            // Division by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::RealVar(l as f64 / r as f64))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            // Division by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::RealVar(l / r))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            // Division by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::RealVar(l as f64 / r))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::DIV) => {
            // Division by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::RealVar(l / r as f64))
          }

          // Numeric Integer division (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            // Division by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::IntVar(l / r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            // Division by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::IntVar((l / r) as i64))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            // Division by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::IntVar((l as f64 / r) as i64))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            // Division by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Division by zero".into(),
                help: "BRO, YOU FREAKIN SRS???!! Bludious woke up and chose violence by trying to divide by zero. Go touch some grass and rethink your life choices.".into(),
              })?;
            }
            Ok(Reference::IntVar((l / r as f64) as i64))
          }

          // Numeric modulus (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::MOD) => {
            // Modulus by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Modulus by zero".into(),
                help:
                  "Nahh bro js pack up! Modulus is also division by zero BTW. Disappointment..."
                    .into(),
              })?;
            }
            Ok(Reference::IntVar(l % r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            // Modulus by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Modulus by zero".into(),
                help:
                  "Nahh bro js pack up! Modulus is also division by zero BTW. Disappointment..."
                    .into(),
              })?;
            }
            Ok(Reference::IntVar((l % r) as i64))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            // Modulus by zero check
            if r == 0.0 {
              Err(EdxRuntimeError {
                message: "Modulus by zero".into(),
                help:
                  "Nahh bro js pack up! Modulus is also division by zero BTW. Disappointment..."
                    .into(),
              })?;
            }
            Ok(Reference::IntVar((l as f64 % r) as i64))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::MOD) => {
            // Modulus by zero check
            if r == 0 {
              Err(EdxRuntimeError {
                message: "Modulus by zero".into(),
                help:
                  "Nahh bro js pack up! Modulus is also division by zero BTW. Disappointment..."
                    .into(),
              })?;
            }
            Ok(Reference::IntVar((l % r as f64) as i64))
          }

          // String concatenation
          (Reference::StrVar(l), r, OperatorType::CAT) => Ok(Reference::StrVar(format!(
            "{}{}",
            l,
            match r {
              Reference::IntVar(v) => v.to_string(),
              Reference::RealVar(v) => v.to_string(),
              Reference::StrVar(v) => v,
              Reference::BoolVar(v) => v.to_string(),
              Reference::ArrayVar(_) => Err(EdxRuntimeError {
                message: "Cannot concatenate array to string".into(),
                help: "Arrays are not supported in string concatenation operations.".into(),
              })?,
              Reference::Procedure {
                name,
                parameters: _,
                body: _,
              } => format!("{} ()", name),
              Reference::Function {
                name,
                parameters: _,
                body: _,
              } => format!("{} ()", name),
              Reference::ControlBreak | Reference::ControlContinuation => {
                panic!("Cannot concatenate control flow constructs to string")
              }
            }
          ))),

          (l, Reference::StrVar(r), OperatorType::CAT) => Ok(Reference::StrVar(format!(
            "{}{}",
            match l {
              Reference::IntVar(v) => v.to_string(),
              Reference::RealVar(v) => v.to_string(),
              Reference::StrVar(v) => v,
              Reference::BoolVar(v) => v.to_string(),
              Reference::ArrayVar(_) => Err(EdxRuntimeError {
                message: "Cannot concatenate array to string".into(),
                help: "Arrays are not supported in string concatenation operations.".into(),
              })?,
              Reference::Procedure {
                name,
                parameters: _,
                body: _,
              } => format!("{} ()", name),
              Reference::Function {
                name,
                parameters: _,
                body: _,
              } => format!("{} ()", name),
              Reference::ControlBreak | Reference::ControlContinuation => {
                panic!("Cannot concatenate control flow constructs to string")
              }
            },
            r
          ))),

          // Boolean AND
          (Reference::BoolVar(l), Reference::BoolVar(r), OperatorType::AND) => {
            Ok(Reference::BoolVar(l && r))
          }
          // Boolean OR
          (Reference::BoolVar(l), Reference::BoolVar(r), OperatorType::OR) => {
            Ok(Reference::BoolVar(l || r))
          }
          // BOOLEAN NOT (unary)
          (_, Reference::BoolVar(r), OperatorType::NOT) => Ok(Reference::BoolVar(!r)),

          // Compare operations
          (Reference::IntVar(l), Reference::IntVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar(l < r)),
            OperatorType::GT => Ok(Reference::BoolVar(l > r)),
            OperatorType::LTE => Ok(Reference::BoolVar(l <= r)),
            OperatorType::GTE => Ok(Reference::BoolVar(l >= r)),
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => Err(EdxRuntimeError {
              message: format!("Unsupported operation for IntVar comparison: {} {} {}", l, op, r),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },
          (Reference::RealVar(l), Reference::RealVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar(l < r)),
            OperatorType::GT => Ok(Reference::BoolVar(l > r)),
            OperatorType::LTE => Ok(Reference::BoolVar(l <= r)),
            OperatorType::GTE => Ok(Reference::BoolVar(l >= r)),
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => Err(EdxRuntimeError {
              message: format!("Unsupported operation for RealVar comparison: {} {} {}", l, op, r),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },
          (Reference::IntVar(l), Reference::RealVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar((l as f64) < r)),
            OperatorType::GT => Ok(Reference::BoolVar((l as f64) > r)),
            OperatorType::LTE => Ok(Reference::BoolVar((l as f64) <= r)),
            OperatorType::GTE => Ok(Reference::BoolVar((l as f64) >= r)),
            OperatorType::EQ => Ok(Reference::BoolVar((l as f64) == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar((l as f64) != r)),
            _ => Err(EdxRuntimeError {
              message: format!("Unsupported operation for IntVar-RealVar comparison: {} {} {}", l, op, r),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },
          (Reference::RealVar(l), Reference::IntVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar(l < (r as f64))),
            OperatorType::GT => Ok(Reference::BoolVar(l > (r as f64))),
            OperatorType::LTE => Ok(Reference::BoolVar(l <= (r as f64))),
            OperatorType::GTE => Ok(Reference::BoolVar(l >= (r as f64))),
            OperatorType::EQ => Ok(Reference::BoolVar(l == (r as f64))),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != (r as f64))),
            _ => Err(EdxRuntimeError {
              message: format!("Unsupported operation for RealVar-IntVar comparison: {} {} {}", l, op, r),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },
          (Reference::StrVar(l), Reference::StrVar(r), op) => match op {
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => Err(EdxRuntimeError {
              message: format!("Unsupported operation for StrVar comparison: {} {} {}", l, op, r),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },
          (Reference::BoolVar(l), Reference::BoolVar(r), op) => match op {
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => Err(EdxRuntimeError {
               message: format!(
              "Unsupported operation for BoolVar comparison: {} {} {}",
              l, op, r
            ),
              help: "Supported comparison operators are <, >, <=, >=, =, and <>. Make sure you are using one of these operators in your comparison.".into(),
            })?,
          },

          (lhs, rhs, op) => {
            Err(EdxRuntimeError {
               message: format!(
              "Unsupported operation or operand types: {} {} {}",
              lhs.type_name(),
              op,
              rhs.type_name()
            ),
              help: "Make sure you are using supported operand types and operators.".into(),
            })?
          }
        }
      }
    }
  }
}
