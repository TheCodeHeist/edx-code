use std::{
  collections::HashMap,
  io::{self, Write},
};

use crate::{
  parser::{Expression, Node},
  tokenizer::{OperatorType, Token},
};

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

  pub fn try_into_bool(&self) -> Result<bool, String> {
    match self {
      Reference::BoolVar(b) => Ok(*b),
      Reference::IntVar(v) => Ok(*v != 0),
      Reference::RealVar(v) => Ok(*v != 0.0),
      Reference::StrVar(v) => Ok(!v.is_empty()),
      Reference::ArrayVar(v) => Ok(!v.is_empty()),
      _ => Err(format!("Cannot convert {} to boolean", self.type_name())),
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

  pub fn execute(&mut self) -> Result<(), String> {
    // Using DFS to traverse and execute the AST
    let body = match self.ast.clone() {
      Node::Program { body } => {
        self.execution_stack.push(Scope::Program);

        body
      }
      _ => panic!("Program node expected at the root of AST"),
    };

    for node in body {
      match self.execute_node(node.clone()) {
        Ok(_) => {}
        Err(err) => {
          return Err(format!("Error executing node {}: {}", node, err));
        }
      }
    }

    // println!("Execution completed.");
    // println!("Global Variables: {:?}", self.global_vars);
    // println!("Local Variables: {:?}", self.local_vars);

    Ok(())
  }

  fn execute_node(&mut self, node: Node) -> Result<Option<Reference>, String> {
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
            None => return Err(format!("Undefined array variable: {}", array_name)),
          },
        };

        // Ensure it's an array variable
        let mut array = match array_ref {
          Reference::ArrayVar(arr) => arr,
          _ => return Err(format!("Variable '{}' is not an array", array_name)),
        };

        // Evaluate index expressions
        let mut current_array = &mut array;
        for (i, idx_expr) in index.iter().enumerate() {
          let idx_value = self.evaluate_expression(idx_expr.clone())?;
          let idx_int = match idx_value {
            Reference::IntVar(v) => v,
            _ => {
              return Err(format!(
                "Array index must evaluate to an integer, got {}",
                idx_value
              ));
            }
          };

          if idx_int < 0 || (idx_int as usize) >= current_array.len() {
            return Err(format!("Array index out of bounds: {}", idx_int));
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
                return Err(format!(
                  "Expected nested array at index {}, got {}",
                  idx_int,
                  current_array[idx_int as usize].type_name()
                ));
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
          _ => return Err(format!("Unsupported device: {}", device)),
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
                  return Err(format!("Invalid input for INTEGER type: {}", input));
                }
              };
              Reference::IntVar(parsed_value)
            }
            "REAL" => {
              let parsed_value = match input.parse::<f64>() {
                Ok(v) => v,
                Err(_) => {
                  return Err(format!("Invalid input for REAL type: {}", input));
                }
              };
              Reference::RealVar(parsed_value)
            }
            "STRING" => Reference::StrVar(input.to_string()),
            _ => return Err(format!("Unsupported input type: {}", input_type)),
          };

          match self.execution_stack.last() {
            Some(Scope::Program) => self.global_vars.insert(variable_name, value),
            _ => self.local_vars.insert(variable_name, value),
          };
        }
        _ => return Err(format!("Unsupported device: {}", device)),
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
                Err(err) => return Err(format!("Error executing if body: {}", err)),
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
                  Err(err) => return Err(format!("Error executing else body: {}", err)),
                }
              }
            }
          }
          Err(e) => {
            return Err(format!(
              "Condition expression did not evaluate to a boolean: {}",
              e
            ));
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
              match self.execute_node(stmt) {
                Ok(val) => {
                  // Check for control flow signals (continue/break) from the loop body execution, otherswise return any value produced by the loop body to the caller
                  match val {
                    Some(Reference::ControlBreak) => {
                      need_to_break = true;
                      break; // Break out of the loop entirely
                    }
                    Some(Reference::ControlContinuation) => {
                      need_to_continue = true;
                      break; // Skip to the next iteration of the loop
                    }
                    Some(other_val) => return Ok(Some(other_val)), // Return value from loop body to caller
                    None => {} // No value produced, continue execution
                  }
                }
                Err(err) => return Err(format!("Error executing while loop body: {}", err)),
              }
            }
          }
          Ok(false) => break, // Condition is false, exit the while loop
          Err(e) => {
            return Err(format!(
              "Condition expression did not evaluate to a boolean: {}",
              e
            ));
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
          match self.execute_node(stmt) {
            Ok(val) => {
              if val.is_some() {
                return Ok(val); // Return value from loop body to caller
              }
            }
            Err(err) => {
              return Err(format!(
                "Error executing post-conditional loop body: {}",
                err
              ));
            }
          }
        }

        let condition_value = self.evaluate_expression(condition.clone())?;

        match condition_value.try_into_bool() {
          Ok(true) => break,
          Ok(false) => continue,
          Err(e) => {
            return Err(format!(
              "Condition expression did not evaluate to a boolean: {}",
              e
            ));
          }
        }
      },
      Node::CountControlledLoop { count, body } => {
        let count_value = self.evaluate_expression(count)?;

        let iterations = match count_value {
          Reference::IntVar(v) => v,
          Reference::RealVar(v) => v as i64,
          _ => return Err("Count expression did not evaluate to a numeric type".into()),
        };

        for _ in 0..iterations {
          for stmt in body.clone() {
            match self.execute_node(stmt) {
              Ok(val) => {
                if val.is_some() {
                  return Ok(val); // Return value from loop body to caller
                }
              }
              Err(err) => {
                return Err(format!(
                  "Error executing count-controlled loop body: {}",
                  err
                ));
              }
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
            return Err("Start, end, and step expressions must evaluate to numeric types".into());
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
            match self.execute_node(stmt) {
              Ok(val) => {
                if val.is_some() {
                  return Ok(val); // Return value from loop body to caller
                }
              }
              Err(err) => return Err(format!("Error executing for loop body: {}", err)),
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
          _ => return Err("Iterable expression did not evaluate to an array".into()),
        };

        for elem in elements {
          match self.execution_stack.last() {
            Some(Scope::Program) => self.global_vars.insert(variable_name.clone(), elem.clone()),
            _ => self.local_vars.insert(variable_name.clone(), elem.clone()),
          };

          for stmt in body.clone() {
            match self.execute_node(stmt.clone()) {
              Ok(val) => {
                if val.is_some() {
                  return Ok(val); // Return value from loop body to caller
                }
              }
              Err(err) => return Err(format!("Error executing foreach loop body: {}", err)),
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
              return Err(format!(
                "Argument count mismatch for procedure '{}': expected {}, got {}",
                name,
                parameters.len(),
                args.len()
              ));
            }
            Reference::Procedure {
              name: name.clone(),
              parameters: parameters.clone(),
              body: body.clone(),
            }
          }
          _ => return Err(format!("Undefined procedure: {}", name)),
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

  fn evaluate_expression(&mut self, expr: Expression) -> Result<Reference, String> {
    match expr {
      Expression::IntConst(token) => {
        if let Token::IntegerNumeric(value) = token {
          Ok(Reference::IntVar(value))
        } else {
          Err("Expected IntegerNumeric token".into())
        }
      }
      Expression::RealConst(token) => {
        if let Token::RealNumeric(value) = token {
          Ok(Reference::RealVar(value))
        } else {
          Err("Expected RealNumeric token".into())
        }
      }
      Expression::StrConst(token) => {
        if let Token::StringLiteral(value) = token {
          Ok(Reference::StrVar(value))
        } else {
          Err("Expected StringLiteral token".into())
        }
      }
      Expression::BoolConst(token) => {
        if let Token::Boolean(value) = token {
          Ok(Reference::BoolVar(value))
        } else {
          Err("Expected Boolean token".into())
        }
      }
      Expression::Identifier(token) => {
        if let Token::Identifier(name) = token {
          match self.local_vars.get(&name) {
            Some(var) => Ok(var.clone()),
            None => match self.global_vars.get(&name) {
              Some(var) => Ok(var.clone()),
              None => Err(format!("Undefined variable: {}", name)),
            },
          }
        } else {
          Err("Expected Identifier token".into())
        }
      }
      Expression::ArrayConst(elements) => {
        let evaluated_elements: Vec<Reference> = elements
          .into_iter()
          .map(|e| match self.evaluate_expression(e) {
            Ok(val) => val,
            Err(err) => panic!("Error evaluating array element: {}", err),
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
              return Err(format!(
                "Argument count mismatch for function '{}': expected {}, got {}",
                name,
                parameters.len(),
                args.len()
              ));
            }
            Reference::Function {
              name: name.clone(),
              parameters: parameters.clone(),
              body: body.clone(),
            }
          }
          _ => return Err(format!("Undefined function: {}", name)),
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
            match self.execute_node(stmt.clone()) {
              Ok(val) => {
                // A return value was any part of the function body, we need to capture it and return it up to the caller
                if val.is_some() {
                  return_value = val;
                  break; // Stop executing the function body once we encounter a return statement
                }
              }
              Err(err) => {
                self.execution_stack.pop(); // Ensure we pop the function scope on error
                return Err(format!("Error executing function body: {}", err));
              }
            }
          }
          self.execution_stack.pop(); // Pop the function scope after execution
          Ok(return_value.unwrap_or(Reference::IntVar(0))) // Default return value if none provided
        } else {
          self.execution_stack.pop(); // Pop the function scope if we fail to execute
          Err(format!("Failed to execute function: {}", name))
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
            Ok(Reference::RealVar(l as f64 / r as f64))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            Ok(Reference::RealVar(l / r))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            Ok(Reference::RealVar(l as f64 / r))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::DIV) => {
            Ok(Reference::RealVar(l / r as f64))
          }

          // Numeric Integer division (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            Ok(Reference::IntVar(l / r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            Ok(Reference::IntVar((l / r) as i64))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            Ok(Reference::IntVar((l as f64 / r) as i64))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            Ok(Reference::IntVar((l / r as f64) as i64))
          }

          // Numeric modulus (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::MOD) => {
            Ok(Reference::IntVar(l % r))
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            Ok(Reference::IntVar((l % r) as i64))
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            Ok(Reference::IntVar((l as f64 % r) as i64))
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::MOD) => {
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
              Reference::ArrayVar(_) => panic!("Cannot concatenate array to string"),
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
              Reference::ArrayVar(_) => panic!("Cannot concatenate array to string"),
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
            _ => panic!(
              "Unsupported operation for IntVar comparison: {} {} {}",
              l, op, r
            ),
          },
          (Reference::RealVar(l), Reference::RealVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar(l < r)),
            OperatorType::GT => Ok(Reference::BoolVar(l > r)),
            OperatorType::LTE => Ok(Reference::BoolVar(l <= r)),
            OperatorType::GTE => Ok(Reference::BoolVar(l >= r)),
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => panic!(
              "Unsupported operation for RealVar comparison: {} {} {}",
              l, op, r
            ),
          },
          (Reference::IntVar(l), Reference::RealVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar((l as f64) < r)),
            OperatorType::GT => Ok(Reference::BoolVar((l as f64) > r)),
            OperatorType::LTE => Ok(Reference::BoolVar((l as f64) <= r)),
            OperatorType::GTE => Ok(Reference::BoolVar((l as f64) >= r)),
            OperatorType::EQ => Ok(Reference::BoolVar((l as f64) == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar((l as f64) != r)),
            _ => panic!(
              "Unsupported operation for IntVar-RealVar comparison: {} {} {}",
              l, op, r
            ),
          },
          (Reference::RealVar(l), Reference::IntVar(r), op) => match op {
            OperatorType::LT => Ok(Reference::BoolVar(l < (r as f64))),
            OperatorType::GT => Ok(Reference::BoolVar(l > (r as f64))),
            OperatorType::LTE => Ok(Reference::BoolVar(l <= (r as f64))),
            OperatorType::GTE => Ok(Reference::BoolVar(l >= (r as f64))),
            OperatorType::EQ => Ok(Reference::BoolVar(l == (r as f64))),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != (r as f64))),
            _ => panic!(
              "Unsupported operation for RealVar-IntVar comparison: {} {} {}",
              l, op, r
            ),
          },
          (Reference::StrVar(l), Reference::StrVar(r), op) => match op {
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => panic!(
              "Unsupported operation for StrVar comparison: {} {} {}",
              l, op, r
            ),
          },
          (Reference::BoolVar(l), Reference::BoolVar(r), op) => match op {
            OperatorType::EQ => Ok(Reference::BoolVar(l == r)),
            OperatorType::NEQ => Ok(Reference::BoolVar(l != r)),
            _ => panic!(
              "Unsupported operation for BoolVar comparison: {} {} {}",
              l, op, r
            ),
          },

          (lhs, rhs, op) => {
            panic!(
              "Unsupported operation or operand types: {} {} {}",
              lhs.type_name(),
              op,
              rhs.type_name()
            );
          }
        }
      }
    }
  }
}
