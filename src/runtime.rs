use std::{collections::HashMap, io, io::Write};

use crate::{
  parser::{Expression, Node},
  tokenizer::{OperatorType, Token},
};

#[derive(Clone, Debug)]
pub enum Reference {
  RealVar(f64),
  IntVar(i64),
  StrVar(String),
  BoolVar(bool),
  // ArrayVar(Vec<Reference>),
}

pub enum Scope {
  Program,
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
      self.execute_node(node.clone());
    }

    // println!("Execution completed.");
    // println!("Global Variables: {:?}", self.global_vars);
    // println!("Local Variables: {:?}", self.local_vars);

    Ok(())
  }

  fn execute_node(&mut self, node: Node) {
    // Placeholder for node execution logic
    match node {
      Node::VariableDeclaration { name, value: expr } => {
        // println!("Declaring variable: {} with initial value: {}", name, expr);

        let evaluated_value = self.evaluate_expression(expr);

        match self.execution_stack.last() {
          Some(Scope::Program) => self.global_vars.insert(name, evaluated_value),
          _ => self.local_vars.insert(name, evaluated_value),
        };
      }
      Node::SendIO { message, device } => {
        let evaluated_message = self.evaluate_expression(message);
        match device.as_str() {
          "DISPLAY" => match evaluated_message {
            Reference::StrVar(s) => println!("{}", s),
            Reference::IntVar(i) => println!("{}", i),
            Reference::RealVar(r) => println!("{}", r),
            Reference::BoolVar(b) => println!("{}", b),
          },
          _ => println!("Unsupported device: {}", device),
        }
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
                  panic!("Invalid input for INTEGER type: {}", input);
                }
              };
              Reference::IntVar(parsed_value)
            }
            "REAL" => {
              let parsed_value = match input.parse::<f64>() {
                Ok(v) => v,
                Err(_) => {
                  panic!("Invalid input for REAL type: {}", input);
                }
              };
              Reference::RealVar(parsed_value)
            }
            "STRING" => Reference::StrVar(input.to_string()),
            _ => panic!("Unsupported input type: {}", input_type),
          };

          match self.execution_stack.last() {
            Some(Scope::Program) => self.global_vars.insert(variable_name, value),
            _ => self.local_vars.insert(variable_name, value),
          };
        }
        _ => println!("Unsupported device: {}", device),
      },
      n => todo!("Execution for this node type is not implemented yet {}", n),
    }
  }

  fn evaluate_expression(&self, expr: Expression) -> Reference {
    match expr {
      Expression::IntConst(token) => {
        if let Token::IntegerNumeric(value) = token {
          Reference::IntVar(value)
        } else {
          panic!("Expected IntegerNumeric token");
        }
      }
      Expression::RealConst(token) => {
        if let Token::RealNumeric(value) = token {
          Reference::RealVar(value)
        } else {
          panic!("Expected RealNumeric token");
        }
      }
      Expression::StrConst(token) => {
        if let Token::StringLiteral(value) = token {
          Reference::StrVar(value)
        } else {
          panic!("Expected StringLiteral token");
        }
      }
      Expression::BoolConst(token) => {
        if let Token::Boolean(value) = token {
          Reference::BoolVar(value)
        } else {
          panic!("Expected Boolean token");
        }
      }
      Expression::Identifier(token) => {
        if let Token::Identifier(name) = token {
          match self.local_vars.get(&name) {
            Some(var) => var.clone(),
            None => match self.global_vars.get(&name) {
              Some(var) => var.clone(),
              None => panic!("Undefined variable: {}", name),
            },
          }
        } else {
          panic!("Expected Identifier token");
        }
      }
      Expression::BinaryOperation {
        operator,
        left,
        right,
      } => {
        let left_val = self.evaluate_expression(*left);
        let right_val = self.evaluate_expression(*right);

        match (left_val, right_val, operator) {
          // Numeric addition
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::ADD) => {
            Reference::IntVar(l + r)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::ADD) => {
            Reference::RealVar(l + r)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::ADD) => {
            Reference::RealVar(l as f64 + r)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::ADD) => {
            Reference::RealVar(l + r as f64)
          }

          // Numeric subtraction
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::SUB) => {
            Reference::IntVar(l - r)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::SUB) => {
            Reference::RealVar(l - r)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::SUB) => {
            Reference::RealVar(l as f64 - r)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::SUB) => {
            Reference::RealVar(l - r as f64)
          }

          // Numeric Negation (unary)
          (_, Reference::IntVar(r), OperatorType::SUB) => Reference::IntVar(-r),
          (_, Reference::RealVar(r), OperatorType::SUB) => Reference::RealVar(-r),

          // Numeric multiplication
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::MUL) => {
            Reference::IntVar(l * r)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::MUL) => {
            Reference::RealVar(l * r)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::MUL) => {
            Reference::RealVar(l as f64 * r)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::MUL) => {
            Reference::RealVar(l * r as f64)
          }

          // Numeric division (always results in RealVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::DIV) => {
            Reference::RealVar(l as f64 / r as f64)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            Reference::RealVar(l / r)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::DIV) => {
            Reference::RealVar(l as f64 / r)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::DIV) => {
            Reference::RealVar(l / r as f64)
          }

          // Numeric Integer division (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            Reference::IntVar(l / r)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            Reference::IntVar((l / r) as i64)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::IDIV) => {
            Reference::IntVar((l as f64 / r) as i64)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::IDIV) => {
            Reference::IntVar((l / r as f64) as i64)
          }

          // Numeric modulus (always results in IntVar)
          (Reference::IntVar(l), Reference::IntVar(r), OperatorType::MOD) => {
            Reference::IntVar(l % r)
          }
          (Reference::RealVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            Reference::IntVar((l % r) as i64)
          }
          (Reference::IntVar(l), Reference::RealVar(r), OperatorType::MOD) => {
            Reference::IntVar((l as f64 % r) as i64)
          }
          (Reference::RealVar(l), Reference::IntVar(r), OperatorType::MOD) => {
            Reference::IntVar((l % r as f64) as i64)
          }

          // String concatenation
          (Reference::StrVar(l), r, OperatorType::CAT) => Reference::StrVar(format!(
            "{}{}",
            l,
            match r {
              Reference::IntVar(v) => v.to_string(),
              Reference::RealVar(v) => v.to_string(),
              Reference::StrVar(v) => v,
              Reference::BoolVar(v) => v.to_string(),
            }
          )),

          (l, Reference::StrVar(r), OperatorType::CAT) => Reference::StrVar(format!(
            "{}{}",
            match l {
              Reference::IntVar(v) => v.to_string(),
              Reference::RealVar(v) => v.to_string(),
              Reference::StrVar(v) => v,
              Reference::BoolVar(v) => v.to_string(),
            },
            r
          )),

          // Boolean AND
          (Reference::BoolVar(l), Reference::BoolVar(r), OperatorType::AND) => {
            Reference::BoolVar(l && r)
          }
          // Boolean OR
          (Reference::BoolVar(l), Reference::BoolVar(r), OperatorType::OR) => {
            Reference::BoolVar(l || r)
          }
          // BOOLEAN NOT (unary)
          (_, Reference::BoolVar(r), OperatorType::NOT) => Reference::BoolVar(!r),

          // Compare operations
          (Reference::IntVar(l), Reference::IntVar(r), op) => match op {
            OperatorType::LT => Reference::BoolVar(l < r),
            OperatorType::GT => Reference::BoolVar(l > r),
            OperatorType::LTE => Reference::BoolVar(l <= r),
            OperatorType::GTE => Reference::BoolVar(l >= r),
            OperatorType::EQ => Reference::BoolVar(l == r),
            OperatorType::NEQ => Reference::BoolVar(l != r),
            _ => panic!("Unsupported operation for IntVar comparison"),
          },
          (Reference::RealVar(l), Reference::RealVar(r), op) => match op {
            OperatorType::LT => Reference::BoolVar(l < r),
            OperatorType::GT => Reference::BoolVar(l > r),
            OperatorType::LTE => Reference::BoolVar(l <= r),
            OperatorType::GTE => Reference::BoolVar(l >= r),
            OperatorType::EQ => Reference::BoolVar(l == r),
            OperatorType::NEQ => Reference::BoolVar(l != r),
            _ => panic!("Unsupported operation for RealVar comparison"),
          },
          (Reference::StrVar(l), Reference::StrVar(r), op) => match op {
            OperatorType::EQ => Reference::BoolVar(l == r),
            OperatorType::NEQ => Reference::BoolVar(l != r),
            _ => panic!("Unsupported operation for StrVar comparison"),
          },
          (Reference::BoolVar(l), Reference::BoolVar(r), op) => match op {
            OperatorType::EQ => Reference::BoolVar(l == r),
            OperatorType::NEQ => Reference::BoolVar(l != r),
            _ => panic!("Unsupported operation for BoolVar comparison"),
          },

          _ => {
            panic!("Unsupported operation or operand types");
          }
        }
      }
    }
  }
}
