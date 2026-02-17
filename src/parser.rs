use std::collections::HashMap;

use crate::{
  error::EdxParsingError,
  tokenizer::{Lexer, OperatorType, Token},
};

use miette::Result;

#[derive(Clone)]
pub enum Expression {
  RealConst(Token),
  IntConst(Token),
  StrConst(Token),
  BoolConst(Token),
  ArrayConst(Vec<Expression>), // Added ArrayConst to represent array literals, e.g. [1, 2, 3], or [[1, 2], [3, 4]]
  ArrayAccess {
    name: String,
    indices: Vec<Expression>,
  }, // Added ArrayAccess to represent accessing an array element, e.g. myArray[0], or myArray[i + 1]
  FunctionCall {
    name: String,
    args: Vec<Expression>,
  }, // Added FunctionCall to represent function calls, e.g. myFunction(1, "hello")
  Identifier(Token),
  BinaryOperation {
    operator: OperatorType,
    left: Box<Expression>,
    right: Box<Expression>,
  },
}

impl std::fmt::Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Expression::RealConst(token) => write!(f, "{}", token),
      Expression::IntConst(token) => write!(f, "{}", token),
      Expression::StrConst(token) => write!(f, "{}", token),
      Expression::BoolConst(token) => write!(f, "{}", token),
      Expression::Identifier(token) => write!(f, "{}", token),
      Expression::BinaryOperation {
        operator,
        left,
        right,
      } => {
        write!(f, "({} {} {})", operator, left, right)
      }
      Expression::ArrayConst(elements) => {
        let element_strs: Vec<String> = elements.iter().map(|e| format!("{}", e)).collect();
        write!(f, "[{}]", element_strs.join(", "))
      }
      Expression::ArrayAccess { name, indices } => {
        let index_strs: Vec<String> = indices.iter().map(|i| format!("{}", i)).collect();
        write!(f, "{}[{}]", name, index_strs.join(", "))
      }
      Expression::FunctionCall { name, args } => {
        let arg_strs: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
        write!(f, "{}({})", name, arg_strs.join(", "))
      }
    }
  }
}

#[derive(Clone)]
pub enum ControlState {
  #[allow(dead_code)]
  Program(usize),
  If(usize, usize, Expression), // scope_id, parent_scope_id, condition
  Else(usize, usize),           // scope_id, if_scope_id
  While(usize, usize, Expression), // scope_id, parent_scope_id, condition
  PostConditionalLoop(usize, usize), // scope_id, parent_scope_id
  CountControlledLoop(usize, usize, Expression), // scope_id, parent_scope_id, count expression
  ForLoop(
    usize,
    usize,
    String,
    Expression,
    Expression,
    Option<Expression>,
  ), // scope_id, parent_scope_id, variable name, start expression, end expression, optional step expression
  ForEachLoop(usize, usize, String, Expression), // scope_id, parent_scope_id, variable name, iterable expression
  ProcedureDeclaration(usize, String, Vec<String>), // scope_id, procedure name, parameter names
  FunctionDeclaration(usize, String, Vec<String>), // scope_id, function name, parameter names
}

#[derive(Clone)]
pub enum Node {
  Program {
    body: Vec<Node>,
  },
  VariableDeclaration {
    name: String,
    value: Expression,
  },
  ArrayElementAssignment {
    array_name: String,
    indices: Vec<Expression>,
    value: Expression,
  },
  SendIO {
    message: Expression,
    device: String,
  },
  ReceiveIO {
    variable_name: String,
    input_type: String,
    device: String,
  },
  IfStatement {
    condition: Expression,
    body: Vec<Node>,
    else_body: Option<Vec<Node>>,
  },
  WhileLoop {
    condition: Expression,
    body: Vec<Node>,
  },
  PostConditionalLoop {
    body: Vec<Node>,
    condition: Expression,
  },
  CountControlledLoop {
    count: Expression,
    body: Vec<Node>,
  },
  ForLoop {
    variable_name: String,
    start: Expression,
    end: Expression,
    step: Option<Expression>,
    body: Vec<Node>,
  },
  ForEachLoop {
    variable_name: String,
    iterable: Expression,
    body: Vec<Node>,
  },
  ContinueStatement,
  BreakStatement,
  ProcedureCall {
    name: String,
    args: Vec<Expression>,
  },
  ProcedureDeclaration {
    name: String,
    parameters: Vec<String>,
    body: Vec<Node>,
  },
  FunctionDeclaration {
    name: String,
    parameters: Vec<String>,
    body: Vec<Node>,
  },
  ReturnStatement {
    value: Option<Expression>,
  },
}

impl std::fmt::Display for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // In a way that pretty prints the AST with indentation
    match self {
      Node::Program { body } => {
        let mut result = String::from("Program\n");
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::VariableDeclaration { name, value } => {
        write!(f, "VariableDeclaration: {} = {}", name, value)
      }
      Node::ArrayElementAssignment {
        array_name,
        indices,
        value,
      } => {
        let index_strs: Vec<String> = indices.iter().map(|i| format!("{}", i)).collect();
        write!(
          f,
          "ArrayElementAssignment: {}[{}] = {}",
          array_name,
          index_strs.join(", "),
          value
        )
      }
      Node::SendIO { message, device } => {
        write!(f, "SendIO: {} -> {}", message, device)
      }
      Node::ReceiveIO {
        variable_name,
        input_type,
        device,
      } => {
        write!(
          f,
          "ReceiveIO: {} ({}) <- {}",
          variable_name, input_type, device
        )
      }
      Node::IfStatement {
        condition,
        body,
        else_body,
      } => {
        let mut result = format!("IfStatement: IF {}\n", condition);
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        if let Some(else_body) = else_body {
          result.push_str("ELSE\n");
          for node in else_body {
            result.push_str(&format!("  {}\n", node));
          }
        }
        write!(f, "{}", result)
      }
      Node::WhileLoop { condition, body } => {
        let mut result = format!("WhileLoop: WHILE {}\n", condition);
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::PostConditionalLoop { body, condition } => {
        let mut result = format!("PostConditionalLoop: REPEAT UNTIL {}\n", condition);
        for node in body {
          result.push_str(&format!("    {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::CountControlledLoop { count, body } => {
        let mut result = format!("CountControlledLoop: REPEAT {} TIMES\n", count);
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::ForLoop {
        variable_name,
        start,
        end,
        step,
        body,
      } => {
        let mut result = format!("ForLoop: FOR {} FROM {} TO {}", variable_name, start, end);
        if let Some(step) = step {
          result.push_str(&format!(" STEP {}", step));
        }
        result.push_str("\n");
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::ForEachLoop {
        variable_name,
        iterable,
        body,
      } => {
        let mut result = format!("ForEachLoop: FOREACH {} IN {}\n", variable_name, iterable);
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::ContinueStatement => {
        write!(f, "ContinueStatement: CONTINUE")
      }
      Node::BreakStatement => {
        write!(f, "BreakStatement: BREAK")
      }
      Node::ProcedureDeclaration {
        name,
        parameters,
        body,
      } => {
        let mut result = format!(
          "ProcedureDeclaration: PROCEDURE {}({})\n",
          name,
          parameters.join(", ")
        );
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::FunctionDeclaration {
        name,
        parameters,
        body,
      } => {
        let mut result = format!(
          "FunctionDeclaration: FUNCTION {}({})\n",
          name,
          parameters.join(", ")
        );
        for node in body {
          result.push_str(&format!("  {}\n", node));
        }
        write!(f, "{}", result)
      }
      Node::ReturnStatement { value } => {
        write!(
          f,
          "ReturnStatement: RETURN {}",
          value.as_ref().map_or("".to_string(), |v| format!("{}", v))
        )
      }
      Node::ProcedureCall { name, args } => {
        let arg_strs: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
        write!(f, "ProcedureCall: {}({})", name, arg_strs.join(", "))
      }
    }
  }
}

pub struct Parser {
  lexer: Lexer,
  control_flow_stack: Vec<ControlState>,
  ast: Node,
  scopes: HashMap<usize, Vec<Node>>,
  current_scope: usize,
  previous_scope_id: usize,
}

impl Parser {
  pub fn new(lexer: Lexer) -> Self {
    Parser {
      lexer,
      control_flow_stack: Vec::new(),
      ast: Node::Program { body: Vec::new() },
      scopes: HashMap::new(),
      current_scope: 0,
      previous_scope_id: 0,
    }
  }

  fn get_new_scope_id(&mut self) -> usize {
    self.previous_scope_id += 1;
    self.previous_scope_id
  }

  pub fn parse(&mut self) -> Result<()> {
    // Plan:
    // by default, there is a main scope with id 0
    // when entering a control flow statement, push a new scope onto the stack and assign it a new id
    // any nodes parsed go into the scope at the top of the stack
    // when exiting a control flow statement, pop the scope off the stack and attach it to
    // the corresponding control flow node
    self.scopes.insert(0, Vec::new());
    self.control_flow_stack.push(ControlState::Program(0));

    while self.lexer.peek_token() != Token::EOF {
      match self.lexer.peek_token() {
        Token::Keyword(kw) => {
          if kw == "SET" {
            // Variable declaration or array element assignment
            // Expecting variable declaration: SET <identifier> TO <expression>
            // Expecting array element assignment: SET <identifier> [ <expression> (, <expression>)* ] TO <expression>

            self.lexer.next_token(); // consume 'SET'

            let name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => {
                Err(EdxParsingError {
                  message: format!("Expected identifier after SET, found {}", other),
                  help: "The SET statement is used to declare a variable or assign a value to an array element. Make sure you provide a valid identifier after SET.".to_string(),
                })?
              }
            }; // <identifier>

            let mut indices = Vec::new();
            if self.lexer.peek_token() == Token::Operator(OperatorType::LBRACKET) {
              // This is an array element assignment
              self.lexer.next_token(); // consume '['

              loop {
                let index_expr = self.expr_bp(0.0); // <expression>
                indices.push(index_expr);

                if self.lexer.peek_token() == Token::Operator(OperatorType::RBRACKET) {
                  break;
                }

                match self.lexer.next_token() {
                  Token::Operator(OperatorType::COMMA) => {}
                  other => {
                    Err(EdxParsingError {
                      message: format!("Expected ',' or ']' in array index, found {}", other),
                      help: "When assigning to an array element, you need to provide the index in square brackets. If you have multiple dimensions, separate the indices with commas.".to_string(),
                    })?;
                  }
                }
              }

              match self.lexer.next_token() {
                Token::Operator(OperatorType::RBRACKET) => {}
                other => {
                  Err(EdxParsingError {
                    message: format!("Expected ']' after array indices, found {}", other),
                    help:
                      "Make sure to close the square brackets after providing the array indices."
                        .to_string(),
                  })?;
                }
              } // consume ']'
            }

            match self.lexer.next_token() {
              Token::Keyword(to_kw) if to_kw == "TO" => {}
              other => {
                Err(EdxParsingError {
                  message: format!("Expected 'TO' after variable name, found {}", other),
                  help: "The SET statement requires the keyword 'TO' after the variable name (and optional array indices) to specify the value being assigned.".to_string(),
                })?;
              }
            } // consume 'TO'

            let value = self.expr_bp(0.0); // <expression>
            if indices.is_empty() {
              self
                .scopes
                .get_mut(&self.current_scope)
                .unwrap()
                .push(Node::VariableDeclaration { name, value });
            } else {
              self.scopes.get_mut(&self.current_scope).unwrap().push(
                Node::ArrayElementAssignment {
                  array_name: name,
                  indices,
                  value,
                },
              );
            }
          } else if kw == "SEND" {
            // SEND statement
            // Expecting: SEND <expression> TO <DEVICE>

            self.lexer.next_token(); // consume 'SEND'

            let message = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(to_kw) if to_kw == "TO" => {}
              other => {
                Err(EdxParsingError {
                  message: format!("Expected 'TO' after SEND message, found {}", other),
                  help: "The SEND statement requires the keyword 'TO' after the message expression to specify the device to send to.".to_string(),
                })?;
              }
            } // consume 'TO'

            let device = match self.lexer.next_token() {
              Token::Device(id) => id,
              other => 
                Err(EdxParsingError {
                message: format!("Expected device after 'TO' in SEND statement, found {}", other),
                help: "After specifying the message to send and the keyword 'TO', you need to provide a valid device identifier.".to_string(),
              })?,
            }; // <DEVICE>

            self
              .scopes
              .get_mut(&self.current_scope)
              .unwrap()
              .push(Node::SendIO { message, device });
          } else if kw == "RECEIVE" {
            // RECEIVE statement
            // Expecting: RECEIVE <identifier> FROM (<TYPE>) <DEVICE>

            self.lexer.next_token(); // consume 'RECEIVE'

            let variable_name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => Err(EdxParsingError {
                message: format!("Expected identifier after RECEIVE, found {}", other),
                help: "The RECEIVE statement requires an identifier after the keyword 'RECEIVE' to specify the variable that will store the received value.".to_string(),
              })?,
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Keyword(from_kw) if from_kw == "FROM" => {}
              other => Err(EdxParsingError {
                message: format!("Expected 'FROM' after variable name in RECEIVE statement, found {}", other),
                help: "After specifying the variable name in a RECEIVE statement, you need to use the keyword 'FROM' to indicate where the input is coming from.".to_string(),
              })?,
            } // consume 'FROM'

            match self.lexer.next_token() {
              Token::Operator(OperatorType::LPAREN) => {}
              other => Err(EdxParsingError {
                message: format!("Expected '(' after 'FROM' in RECEIVE statement, found {}", other),
                help: "The RECEIVE statement requires the input type to be specified in parentheses after the 'FROM' keyword. Make sure to include the type in parentheses.".to_string(),
              })?,
            } // consume '('

            let input_type = match self.lexer.next_token() {
              Token::Type(ty) => ty,
              other => Err(EdxParsingError {
                message: format!("Expected type after '(' in RECEIVE statement, found {}", other),
                help: "Inside the parentheses after 'FROM' in a RECEIVE statement, you need to specify the type of input expected (e.g., INTEGER, STRING).".to_string(),
              })?,
            }; // consume <TYPE>

            match self.lexer.next_token() {
              Token::Operator(OperatorType::RPAREN) => {}
              other => Err(EdxParsingError {
                message: format!("Expected ')', found {}", other),
                help: "The closing parenthesis for the input type in a RECEIVE statement is missing.".to_string(),
              })?,
            } // consume ')'

            let device = match self.lexer.next_token() {
              Token::Device(id) => id,
              other => Err(EdxParsingError {
                message: format!("Expected device after input type in RECEIVE statement, found {}", other),
                help: "After specifying the input type in a RECEIVE statement, you must specify the device from which to receive the input.".to_string(),
              })?,
            }; // <DEVICE>

            self
              .scopes
              .get_mut(&self.current_scope)
              .unwrap()
              .push(Node::ReceiveIO {
                variable_name,
                input_type,
                device,
              });
          } else if kw == "IF" {
            // If statement
            // Expecting: IF <expression> THEN <parse(body)>

            self.lexer.next_token(); // consume 'IF'

            let condition = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(then_kw) if then_kw == "THEN" => {}
              other => Err(EdxParsingError {
                message: format!("Expected 'THEN' after IF condition, found {}", other),
                help: "The IF statement requires the keyword 'THEN' after the condition expression to indicate the start of the IF body.".to_string(),
              })?,
            } // consume 'THEN'

            // Assign a new scope for the IF body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self.control_flow_stack.push(ControlState::If(
              new_scope_id,
              self.current_scope,
              condition,
            ));
            self.current_scope = new_scope_id;
          } else if kw == "ELSE" {
            // Else statement
            // Expecting: ELSE <parse(body)>

            self.lexer.next_token(); // consume 'ELSE'

            match self.control_flow_stack.last().cloned() {
              Some(ControlState::If(if_scope_id, _, _)) => {
                // Assign a new scope for the ELSE body
                let new_scope_id = self.get_new_scope_id();
                self.scopes.insert(new_scope_id, Vec::new());
                self.current_scope = new_scope_id;

                self
                  .control_flow_stack
                  .push(ControlState::Else(new_scope_id, if_scope_id));
              }
              _ => {
                return Err(EdxParsingError {
                  message: "Mismatched ELSE without IF".to_string(),
                  help: "An ELSE statement must be directly preceded by an IF statement.".to_string(),
                })?;
              }
            };
          } else if kw == "WHILE" {
            // While loop
            // Expecting: WHILE <expression> DO <parse(body)>

            self.lexer.next_token(); // consume 'WHILE'

            let condition = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(do_kw) if do_kw == "DO" => {}
              other => Err(EdxParsingError {
                message: format!("Expected 'DO' after WHILE condition, found {}", other),
                help: "The WHILE statement requires the keyword 'DO' after the condition expression to indicate the start of the loop body.".to_string(),
              })?,
            } // consume 'DO'

            // Assign a new scope for the WHILE body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self.control_flow_stack.push(ControlState::While(
              new_scope_id,
              self.current_scope,
              condition,
            ));
            self.current_scope = new_scope_id;
          } else if kw == "REPEAT" {
            // REPEAT loop
            // Expecting for Post-conditioned loop: REPEAT <parse(body)>
            // Expecting for Count-controlled loop: REPEAT <expression> TIMES <parse(body)>

            self.lexer.next_token(); // consume 'REPEAT'

            let next_token = self.lexer.peek_token();

            if let Token::IntegerNumeric(_) | Token::Identifier(_) | Token::Operator(_) = next_token
            {
              // Count-controlled loop
              let count = self.expr_bp(0.0); // <expression>

              match self.lexer.next_token() {
                Token::Keyword(times_kw) if times_kw == "TIMES" => {}
                other => return Err(EdxParsingError {
                  message: format!("Expected 'TIMES' after count expression in REPEAT statement, found {}", other),
                  help: "When using a count-controlled loop with REPEAT, you need to specify the number of iterations followed by the keyword 'TIMES'.".to_string(),
                })?,
              } // consume 'TIMES'

              // Assign a new scope for the loop body
              let new_scope_id = self.get_new_scope_id();
              self.scopes.insert(new_scope_id, Vec::new());
              self
                .control_flow_stack
                .push(ControlState::CountControlledLoop(
                  new_scope_id,
                  self.current_scope,
                  count,
                ));
              self.current_scope = new_scope_id;
            } else {
              // Post-conditioned loop

              // Assign a new scope for the loop body
              let new_scope_id = self.get_new_scope_id();
              self.scopes.insert(new_scope_id, Vec::new());
              self
                .control_flow_stack
                .push(ControlState::PostConditionalLoop(
                  new_scope_id,
                  self.current_scope,
                ));
              self.current_scope = new_scope_id;
            }
          } else if kw == "UNTIL" {
            // End of a post-conditioned loop
            // Expecting: UNTIL <expression>

            self.lexer.next_token(); // consume 'UNTIL'

            let condition = self.expr_bp(0.0); // <expression>

            match self.control_flow_stack.pop() {
              Some(ControlState::PostConditionalLoop(loop_scope_id, parent_scope_id)) => {
                let body = self.scopes.remove(&loop_scope_id).unwrap();
                self.current_scope = parent_scope_id;

                self
                  .scopes
                  .get_mut(&self.current_scope)
                  .unwrap()
                  .push(Node::PostConditionalLoop { body, condition });
              }
              _ => {
                return Err(EdxParsingError {
                  message: "Mismatched UNTIL without REPEAT".to_string(),
                  help: "An UNTIL statement must be used to close a post-conditioned REPEAT loop.".to_string(),
                })?;
              }
            }
          } else if kw == "FOR" {
            // For loop
            // Expecting without step: FOR <identifier> FROM <expression> TO <expression> DO <parse(body)>
            // Expecting with step: FOR <identifier> FROM <expression> TO <expression> STEP <expression> DO <parse(body)>

            self.lexer.next_token(); // consume 'FOR'

            let variable_name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(EdxParsingError {
                message: format!("Expected identifier after FOR, found {}", other),
                help: "The FOR loop requires a loop variable name immediately after the 'FOR' keyword.".to_string(),
              })?,
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Keyword(from_kw) if from_kw == "FROM" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'FROM', found {}", other),
                help: "The FOR loop requires the keyword 'FROM' after the loop variable name.".to_string(),
              })?,
            } // consume 'FROM'

            let start = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(to_kw) if to_kw == "TO" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'TO', found {}", other),
                help: "The FOR loop requires the keyword 'TO' after the start expression to specify the end expression.".to_string(),
              })?,
            } // consume 'TO'

            let end = self.expr_bp(0.0); // <expression>

            let mut step = None;

            if let Token::Keyword(step_kw) = self.lexer.peek_token() {
              if step_kw == "STEP" {
                self.lexer.next_token(); // consume 'STEP'
                step = Some(self.expr_bp(0.0)); // <expression>
              }
            }

            match self.lexer.next_token() {
              Token::Keyword(do_kw) if do_kw == "DO" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'DO', found {}", other),
                help: "The FOR loop requires the keyword 'DO' after the end (and optional step) expression to indicate the start of the loop body.".to_string(),
              })?,
            } // consume 'DO'

            // Assign a new scope for the loop body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self.control_flow_stack.push(ControlState::ForLoop(
              new_scope_id,
              self.current_scope,
              variable_name,
              start,
              end,
              step,
            ));
            self.current_scope = new_scope_id;
          } else if kw == "FOREACH" {
            // FOREACH loop
            // Expecting: FOREACH <identifier> FROM <expression> DO <parse(body)>

            self.lexer.next_token(); // consume 'FOREACH'

            let variable_name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(EdxParsingError {
                message: format!("Expected identifier after FOREACH, found {}", other),
                help: "The FOREACH loop requires a loop variable name immediately after the 'FOREACH' keyword.".to_string(),
              })?,
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Keyword(from_kw) if from_kw == "FROM" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'FROM', found {}", other),
                help: "The FOREACH loop requires the keyword 'FROM' after the loop variable name.".to_string(),
              })?,
            } // consume 'FROM'

            let iterable = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(do_kw) if do_kw == "DO" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'DO', found {}", other),
                help: "The FOREACH loop requires the keyword 'DO' after the iterable expression to indicate the start of the loop body.".to_string(),
              })?,
            } // consume 'DO'

            // Assign a new scope for the loop body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self.control_flow_stack.push(ControlState::ForEachLoop(
              new_scope_id,
              self.current_scope,
              variable_name,
              iterable,
            ));
            self.current_scope = new_scope_id;
          } else if kw == "CONTINUE" {
            // CONTINUE statement
            // Expecting: CONTINUE

            self.lexer.next_token(); // consume 'CONTINUE'

            self
              .scopes
              .get_mut(&self.current_scope)
              .unwrap()
              .push(Node::ContinueStatement);
          } else if kw == "BREAK" {
            // BREAK statement
            // Expecting: BREAK

            self.lexer.next_token(); // consume 'BREAK'

            self
              .scopes
              .get_mut(&self.current_scope)
              .unwrap()
              .push(Node::BreakStatement);
          } else if kw == "PROCEDURE" {
            // Procedure declaration
            // Expecting: PROCEDURE <identifier> (<parameters>) BEGIN PROCEDURE <parse(body)>

            // FIRST CHECK IF THIS IS CALLED IN THE PROGRAM SCOPE, IF NOT, THE PROCEDURE DECLARATION IS INVALID

            if self.current_scope != 0 {
              Err(EdxParsingError {
                message: "Procedure declarations are only allowed in the main program scope".to_string(),
                help: "Procedure declarations must be at the top level of the program, not nested within other scopes.".to_string(),
              })?;
            }

            self.lexer.next_token(); // consume 'PROCEDURE'

            let name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(EdxParsingError {
                message: format!("Expected identifier, found {}", other),
                help: "After the 'PROCEDURE' keyword, you need to provide a valid identifier for the procedure name.".to_string(),
              })?,
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Operator(OperatorType::LPAREN) => {}
              other => return Err(EdxParsingError {
                message: format!("Expected '(', found {}", other),
                help: "After the procedure name, you must specify the parameter list in parentheses.".to_string(),
              })?,
            } // consume '('

            let mut parameters = Vec::new();

            if self.lexer.peek_token() != Token::Operator(OperatorType::RPAREN) {
              loop {
                let param = match self.lexer.next_token() {
                  Token::Identifier(id) => id,
                  other => return Err(EdxParsingError {
                    message: format!("Expected identifier in parameter list, found {}", other),
                    help: "The parameter list for a procedure declaration must consist of valid identifiers separated by commas.".to_string(),
                  })?,
                }; // <identifier>
                parameters.push(param);

                if self.lexer.peek_token() == Token::Operator(OperatorType::RPAREN) {
                  break;
                }

                match self.lexer.next_token() {
                  Token::Operator(OperatorType::COMMA) => {}
                  other => return Err(EdxParsingError {
                    message: format!("Expected ',' or ')', found {}", other),
                    help: "In the parameter list, parameters must be separated by commas, and the list must be closed with a parenthesis.".to_string(),
                  })?,
                }
              }
            }

            match self.lexer.next_token() {
              Token::Operator(OperatorType::RPAREN) => {}
              other => return Err(EdxParsingError {
                message: format!("Expected ')', found {}", other),
                help: "After listing the parameters for a procedure declaration, make sure to close the parenthesis.".to_string(),
              })?,
            } // consume ')'

            match self.lexer.next_token() {
              Token::Keyword(begin_kw) if begin_kw == "BEGIN" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'BEGIN', found {}", other),
                help: "After the procedure declaration header, you need to use the keyword 'BEGIN' to indicate the start of the procedure body.".to_string(),
              })?,
            } // consume 'BEGIN'

            match self.lexer.next_token() {
              Token::Keyword(proc_kw) if proc_kw == "PROCEDURE" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'PROCEDURE', found {}", other),
                help: "After the 'BEGIN' keyword in a procedure declaration, you must specify the procedure name.".to_string(),
              })?,
            } // consume 'PROCEDURE'

            // Assign a new scope for the procedure body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self
              .control_flow_stack
              .push(ControlState::ProcedureDeclaration(
                new_scope_id,
                name.clone(),
                parameters,
              ));
            self.current_scope = new_scope_id;
          } else if kw == "CALL" {
            // Procedure call
            // Expecting: CALL <expression::function_call>

            self.lexer.next_token(); // consume 'CALL'

            let subprogram_call_expr = self.expr_bp(0.0); // <expression>

            if let Expression::FunctionCall { name, args } = subprogram_call_expr {
              self
                .scopes
                .get_mut(&self.current_scope)
                .unwrap()
                .push(Node::ProcedureCall { name, args });
            } else {
              return Err(EdxParsingError {
                message: format!("Expected procedure call expression after CALL, found {}", subprogram_call_expr),
                help: "After the 'CALL' keyword, you must specify a valid procedure call expression.".to_string(),
              })?;
            }
          } else if kw == "FUNCTION" {
            // Function declaration
            // Expecting: FUNCTION <identifier> (<parameters>) BEGIN FUNCTION <parse(body)>

            // FIRST CHECK IF THIS IS CALLED IN THE PROGRAM SCOPE, IF NOT, THE FUNCTION DECLARATION IS INVALID

            if self.current_scope != 0 {
              return Err(EdxParsingError {
                message: "Function declarations are only allowed in the main program scope".to_string(),
                help: "Function declarations must be at the top level of the program.".to_string(),
              })?;
            }

            self.lexer.next_token(); // consume 'FUNCTION'

            let name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(EdxParsingError {
                message: format!("Expected identifier, found {}", other),
                help: "After the 'FUNCTION' keyword, you must specify a valid function name.".to_string(),
              })?,
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Operator(OperatorType::LPAREN) => {}
              other => return Err(EdxParsingError {
                message: format!("Expected '(', found {}", other),
                help: "After the function name, you must specify the parameter list in parentheses.".to_string(),
              })?,
            } // consume '('

            let mut parameters = Vec::new();

            if self.lexer.peek_token() != Token::Operator(OperatorType::RPAREN) {
              loop {
                let param = match self.lexer.next_token() {
                  Token::Identifier(id) => id,
                  other => return Err(EdxParsingError {
                    message: format!("Expected identifier in parameter list, found {}", other),
                    help: "The parameter list for a function declaration must consist of valid identifiers separated by commas.".to_string(),
                  })?,
                }; // <identifier>
                parameters.push(param);

                if self.lexer.peek_token() == Token::Operator(OperatorType::RPAREN) {
                  break;
                }

                match self.lexer.next_token() {
                  Token::Operator(OperatorType::COMMA) => {}
                  other => return Err(EdxParsingError {
                    message: format!("Expected ',' or ')', found {}", other),
                    help: "In the parameter list, parameters must be separated by commas, and the list must be closed with a parenthesis.".to_string(),
                  })?,
                }
              }
            }

            match self.lexer.next_token() {
              Token::Operator(OperatorType::RPAREN) => {}
              other => return Err(EdxParsingError {
                message: format!("Expected ')', found {}", other),
                help: "The function parameter list must be closed with a right parenthesis.".to_string(),
              })?,
            } // consume ')'

            match self.lexer.next_token() {
              Token::Keyword(begin_kw) if begin_kw == "BEGIN" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'BEGIN', found {}", other),
                help: "After the function declaration header, you need to use the keyword 'BEGIN' to indicate the start of the function body.".to_string(),
              })?,
            } // consume 'BEGIN'

            match self.lexer.next_token() {
              Token::Keyword(func_kw) if func_kw == "FUNCTION" => {}
              other => return Err(EdxParsingError {
                message: format!("Expected 'FUNCTION', found {}", other),
                help: "After the 'BEGIN' keyword, you must specify the function body with a 'FUNCTION' keyword.".to_string(),
              })?,
            } // consume 'FUNCTION'

            // Assign a new scope for the function body
            let new_scope_id = self.get_new_scope_id();
            self.scopes.insert(new_scope_id, Vec::new());
            self
              .control_flow_stack
              .push(ControlState::FunctionDeclaration(
                new_scope_id,
                name.clone(),
                parameters,
              ));
            self.current_scope = new_scope_id;
          } else if kw == "RETURN" {
            // Return statement
            // Expecting: RETURN <expression>

            self.lexer.next_token(); // consume 'RETURN'

            let return_value = self.expr_bp(0.0); // <expression>

            self
              .scopes
              .get_mut(&self.current_scope)
              .unwrap()
              .push(Node::ReturnStatement {
                value: Some(return_value),
              });
          } else if kw == "END" {
            // End of a control flow statement
            // Expecting: END <kw>

            self.lexer.next_token(); // consume 'END'

            match self.lexer.next_token() {
              Token::Keyword(end_kw) => {
                // End of IF statement or IF-ELSE statement
                if end_kw == "IF" {
                  match self.control_flow_stack.pop() {
                    Some(ControlState::If(if_scope_id, parent_scope_id, condition)) => {
                      // IF without ELSE
                      let body = self.scopes.remove(&if_scope_id).unwrap();
                      self.current_scope = parent_scope_id;

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::IfStatement {
                          condition,
                          body,
                          else_body: None,
                        });
                    }
                    Some(ControlState::Else(else_scope_id, if_scope_id)) => {
                      // IF with ELSE
                      let if_body = self.scopes.remove(&if_scope_id).unwrap();
                      let else_body = self.scopes.remove(&else_scope_id).unwrap();

                      let (parent_scope_id, condition) = match self.control_flow_stack.pop() {
                        Some(ControlState::If(_, parent_scope_id, condition)) => {
                          (parent_scope_id, condition)
                        }
                        _ => {
                          return Err(EdxParsingError {
                             message:
                            "Expected control flow stack to have an If state before an Else state"
                              .to_string(),
                            help: "This error indicates an issue with the internal state of the parser. Please report this to the developers.".to_string(),
                            }
                          )?;
                        }
                      };
                      self.current_scope = parent_scope_id;

                      // println!("Parsed IF-ELSE with condition: {}", condition);
                      // println!("Parent scope id: {}", parent_scope_id);

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::IfStatement {
                          condition,
                          body: if_body,
                          else_body: Some(else_body),
                        });
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END IF without IF".to_string(),
                        help: "An END IF statement must be used to close an IF statement.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "WHILE" {
                  // End of WHILE loop

                  match self.control_flow_stack.pop() {
                    Some(ControlState::While(while_scope_id, parent_scope_id, condition)) => {
                      let body = self.scopes.remove(&while_scope_id).unwrap();
                      self.current_scope = parent_scope_id;

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::WhileLoop { condition, body });
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END WHILE without WHILE".to_string(),
                        help: "An END WHILE statement must be used to close a WHILE loop.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "REPEAT" {
                  // End of a REPEAT Count-controlled loop only, since post-conditioned loops are ended with UNTIL

                  match self.control_flow_stack.pop() {
                    Some(ControlState::CountControlledLoop(
                      loop_scope_id,
                      parent_scope_id,
                      count,
                    )) => {
                      let body = self.scopes.remove(&loop_scope_id).unwrap();
                      self.current_scope = parent_scope_id;

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::CountControlledLoop { count, body });
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END REPEAT without Count-controlled REPEAT".to_string(),
                        help: "An END REPEAT statement must be used to close a count-controlled REPEAT loop. Post-conditioned REPEAT loops should be closed with an UNTIL statement.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "FOR" {
                  // End of a FOR loop

                  match self.control_flow_stack.pop() {
                    Some(ControlState::ForLoop(
                      loop_scope_id,
                      parent_scope_id,
                      variable_name,
                      start,
                      end,
                      step,
                    )) => {
                      let body = self.scopes.remove(&loop_scope_id).unwrap();
                      self.current_scope = parent_scope_id;

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::ForLoop {
                          variable_name,
                          start,
                          end,
                          step,
                          body,
                        });
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END FOR without FOR".to_string(),
                        help: "An END FOR statement must be used to close a FOR loop.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "FOREACH" {
                  // End of a FOREACH loop

                  match self.control_flow_stack.pop() {
                    Some(ControlState::ForEachLoop(
                      loop_scope_id,
                      parent_scope_id,
                      variable_name,
                      iterable,
                    )) => {
                      let body = self.scopes.remove(&loop_scope_id).unwrap();
                      self.current_scope = parent_scope_id;

                      self
                        .scopes
                        .get_mut(&self.current_scope)
                        .unwrap()
                        .push(Node::ForEachLoop {
                          variable_name,
                          iterable,
                          body,
                        });
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END FOREACH without FOREACH".to_string(),
                        help: "An END FOREACH statement must be used to close a FOREACH loop.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "PROCEDURE" {
                  // End of a procedure declaration

                  match self.control_flow_stack.pop() {
                    Some(ControlState::ProcedureDeclaration(proc_scope_id, name, parameters)) => {
                      let body = self.scopes.remove(&proc_scope_id).unwrap();
                      self.current_scope = 0; // procedure declarations can only be in the main program scope, so parent scope is always 0

                      self.scopes.get_mut(&self.current_scope).unwrap().push(
                        Node::ProcedureDeclaration {
                          name,
                          parameters,
                          body,
                        },
                      );
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END PROCEDURE without PROCEDURE".to_string(),
                        help: "An END PROCEDURE statement must be used to close a procedure declaration.".to_string(),
                      })?;
                    }
                  }
                } else if end_kw == "FUNCTION" {
                  // End of a function declaration

                  match self.control_flow_stack.pop() {
                    Some(ControlState::FunctionDeclaration(func_scope_id, name, parameters)) => {
                      let body = self.scopes.remove(&func_scope_id).unwrap();
                      self.current_scope = 0; // function declarations can only be in the main program scope, so parent scope is always 0

                      self.scopes.get_mut(&self.current_scope).unwrap().push(
                        Node::FunctionDeclaration {
                          name,
                          parameters,
                          body,
                        },
                      );
                    }
                    _ => {
                      return Err(EdxParsingError {
                        message: "Mismatched END FUNCTION without FUNCTION".to_string(),
                        help: "An END FUNCTION statement must be used to close a function declaration.".to_string(),
                      })?;
                    }
                  }
                }
              }
              other => {
                return Err(EdxParsingError {
                  message: format!("Expected control flow keyword after END, found {}", other),
                  help: "An END statement must be followed by a matching control flow keyword.".to_string(),
                })?;
              }
            }
          }
        }
        other => {
          return Err(EdxParsingError {
            message: format!("Unexpected token: {}", other),
            help: "This error indicates an issue with the internal state of the parser. Please report this to the developers.".to_string(),
          })?;
        }
      }
    }

    if self.control_flow_stack.len() != 1 {
      return Err(EdxParsingError {
        message: "Unclosed control flow statements at end of program".to_string(),
        help: "Make sure all control flow statements (IF, WHILE, REPEAT, FOR, FOREACH, PROCEDURE, FUNCTION) are properly closed with their corresponding END statements (or UNTIL for post-conditioned REPEAT loops).".to_string(),
      })?;
    }

    self.ast = Node::Program {
      body: self.scopes.remove(&0).unwrap(),
    };
    Ok(())
  }

  #[allow(dead_code)]
  pub fn pretty_ast(&self) -> String {
    // Pretty print the AST with indentation
    format!("{}", self.ast)
  }

  fn expr_bp(&mut self, min_bp: f64) -> Expression {
    let mut lhs = match self.lexer.next_token() {
      // Atoms
      Token::Identifier(name) => Expression::Identifier(Token::Identifier(name)),
      Token::IntegerNumeric(value) => Expression::IntConst(Token::IntegerNumeric(value)),
      Token::RealNumeric(value) => Expression::RealConst(Token::RealNumeric(value)),
      Token::StringLiteral(value) => Expression::StrConst(Token::StringLiteral(value)),
      Token::Boolean(value) => Expression::BoolConst(Token::Boolean(value)),
      // Parentheses for grouping
      Token::Operator(OperatorType::LPAREN) => {
        let expr = self.expr_bp(0.0);
        match self.lexer.next_token() {
          Token::Operator(OperatorType::RPAREN) => expr,
          other => panic!("Expected ')', found {}", other),
        }
      }
      // Prefix operator setup for [] to work as array literal, e.g. [1, 2, 3], or [[1, 2], [3, 4]], and as array access, e.g. myArray[1]
      Token::Operator(OperatorType::LBRACKET) => {
        let mut elements = Vec::new();

        if self.lexer.peek_token() != Token::Operator(OperatorType::RBRACKET) {
          loop {
            let element = self.expr_bp(0.0);
            elements.push(element);

            if self.lexer.peek_token() == Token::Operator(OperatorType::RBRACKET) {
              break;
            }

            match self.lexer.next_token() {
              Token::Operator(OperatorType::COMMA) => {}
              other => panic!("Expected ',' or ']', found {}", other),
            }
          }
        }

        match self.lexer.next_token() {
          Token::Operator(OperatorType::RBRACKET) => Expression::ArrayConst(elements),
          other => panic!("Expected ']', found {}", other),
        }
      }
      Token::Operator(op) => {
        let (_, r_bp) = self.prefix_binding_power(&op);
        let rhs = self.expr_bp(r_bp);
        Expression::BinaryOperation {
          operator: op,
          left: Box::new(Expression::IntConst(Token::IntegerNumeric(0))), // Using 0 as left operand for unary
          right: Box::new(rhs),
        }
      }
      other => panic!("Unexpected token: {}", other),
    };

    loop {
      let op = match self.lexer.peek_token() {
        Token::EOF | Token::Keyword(_) => break,
        Token::Operator(op) => op,
        other => panic!("Unexpected token: {}", other),
      };

      if let Some((l_bp, ())) = self.postfix_binding_power(&op) {
        if l_bp < min_bp {
          break;
        }
        self.lexer.next_token();

        lhs = if op == OperatorType::LBRACKET {
          // Array access
          // Format: <expression> [ <expression> (, <expression>)* ]
          let mut indices = Vec::new();

          loop {
            let index = self.expr_bp(0.0);
            indices.push(index);

            if self.lexer.peek_token() == Token::Operator(OperatorType::RBRACKET) {
              break;
            }

            match self.lexer.next_token() {
              Token::Operator(OperatorType::COMMA) => {}
              other => panic!("Expected ',' or ']', found {}", other),
            }
          }

          match self.lexer.next_token() {
            Token::Operator(OperatorType::RBRACKET) => Expression::ArrayAccess {
              name: match lhs {
                Expression::Identifier(Token::Identifier(name)) => name,
                _ => panic!("Expected array name before '[', found {}", lhs),
              },
              indices,
            },
            other => panic!("Expected ']', found {}", other),
          }
        } else if op == OperatorType::LPAREN {
          // Function call
          let mut args = Vec::new();

          if self.lexer.peek_token() != Token::Operator(OperatorType::RPAREN) {
            loop {
              let arg = self.expr_bp(0.0);
              args.push(arg);

              if self.lexer.peek_token() == Token::Operator(OperatorType::RPAREN) {
                break;
              }

              match self.lexer.next_token() {
                Token::Operator(OperatorType::COMMA) => {}
                other => panic!("Expected ',' or ')', found {}", other),
              }
            }
          }

          match self.lexer.next_token() {
            Token::Operator(OperatorType::RPAREN) => Expression::FunctionCall {
              name: match lhs {
                Expression::Identifier(Token::Identifier(name)) => name,
                _ => panic!("Expected function name before '(', found {}", lhs),
              },
              args,
            },
            other => panic!("Expected ')', found {}", other),
          }
        } else {
          Expression::BinaryOperation {
            operator: op,
            left: Box::new(lhs),
            right: Box::new(Expression::IntConst(Token::IntegerNumeric(0))), // Using 0 as left operand for unary
          }
        };
        continue;
      }

      if let Some((l_bp, r_bp)) = self.infix_binding_power(&op) {
        if l_bp < min_bp {
          break;
        }

        self.lexer.next_token(); // consume operator

        let rhs = self.expr_bp(r_bp);

        lhs = Expression::BinaryOperation {
          operator: op,
          left: Box::new(lhs),
          right: Box::new(rhs),
        };
        continue;
      }

      break;
    }

    lhs
  }

  fn infix_binding_power(&self, op: &OperatorType) -> Option<(f64, f64)> {
    Some(match op {
      OperatorType::ADD | OperatorType::SUB | OperatorType::CAT => (1.0, 1.1),
      OperatorType::MUL | OperatorType::DIV => (2.0, 2.1),
      OperatorType::POW => (6.0, 5.0),
      OperatorType::IDIV | OperatorType::MOD => (3.0, 3.1),
      OperatorType::EQ | OperatorType::NEQ => (0.5, 0.6),
      OperatorType::AND | OperatorType::OR => (0.3, 0.4),
      OperatorType::LT | OperatorType::LTE | OperatorType::GT | OperatorType::GTE => (0.7, 0.8),
      // Added comma operator to handle array declarations, and array indexing, with low precedence
      // OperatorType::COMMA => (0.1, 0.2),
      _ => return None,
    })
  }

  fn prefix_binding_power(&self, op: &OperatorType) -> ((), f64) {
    match op {
      OperatorType::ADD | OperatorType::SUB => ((), 4.0),
      OperatorType::NOT => ((), 5.0),
      _ => panic!("Unknown operator: {:?}", op),
    }
  }

  fn postfix_binding_power(&self, op: &OperatorType) -> Option<(f64, ())> {
    let res = match op {
      // Added support for array access with [] as a postfix operator with high precedence
      OperatorType::LBRACKET => (7.0, ()),
      // Added support for function calls with () as a postfix operator with high precedence
      OperatorType::LPAREN => (7.0, ()),
      _ => return None,
    };

    Some(res)
  }

  pub fn get_ast(&self) -> Node {
    self.ast.clone()
  }
}
