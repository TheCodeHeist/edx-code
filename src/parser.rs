use crate::tokenizer::{Lexer, OperatorType, Token};

#[derive(Clone)]
pub enum Expression {
  RealConst(Token),
  IntConst(Token),
  StrConst(Token),
  BoolConst(Token),
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
    }
  }
}

#[derive(Clone)]
pub enum ControlState {
  If(Expression),
  // While(Expression),
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
  },
}

impl std::fmt::Display for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // In a way that pretty prints a tree the AST nodes
    match self {
      Node::Program { body } => {
        writeln!(f, "Program {{")?;
        for node in body {
          writeln!(f, "  {}", node)?;
        }
        writeln!(f, "}}")
      }
      Node::VariableDeclaration { name, value } => {
        write!(f, "VariableDeclaration {{ {} -> {} }}", name, value)
      }
      Node::SendIO { message, device } => {
        write!(f, "SendIO {{ {} -> {} }}", message, device)
      }
      Node::ReceiveIO {
        variable_name,
        input_type,
        device,
      } => {
        write!(
          f,
          "ReceiveIO {{ {} -> {} ({}) }}",
          device, variable_name, input_type
        )
      }
      Node::IfStatement { condition, body } => {
        writeln!(f, "IfStatement {{ {} {{", condition)?;
        for node in body {
          writeln!(f, "    {}", node)?;
        }
        writeln!(f, "  }} }}")
      }
    }
  }
}

pub struct Parser {
  lexer: Lexer,
  control_flow_stack: Vec<ControlState>,
  ast: Node,
  global_scope: Vec<Node>,
  current_scope: Vec<Node>,
}

impl Parser {
  pub fn new(lexer: Lexer) -> Self {
    Parser {
      lexer,
      control_flow_stack: Vec::new(),
      ast: Node::Program { body: Vec::new() },
      global_scope: Vec::new(),
      current_scope: Vec::new(),
    }
  }

  pub fn parse(&mut self) -> Result<(), String> {
    // Plan:
    // by default, we are in the current_scope, which is the global scope
    // when we are moving into an inner scope (like an if statement), we push all the nodes from
    // current_scope into a global_scope, and clear current_scope
    // when we exit the inner scope, we pop the last control flow state, and then we push the body
    // of the inner scope as a node into the global_scope under the appropriate control flow node.
    // finally, at the end of parsing, we set the ast to be a Program node with the global_scope as its body.

    while self.lexer.peek_token() != Token::EOF {
      match self.lexer.peek_token() {
        Token::Keyword(kw) => {
          if kw == "SET" {
            // Variable declaration
            // Expecting: SET <identifier> TO <expression>

            self.lexer.next_token(); // consume 'SET'

            let name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(format!("Expected identifier, found {}", other)),
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Keyword(to_kw) if to_kw == "TO" => {}
              other => return Err(format!("Expected 'TO', found {}", other)),
            } // consume 'TO'

            let value = self.expr_bp(0.0); // <expression>

            self
              .current_scope
              .push(Node::VariableDeclaration { name, value });
          } else if kw == "SEND" {
            // SEND statement
            // Expecting: SEND <expression> TO <DEVICE>

            self.lexer.next_token(); // consume 'SEND'

            let message = self.expr_bp(0.0); // <expression>

            match self.lexer.next_token() {
              Token::Keyword(to_kw) if to_kw == "TO" => {}
              other => return Err(format!("Expected 'TO', found {}", other)),
            } // consume 'TO'

            let device = match self.lexer.next_token() {
              Token::Device(id) => id,
              other => return Err(format!("Expected device, found {}", other)),
            }; // <DEVICE>

            self.current_scope.push(Node::SendIO { message, device });
          } else if kw == "RECEIVE" {
            // RECEIVE statement
            // Expecting: RECEIVE <identifier> FROM (<TYPE>) <DEVICE>

            self.lexer.next_token(); // consume 'RECEIVE'

            let variable_name = match self.lexer.next_token() {
              Token::Identifier(id) => id,
              other => return Err(format!("Expected identifier, found {}", other)),
            }; // <identifier>

            match self.lexer.next_token() {
              Token::Keyword(from_kw) if from_kw == "FROM" => {}
              other => return Err(format!("Expected 'FROM', found {}", other)),
            } // consume 'FROM'

            match self.lexer.next_token() {
              Token::Operator(OperatorType::LPAREN) => {}
              other => return Err(format!("Expected '(', found {}", other)),
            } // consume '('

            let input_type = match self.lexer.next_token() {
              Token::Type(ty) => ty,
              other => return Err(format!("Expected type, found {}", other)),
            }; // consume <TYPE>

            match self.lexer.next_token() {
              Token::Operator(OperatorType::RPAREN) => {}
              other => return Err(format!("Expected ')', found {}", other)),
            } // consume ')'

            let device = match self.lexer.next_token() {
              Token::Device(id) => id,
              other => return Err(format!("Expected device, found {}", other)),
            }; // <DEVICE>

            self.current_scope.push(Node::ReceiveIO {
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
              other => return Err(format!("Expected 'THEN', found {}", other)),
            } // consume 'THEN'

            self.control_flow_stack.push(ControlState::If(condition));
            self.global_scope.append(&mut self.current_scope);
            self.current_scope = Vec::new();
          } else if kw == "END" {
            // End of a control flow statement
            // Expecting: END <kw>

            self.lexer.next_token(); // consume 'END'

            match self.lexer.next_token() {
              Token::Keyword(end_kw) if end_kw == "IF" => {
                // End of IF statement
                if let Some(ControlState::If(condition)) = self.control_flow_stack.pop() {
                  let body = self.current_scope.clone();
                  self.current_scope = Vec::new();

                  self
                    .global_scope
                    .push(Node::IfStatement { condition, body });
                } else {
                  return Err("Mismatched END IF".to_string());
                }
              }
              other => {
                return Err(format!(
                  "Expected control flow keyword after END, found {}",
                  other
                ));
              }
            }
          }
        }
        other => {
          return Err(format!("Unexpected token: {}", other));
        }
      }
    }

    if !self.control_flow_stack.is_empty() {
      return Err("Unclosed control flow statements".to_string());
    } else {
      self.global_scope.append(&mut self.current_scope);
    }

    self.ast = Node::Program {
      body: self.global_scope.clone(),
    };
    Ok(())
  }

  // pub fn pretty_ast(&self) -> String {
  //   format!("{}", self.ast)
  // }

  fn expr_bp(&mut self, min_bp: f64) -> Expression {
    let mut lhs = match self.lexer.next_token() {
      // Atoms
      Token::Identifier(name) => Expression::Identifier(Token::Identifier(name)),
      Token::IntegerNumeric(value) => Expression::IntConst(Token::IntegerNumeric(value)),
      Token::RealNumeric(value) => Expression::RealConst(Token::RealNumeric(value)),
      Token::StringLiteral(value) => Expression::StrConst(Token::StringLiteral(value)),
      Token::Boolean(value) => Expression::BoolConst(Token::Boolean(value)),
      Token::Operator(OperatorType::LPAREN) => {
        let lhs = self.expr_bp(0.0);
        match self.lexer.next_token() {
          Token::Operator(OperatorType::RPAREN) => lhs,
          other => panic!("Expected ')', found {}", other),
        }
      }
      // Prefix operator setup for [] to work as array literal
      Token::Operator(OperatorType::LBRACKET) => {
        let expr = self.expr_bp(0.0);

        match self.lexer.next_token() {
          Token::Operator(OperatorType::RBRACKET) => expr,
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
          let rhs = self.expr_bp(0.0);

          match self.lexer.next_token() {
            Token::Operator(OperatorType::RBRACKET) => Expression::BinaryOperation {
              operator: op,
              left: Box::new(lhs),
              right: Box::new(rhs),
            },
            other => panic!("Expected ']', found {}", other),
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
      OperatorType::COMMA => (0.1, 0.2),
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
      OperatorType::LBRACKET => (7.0, ()),
      _ => return None,
    };

    Some(res)
  }

  pub fn get_ast(&self) -> Node {
    self.ast.clone()
  }
}
