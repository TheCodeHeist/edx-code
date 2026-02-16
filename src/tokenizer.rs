use miette::{NamedSource, Result};

use crate::error::EdxSyntaxError;

const RESERVED_KEYWORDS: [&str; 26] = [
  "SET",
  "TO",
  "IF",
  "THEN",
  "ELSE",
  "END",
  "WHILE",
  "DO",
  "REPEAT",
  "UNTIL",
  "TIMES",
  "STEP",
  "FOR",
  "FOREACH",
  "CONTINUE",
  "BREAK",
  "FROM",
  "SEND",
  "RECEIVE",
  "READ",
  "WRITE",
  "CALL",
  "PROCEDURE",
  "FUNCTION",
  "BEGIN",
  "RETURN",
];

const RESERVED_DEVICES: [&str; 2] = ["KEYBOARD", "DISPLAY"];

const RESERVED_TYPES: [&str; 5] = ["INTEGER", "REAL", "STRING", "BOOLEAN", "CHARACTER"];

#[derive(Debug, PartialEq, Clone)]
pub enum OperatorType {
  ADD,
  SUB,
  MUL,
  DIV,
  IDIV,
  POW,
  MOD,
  EQ,
  NEQ,
  LT,
  LTE,
  GT,
  GTE,
  AND,
  OR,
  NOT,
  CAT,
  LPAREN,
  RPAREN,
  LBRACKET,
  RBRACKET,
  COMMA,
}

impl std::fmt::Display for OperatorType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let op_str = match self {
      OperatorType::ADD => "+",
      OperatorType::SUB => "-",
      OperatorType::MUL => "*",
      OperatorType::DIV => "/",
      OperatorType::POW => "^",
      OperatorType::IDIV => "DIV",
      OperatorType::MOD => "MOD",
      OperatorType::EQ => "=",
      OperatorType::NEQ => "<>",
      OperatorType::LT => "<",
      OperatorType::LTE => "<=",
      OperatorType::GT => ">",
      OperatorType::GTE => ">=",
      OperatorType::AND => "AND",
      OperatorType::OR => "OR",
      OperatorType::NOT => "NOT",
      OperatorType::CAT => "&",
      OperatorType::LPAREN => "(",
      OperatorType::RPAREN => ")",
      OperatorType::LBRACKET => "[",
      OperatorType::RBRACKET => "]",
      OperatorType::COMMA => ",",
    };
    write!(f, "{}", op_str)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  // Main
  Identifier(String),
  Keyword(String),
  RealNumeric(f64),
  IntegerNumeric(i64),
  Boolean(bool),
  StringLiteral(String),
  Type(String),
  Operator(OperatorType),
  Device(String),

  EOF,
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Token::Identifier(name) => write!(f, "{}", name),
      Token::Keyword(kw) => write!(f, "Keyword({})", kw),
      Token::RealNumeric(value) => write!(f, "{}", value),
      Token::IntegerNumeric(value) => write!(f, "{}", value),
      Token::Boolean(value) => write!(f, "{}", value),
      Token::StringLiteral(lit) => write!(f, "\"{}\"", lit),
      Token::Operator(op) => write!(f, "{}", op),
      Token::Device(dev) => write!(f, "Device({})", dev),
      Token::Type(ty) => write!(f, "Type({})", ty),
      Token::EOF => write!(f, "EOF"),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenizerState {
  Default,
  InNumber,
  InWord,
  InLiteral,
}

pub struct Lexer {
  filename: String,
  source: String,
  position: usize,
  pub(crate) tokens: Vec<Token>,
  state: TokenizerState,
}

impl Lexer {
  pub fn new(filename: String, source: String) -> Self {
    Lexer {
      filename,
      source,
      position: 0,
      tokens: Vec::new(),
      state: TokenizerState::Default,
    }
  }

  pub fn tokenize(&mut self) -> Result<()> {
    while self.position < self.source.len() {
      let current_char = self.source.chars().nth(self.position).unwrap();

      match self.state {
        TokenizerState::Default => {
          if current_char.is_control() {
            self.position += 1; // Skip control characters
          } else if current_char.is_whitespace() {
            self.position += 1; // Skip whitespace
          } else if current_char == '#' {
            // Skip comments
            while self.position < self.source.len()
              && self.source.chars().nth(self.position).unwrap() != '\n'
            {
              self.position += 1;
            }
            self.position += 1; // Skip the newline character
          } else if current_char.is_alphabetic() {
            self.state = TokenizerState::InWord;
          } else if current_char.is_digit(10) {
            self.state = TokenizerState::InNumber;
          } else if current_char == '\'' {
            self.state = TokenizerState::InLiteral;
            self.position += 1; // Skip the opening quote
          } else if "<>".contains(current_char) {
            // Handle two-character operators
            if let Some(next_char) = self.peek_char() {
              let double_char_op = format!("{}{}", current_char, next_char);

              match double_char_op.as_str() {
                "<=" => {
                  self.tokens.push(Token::Operator(OperatorType::LTE));
                  self.position += 2;
                  continue;
                }
                ">=" => {
                  self.tokens.push(Token::Operator(OperatorType::GTE));
                  self.position += 2;
                  continue;
                }
                "==" => {
                  self.tokens.push(Token::Operator(OperatorType::EQ));
                  self.position += 2;
                  continue;
                }
                "<>" => {
                  self.tokens.push(Token::Operator(OperatorType::NEQ));
                  self.position += 2;
                  continue;
                }
                _ => {
                  // Single character operators
                  match current_char {
                    '<' => self.tokens.push(Token::Operator(OperatorType::LT)),
                    '>' => self.tokens.push(Token::Operator(OperatorType::GT)),
                    _ => {
                      Err(EdxSyntaxError {
                        message: format!("Unexpected character '{}'", current_char),
                        help: "Sorry bruv, I gave up...".to_string(),
                        src: NamedSource::new(self.filename.clone(), self.source.clone()),
                        span: (self.position, 1).into(),
                      })?;
                    }
                  };
                  self.position += 1;
                  continue;
                }
              }
            }
          } else if "+-*/=^&".contains(current_char) {
            match current_char {
              '+' => self.tokens.push(Token::Operator(OperatorType::ADD)),
              '-' => self.tokens.push(Token::Operator(OperatorType::SUB)),
              '*' => self.tokens.push(Token::Operator(OperatorType::MUL)),
              '/' => self.tokens.push(Token::Operator(OperatorType::DIV)),
              '=' => self.tokens.push(Token::Operator(OperatorType::EQ)),
              '^' => self.tokens.push(Token::Operator(OperatorType::POW)),
              '&' => self.tokens.push(Token::Operator(OperatorType::CAT)),
              _ => panic!("Unreachable operator case, I don't know how we got here..."),
            };
            self.position += 1;
          } else if current_char == '(' {
            // Similar to the handling of '[', there are two possible interpretations of the '(' character:
            // 1. The start of a parenthesized expression, e.g. (a + b), in which case we will treat it as an operator token and the parser will handle the nested structure
            // 2. The start of a function call, e.g. myFunction(a, b), in which case we will also treat it as an operator token and the parser will handle the function call
            self.tokens.push(Token::Operator(OperatorType::LPAREN));
            self.position += 1;
          } else if current_char == ')' {
            self.tokens.push(Token::Operator(OperatorType::RPAREN));
            self.position += 1;
          } else if current_char == ',' {
            self.tokens.push(Token::Operator(OperatorType::COMMA));
            self.position += 1;
          } else if current_char == '[' {
            // There are to possible interpretations of the '[' character:
            // 1. The start of an array literal, e.g. [1, 2, 3], or [[1, 2], [3, 4]]
            //    In this case, we will parse the entire array literal as a single token, and the parser will handle the nested structure
            // 2. The start of an array access, e.g. myArray[1]
            //    In this case, we will treat the '[' as an operator token, and the parser will handle the array access
            self.tokens.push(Token::Operator(OperatorType::LBRACKET));
            self.position += 1;
          } else if current_char == ']' {
            self.tokens.push(Token::Operator(OperatorType::RBRACKET));
            self.position += 1;
          } else {
            Err(EdxSyntaxError {
              message: format!("Unexpected character '{}'", current_char),
              help: "Sorry bruv, I gave up...".to_string(),
              src: NamedSource::new(self.filename.clone(), self.source.clone()),
              span: (self.position, 1).into(),
            })?;
          }
        }
        TokenizerState::InWord => {
          let start_pos = self.position;
          while self.position < self.source.len()
            && (self
              .source
              .chars()
              .nth(self.position)
              .unwrap()
              .is_alphanumeric()
              || self.source.chars().nth(self.position).unwrap() == '_')
          {
            self.position += 1;
          }
          let word: String = self.source[start_pos..self.position].to_string();
          if RESERVED_KEYWORDS.contains(&word.as_str()) {
            self.tokens.push(Token::Keyword(word));
          } else if RESERVED_DEVICES.contains(&word.as_str()) {
            self.tokens.push(Token::Device(word));
          } else if RESERVED_TYPES.contains(&word.as_str()) {
            self.tokens.push(Token::Type(word));
          } else if ["DIV", "MOD", "AND", "OR", "NOT"].contains(&word.as_str()) {
            let operator = match word.as_str() {
              "DIV" => OperatorType::IDIV,
              "MOD" => OperatorType::MOD,
              "AND" => OperatorType::AND,
              "OR" => OperatorType::OR,
              "NOT" => OperatorType::NOT,
              _ => panic!("Unreachable operator case, I don't know how we got here..."),
            };
            self.tokens.push(Token::Operator(operator));
          } else if word == "TRUE" {
            self.tokens.push(Token::Boolean(true));
          } else if word == "FALSE" {
            self.tokens.push(Token::Boolean(false));
          } else {
            self.tokens.push(Token::Identifier(word));
          }
          self.state = TokenizerState::Default;
        }
        TokenizerState::InNumber => {
          let start_pos = self.position;
          let mut has_decimal_point = false;
          while self.position < self.source.len() {
            let ch = self.source.chars().nth(self.position).unwrap();
            if ch.is_digit(10) {
              self.position += 1;
            } else if ch == '.' && !has_decimal_point {
              has_decimal_point = true;
              self.position += 1;
            } else {
              break;
            }
          }
          let number_str: String = self.source[start_pos..self.position].to_string();
          if has_decimal_point {
            let value: f64 = number_str.parse().unwrap();
            self.tokens.push(Token::RealNumeric(value));
          } else {
            let value: i64 = number_str.parse().unwrap();
            self.tokens.push(Token::IntegerNumeric(value));
          }
          self.state = TokenizerState::Default;
        }
        TokenizerState::InLiteral => {
          let start_pos = self.position;
          while self.position < self.source.len()
            && self.source.chars().nth(self.position).unwrap() != '\''
          {
            // If we reached the a newline before finding a closing quote, it's an error
            if self.source.chars().nth(self.position).unwrap() == '\n' {
              Err(EdxSyntaxError {
                message: "Unterminated string literal".to_string(),
                help: "I guess you forgot the single quote?".to_string(),
                src: NamedSource::new(self.filename.clone(), self.source.clone()),
                span: (start_pos, self.position - start_pos).into(),
              })?;
            }

            self.position += 1;
          }

          if self.position >= self.source.len() {
            Err(EdxSyntaxError {
              message: "Unterminated string literal".to_string(),
              help: "I guess you forgot the single quote?".to_string(),
              src: NamedSource::new(self.filename.clone(), self.source.clone()),
              span: (start_pos, self.position - start_pos).into(),
            })?;
          }

          let literal: String = self.source[start_pos..self.position].to_string();
          self.tokens.push(Token::StringLiteral(literal));
          self.position += 1; // Skip the closing quote
          self.state = TokenizerState::Default;
        }
      }
    }

    self.tokens.reverse();
    Ok(())
  }

  fn peek_char(&self) -> Option<char> {
    if self.position + 1 < self.source.len() {
      Some(self.source.chars().nth(self.position + 1).unwrap())
    } else {
      None
    }
  }

  #[allow(dead_code)]
  fn peek_next_non_whitespace_char(&self) -> Option<char> {
    let mut pos = self.position + 1;
    while pos < self.source.len() {
      let ch = self.source.chars().nth(pos).unwrap();
      if !ch.is_whitespace() {
        return Some(ch);
      }
      pos += 1;
    }
    None
  }

  pub fn next_token(&mut self) -> Token {
    self.tokens.pop().unwrap_or(Token::EOF)
  }

  pub fn peek_token(&self) -> Token {
    self.tokens.last().cloned().unwrap_or(Token::EOF)
  }
}
