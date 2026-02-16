mod error;
mod parser;
mod runtime;
mod tokenizer;

use miette::Result;
use parser::Parser;
use runtime::Runtime;
use tokenizer::Lexer;

fn main() -> Result<()> {
  // Getting source code from a file in the arguments
  let args: Vec<String> = std::env::args().collect();
  if args.len() < 2 {
    eprintln!("Usage: {} <source_file>", args[0]);
    std::process::exit(1);
  }

  let filename = &args[1];
  let source_code =
    std::fs::read_to_string(filename).expect("Something went wrong reading the file");

  // let source_code = String::from("SET x TO 10 IF x THEN WRITE 'Hello' END IF");

  let mut lexer = Lexer::new(filename.to_string(), source_code);
  lexer.tokenize()?;

  // for token in &lexer.tokens {
  //   println!("Token: {}", token);
  // }

  let mut parser = Parser::new(lexer);
  parser.parse()?;

  // println!("{}", parser.pretty_ast());

  let mut runtime = Runtime::new(parser.get_ast());
  runtime.execute()?;

  Ok(())
}

// #[test]
// fn test_binary_expression_parsing() {
//   let input = String::from("a + b * 2 * c + a / 4");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();

//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "(+ (+ a (* (* b 2) c)) (/ a 4))"
//   );
// }

// #[test]
// fn test_prefix_expression_parsing() {
//   let input = String::from("-a + +b * -2");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();
//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "(+ (- 0 a) (* (+ 0 b) (- 0 2)))"
//   );
// }

// #[test]
// fn test_parentheses_expression_parsing() {
//   let input = String::from("-(a + b) * (c + d)");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();
//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "(* (- 0 (+ a b)) (+ c d))"
//   );
// }

// #[test]
// fn test_postfix_expression_parsing() {
//   let input = String::from("a[b + c * d][c]");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();
//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "([ ([ a (+ b (* c d))) c)"
//   );
// }

// #[test]
// fn test_array_expression_parsing() {
//   let input = String::from("[a, b + c, d * e]");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();
//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "(, (, a (+ b c)) (* d e))"
//   );
// }

// #[test]
// fn test_string_literal_parsing() {
//   let input = String::from("\'Hello, \' & \'world! My code is \' & code");
//   let mut lexer = Lexer::new(input);
//   lexer.tokenize().unwrap();
//   assert_eq!(
//     format!("{}", expr_bp(&mut lexer, 0.0)),
//     "(& (& \"Hello, \" \"world! My code is \") code)"
//   );
// }
