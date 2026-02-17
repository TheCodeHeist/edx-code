mod error;
mod parser;
mod runtime;
mod stdlib;
mod tokenizer;
mod transpiler;

use clap::Parser as ClapParser;
use miette::Result;

use parser::Parser;
use runtime::Runtime;
use tokenizer::Lexer;
use transpiler::PythonTranspiler;

#[derive(ClapParser, Debug)]
#[command(name = "Pearson Edexcel Pseudocode Interpreter", version, about, long_about = None)]
struct Cli {
  /// The source file to interpret or transpile
  source_file: String,
  /// The output path for transpiled Python code
  #[arg(long, help = "The output path for transpiled Python code")]
  transpile_py: Option<String>,
}

fn main() -> Result<()> {
  // Getting source code from a file in the arguments
  let args = Cli::parse();
  if let Some(output_path) = args.transpile_py {
    // Transpile to Python
    transpile_to_python(&args.source_file, &output_path)?;
  } else {
    // Interpret the source code
    interpreter(&args.source_file)?;
  }

  Ok(())
}

fn transpile_to_python(filename: &str, output_path: &str) -> Result<()> {
  let source_code =
    std::fs::read_to_string(filename).expect("Something went wrong reading the file");

  let mut lexer = Lexer::new(filename.to_string(), source_code);
  lexer.tokenize()?;

  let mut parser = Parser::new(lexer);
  parser.parse()?;

  let mut transpiler = PythonTranspiler::new(parser.get_ast());
  let python_code = transpiler.transpile()?;

  std::fs::write(output_path, python_code).expect("Unable to write file");

  Ok(())
}

fn interpreter(filename: &str) -> Result<()> {
  let source_code =
    std::fs::read_to_string(filename).expect("Something went wrong reading the file");

  let mut lexer = Lexer::new(filename.to_string(), source_code);
  lexer.tokenize()?;

  let mut parser = Parser::new(lexer);
  parser.parse()?;

  println!("{}", parser.get_ast());

  // let mut runtime = Runtime::new(parser.get_ast());
  // runtime.execute()?;

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
