pub mod error;
pub mod parser;
pub mod runtime;
pub mod stdlib;
pub mod tokenizer;
pub mod transpiler;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn to_python(code: &str) -> Result<String, JsValue> {
  console_error_panic_hook::set_once();

  let mut tokenizer = tokenizer::Lexer::new("playground.edx".to_string(), code.to_string());
  tokenizer
    .tokenize()
    .map_err(|e| JsError::new(&e.to_string()))?;

  let mut parser = parser::Parser::new(tokenizer);
  parser.parse().map_err(|e| JsError::new(&e.to_string()))?;

  let mut transpiler = transpiler::PythonTranspiler::new(parser.get_ast());
  let python_equivalent = transpiler
    .transpile()
    .map_err(|e| JsError::new(&e.to_string()))?;

  Ok(python_equivalent)
}
