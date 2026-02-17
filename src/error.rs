use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
#[error("FAAHHH! Syntax error: {message}")]
#[diagnostic(code(edx::syntax_error), help("{help}"))] // You can customize the code and severity as needed
pub struct EdxSyntaxError {
  pub message: String,
  pub help: String,
  #[source_code]
  pub src: NamedSource<String>,
  #[label("This bit here")]
  pub span: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[error("FAAHHH! Parsing error: {message}")]
#[diagnostic(code(edx::parsing_error), help("{help}"))] // You can customize the code and severity as needed
pub struct EdxParsingError {
  pub message: String,
  pub help: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("FAAHHH! Runtime error: {message}")]
#[diagnostic(code(edx::runtime_error), help("{help}"))] // You can customize the code and severity as needed
pub struct EdxRuntimeError {
  pub message: String,
  pub help: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("FAAHHH! StdLib error: {message}")]
#[diagnostic(code(edx::stdlib_error), help("{help}"))] // You can customize the code and severity as needed
pub struct EdxStdLibError {
  pub message: String,
  pub help: String,
}
