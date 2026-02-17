// This file defines the standard library for EDX. It includes basic functions and types that are commonly used.
// It will have a handy binder function to the EDX runtime, which will provide access to the runtime's features and capabilities.

use miette::Result;

use crate::{error::EdxStdLibError, runtime::Reference};

pub struct StdLib;

pub enum StdLibFunction {
  Length(usize), // no. of parameters
  Type(usize),   // no. of parameters
  Random(usize), // no. of parameters
}

impl StdLib {
  pub fn import() -> Self {
    // This function will allow users to import the standard library into their EDX programs.

    StdLib
  }

  pub fn get_function_ref(&self, name: String) -> Option<StdLibFunction> {
    // This function will allow users to get a reference to a standard library function by its name.

    match name.as_str() {
      "LENGTH" => Some(StdLibFunction::Length(1)),
      "TYPE" => Some(StdLibFunction::Type(1)),
      "RANDOM" => Some(StdLibFunction::Random(1)),
      _ => None,
    }
  }

  pub fn get_python_equivalent(
    &self,
    func: &StdLibFunction,
    translated_args: Vec<String>,
  ) -> (String, Option<String>) {
    // This function will return the Python equivalent of a given standard library function.
    // The second element of the tuple is an optional module name that needs to be imported to use the function.

    match func {
      StdLibFunction::Length(_) => (format!("len({})", translated_args[0]), None),
      StdLibFunction::Type(_) => (format!("type({})", translated_args[0]), None),
      StdLibFunction::Random(_) => (
        format!("random.randint(0, {})", translated_args[0]),
        Some("random".to_string()),
      ),
    }
  }

  pub fn call_function(&self, func: StdLibFunction, args: Vec<Reference>) -> Result<Reference> {
    // This function will allow users to call a standard library function with the given arguments.

    match func {
      StdLibFunction::Length(expected_args) => {
        if args.len() != expected_args {
          Err(EdxStdLibError {
            message: format!(
              "LENGTH function expects {} argument(s), but got {}",
              expected_args,
              args.len()
            ),
            help: "Make sure to provide the correct number of arguments to the LENGTH function."
              .to_string(),
          })?;
        }

        self.get_length(&args[0])
      }
      StdLibFunction::Type(expected_args) => {
        if args.len() != expected_args {
          Err(EdxStdLibError {
            message: format!(
              "TYPE function expects {} argument(s), but got {}",
              expected_args,
              args.len()
            ),
            help: "Make sure to provide the correct number of arguments to the TYPE function."
              .to_string(),
          })?;
        }

        self.get_type(&args[0])
      }
      StdLibFunction::Random(expected_args) => {
        if args.len() != expected_args {
          Err(EdxStdLibError {
            message: format!(
              "RANDOM function expects {} argument(s), but got {}",
              expected_args,
              args.len()
            ),
            help: "Make sure to provide the correct number of arguments to the RANDOM function."
              .to_string(),
          })?;
        }

        match &args[0] {
          Reference::IntVar(max) => self.get_random_int(*max),
          _ => Err(EdxStdLibError {
            message: "RANDOM function expects an integer argument".to_string(),
            help: "Make sure to pass an integer as the argument to the RANDOM function."
              .to_string(),
          })?,
        }
      }
    }
  }

  fn get_length(&self, value: &Reference) -> Result<Reference> {
    // This function will implement the LENGTH function, which returns the length of a string or array.

    match value {
      Reference::StrVar(s) => Ok(Reference::IntVar(s.len() as i64)),
      Reference::ArrayVar(arr) => Ok(Reference::IntVar(arr.len() as i64)),
      _ => Err(EdxStdLibError {
        message: "LENGTH function only works on strings and arrays".to_string(),
        help: "Make sure to pass a string or array to the LENGTH function.".to_string(),
      })?,
    }
  }

  fn get_type(&self, value: &Reference) -> Result<Reference> {
    // This function will implement the TYPE function, which returns the type of a value as a string.

    match value.type_name() {
      "IntVar" => Ok(Reference::StrVar("INTEGER".to_string())),
      "StrVar" => Ok(Reference::StrVar("STRING".to_string())),
      "ArrayVar" => Ok(Reference::StrVar("ARRAY".to_string())),
      "BoolVar" => Ok(Reference::StrVar("BOOLEAN".to_string())),
      _ => Ok(Reference::StrVar("UNKNOWN".to_string())),
    }
  }

  fn get_random_int(&self, max: i64) -> Result<Reference> {
    // This function will implement a RANDOM_INT function, which returns a random integer between 0 and the given max value (inclusive).

    let random_int = (rand::random::<u64>() % (max + 1) as u64) as i64;
    Ok(Reference::IntVar(random_int))
  }
}
