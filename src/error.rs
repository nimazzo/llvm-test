use ansi_term::Color;
use std::fmt::{Display, Formatter};
use thiserror::Error;

#[derive(Error, Debug)]
pub struct ParseError {
    pub context: String,
    pub error_type: ParseErrorType,
    pub location: String,
    pub row: Option<usize>,
    pub col: Option<usize>,
}

#[derive(Error, Debug)]
pub enum ParseErrorType {
    #[error("Missing '{0}' token")]
    MissingToken(String),
    #[error("Not a primary expression: {0}")]
    NoPrimaryExpression(String),
    #[error("Incompatible types: Expected: '{0}' Found: '{1}'")]
    UnexpectedType(String, String),
    #[error("Wrong Argument Count: Expected: '{0}' Found: '{1}'")]
    WrongArgumentCount(usize, usize),
    #[error("Unknown type: '{0}'")]
    UnknownType(String),
    #[error("Illegal Type: {0}")]
    IllegalType(String),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
}

#[derive(Error, Debug)]
pub struct CompileError {
    pub error_type: CompileErrorType,
    pub location: String,
}

#[derive(Error, Debug)]
pub enum CompileErrorType {
    #[error("Unable to compile program: {0}")]
    GenericCompilationError(String),
    #[error("Unable to JIT compile function: {0}")]
    JITCompilationError(String),
    #[error("Unknown variable: {0}")]
    UnknownVariable(String),
    #[error("Unknown Function: {0}")]
    UnknownFunction(String),
    #[error("Runtime Error: {0}")]
    RuntimeError(String),
    #[error("This is not an error, just an internal marker")]
    VoidReturn,
}

impl CompileError {
    pub fn generic_compilation_error(msg: &str, location: String) -> Self {
        Self {
            error_type: CompileErrorType::GenericCompilationError(msg.to_string()),
            location,
        }
    }

    pub fn jit_compilation_error(msg: &str, location: String) -> Self {
        Self {
            error_type: CompileErrorType::JITCompilationError(msg.to_string()),
            location,
        }
    }

    pub fn unknown_variable(msg: &str, location: String) -> Self {
        Self {
            error_type: CompileErrorType::UnknownVariable(msg.to_string()),
            location,
        }
    }

    pub fn unknown_function(msg: &str, location: String) -> Self {
        Self {
            error_type: CompileErrorType::UnknownFunction(msg.to_string()),
            location,
        }
    }

    pub fn runtime_error(msg: &str, location: String) -> Self {
        Self {
            error_type: CompileErrorType::RuntimeError(msg.to_string()),
            location,
        }
    }

    pub fn void_return(location: String) -> Self {
        Self {
            error_type: CompileErrorType::VoidReturn,
            location,
        }
    }
}

impl ParseError {
    pub fn missing_token(context: String, token: &str, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::MissingToken(token.to_string()),
            location,
            row: None,
            col: None,
        }
    }

    pub fn no_primary_expression(context: String, token: String, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::NoPrimaryExpression(token),
            location,
            row: None,
            col: None,
        }
    }

    pub fn unexpected_eof(context: String, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::UnexpectedEOF,
            location,
            row: None,
            col: None,
        }
    }

    pub fn unexpected_type(context: String, t1: &str, t2: &str, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::UnexpectedType(t1.into(), t2.into()),
            location,
            row: None,
            col: None,
        }
    }

    pub fn unknown_type(context: String, t1: &str, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::UnknownType(t1.into()),
            location,
            row: None,
            col: None,
        }
    }

    pub fn illegal_type(context: String, desc: &str, location: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::IllegalType(desc.to_string()),
            location,
            row: None,
            col: None,
        }
    }

    pub fn wrong_argument_count(
        context: String,
        expected: usize,
        found: usize,
        location: String,
    ) -> Self {
        Self {
            context,
            error_type: ParseErrorType::WrongArgumentCount(expected, found),
            location,
            row: None,
            col: None,
        }
    }

    pub fn with_pos(mut self, pos: (usize, usize)) -> Self {
        self.row = Some(pos.0);
        self.col = Some(pos.1);
        self
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new();
        display.push_str(
            &Color::Red
                .paint(format!("ParseError: {} ", self.location))
                .to_string(),
        );
        if let (Some(row), Some(col)) = (self.row, self.col) {
            display.push_str(
                &Color::Red
                    .paint(format!("at:({}:{}) ", row, col))
                    .to_string(),
            );
        }

        display.push_str(&Color::Red.paint(format!("{}", self.error_type)).to_string());
        display.push_str(&format!("\n{}", self.context));

        f.write_str(&display)
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new();
        display.push_str(
            &Color::Red
                .paint(format!("CompileError: {} ", self.location))
                .to_string(),
        );
        display.push_str(&Color::Red.paint(format!("{}", self.error_type)).to_string());
        f.write_str(&display)
    }
}
