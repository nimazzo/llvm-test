use ansi_term::Color;
use std::fmt::{Display, Formatter};
use thiserror::Error;

#[derive(Error, Debug)]
pub struct ParseError {
    context: String,
    error_type: ParseErrorType,
    row: Option<usize>,
    col: Option<usize>,
}

#[derive(Error, Debug)]
enum ParseErrorType {
    #[error("Missing '{0}' token")]
    MissingToken(String),
    #[error("Not a primary expression: {0}")]
    NoPrimaryExpression(String),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Unable to compile program: {0}")]
    GenericCompilationError(String),
    #[error("Unable to JIT compile function: {0}")]
    JITCompilationError(String),
}

impl ParseError {
    pub fn missing_token(context: String, token: &str) -> Self {
        Self {
            context,
            error_type: ParseErrorType::MissingToken(token.to_string()),
            row: None,
            col: None,
        }
    }

    pub fn no_primary_expression(context: String, token: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::NoPrimaryExpression(token),
            row: None,
            col: None,
        }
    }

    pub fn unexpected_eof(context: String) -> Self {
        Self {
            context,
            error_type: ParseErrorType::UnexpectedEOF,
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
        display.push_str(&Color::Red.paint("ParseError: ").to_string());
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
