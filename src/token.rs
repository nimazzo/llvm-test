use std::fmt::{Display, Formatter};

#[derive(Copy, Clone)]
pub struct Token {
    pub pos: (usize, usize),
    pub token_type: TokenType,
}

impl Token {
    pub fn new(pos: (usize, usize), token_type: TokenType) -> Self {
        Self {
            pos,
            token_type,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    Eof,
    Number(i64),
    Plus,
    Other(char),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Eof => f.write_str("<EOF>"),
            TokenType::Other(c) => f.write_str(&format!("Other({})", *c)),
            TokenType::Number(n) => f.write_str(&format!("Number({})", *n)),
            TokenType::Plus => f.write_str("+"),
        }
    }
}

impl TokenType {
    pub fn number(&self) -> Option<i64> {
        match self {
            TokenType::Number(n) => Some(*n),
            _ => None,
        }
    }

    fn get_precedence(&self) -> i32 {
        match self {
            TokenType::Plus => 20,
            _ => -1,
        }
    }
}

pub fn get_token_precedence(token: Token) -> i32 {
    token.token_type.get_precedence()
}