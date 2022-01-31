use std::fmt::{Display, Formatter};

#[derive(Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Eof,
    Number(i32),
    Plus,
    Fn,
    Identifier(String),
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Semicolon,
    RightArrow,
    Comment(String),
    Other(char),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Eof => f.write_str("<EOF>"),
            TokenType::Number(n) => f.write_str(&format!("Number('{}')", *n)),
            TokenType::Plus => f.write_str("Plus('+')"),
            TokenType::Fn => f.write_str("Fn"),
            TokenType::Identifier(id) => f.write_str(&format!("Identifier('{}')", id)),
            TokenType::LeftParen => f.write_str("LeftParen('(')"),
            TokenType::RightParen => f.write_str("RightParen(')')"),
            TokenType::LeftCurly => f.write_str("LeftCurly('{')"),
            TokenType::RightCurly => f.write_str("RightCurly('}')"),
            TokenType::Semicolon => f.write_str("Semicolon(';')"),
            TokenType::RightArrow => f.write_str("RightArrrow('->')"),
            TokenType::Comment(c) => f.write_str(&format!("Comment('{}')", c)),
            TokenType::Other(c) => f.write_str(&format!("Other('{}')", *c)),
        }
    }
}

impl TokenType {
    pub fn number(&self) -> Option<i32> {
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

pub fn get_token_precedence(token: &Token) -> i32 {
    token.token_type.get_precedence()
}