use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct Token {
    pub pos: (usize, usize),
    pub idx: (Option<usize>, Option<usize>),
    pub token_type: TokenType,
}

impl Token {
    pub fn new(pos: (usize, usize), idx: (Option<usize>, Option<usize>), token_type: TokenType) -> Self {
        Self {
            pos,
            idx,
            token_type,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Eof,
    Integer(i32),
    Plus,
    Fn,
    Identifier(String),
    Whitespace(String),
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Semicolon,
    DoubleQuotes,
    RightArrow,
    Comment(String),
    Other(char),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Eof => f.write_str("<EOF>"),
            TokenType::Integer(n) => f.write_str(&format!("Integer('{}')", *n)),
            TokenType::Plus => f.write_str("Plus('+')"),
            TokenType::Fn => f.write_str("Fn"),
            TokenType::Identifier(id) => f.write_str(&format!("Identifier('{}')", id)),
            TokenType::Whitespace(_) => f.write_str("WS"),
            TokenType::LeftParen => f.write_str("LeftParen('(')"),
            TokenType::RightParen => f.write_str("RightParen(')')"),
            TokenType::LeftCurly => f.write_str("LeftCurly('{')"),
            TokenType::RightCurly => f.write_str("RightCurly('}')"),
            TokenType::Semicolon => f.write_str("Semicolon(';')"),
            TokenType::DoubleQuotes => f.write_str("DoubleQuotes('\"')"),
            TokenType::RightArrow => f.write_str("RightArrrow('->')"),
            TokenType::Comment(c) => f.write_str(&format!("Comment('{}')", c)),
            TokenType::Other(c) => f.write_str(&format!("Other('{}')", *c)),
        }
    }
}

impl TokenType {
    pub fn as_str(&self) -> String {
        match self {
            TokenType::Eof => "".to_string(),
            TokenType::Integer(n) => n.to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Fn => "fn".to_string(),
            TokenType::Identifier(id) => id.clone(),
            TokenType::Whitespace(c) => c.clone(),
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::LeftCurly => "}".to_string(),
            TokenType::RightCurly => "}".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::DoubleQuotes => "\"".to_string(),
            TokenType::RightArrow => "->".to_string(),
            TokenType::Comment(c) => c.clone(),
            TokenType::Other(c) => c.to_string(),
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