use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Eof,
    Integer(i32),
    Plus,
    Minus,
    Mul,
    Div,
    Fn,
    Extern,
    Identifier(String),
    Whitespace(String),
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Semicolon,
    Colon,
    Comma,
    DoubleQuotes,
    RightArrow,
    TripleDot,
    Comment(String),
    Other(char),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Eof => f.write_str("<EOF>"),
            Token::Integer(n) => f.write_str(&format!("Integer('{}')", *n)),
            Token::Plus => f.write_str("Plus('+')"),
            Token::Minus => f.write_str("Minus('-')"),
            Token::Mul => f.write_str("Mul('*')"),
            Token::Div => f.write_str("ForwardSlash('/')"),
            Token::Fn => f.write_str("Fn"),
            Token::Extern => f.write_str("Extern"),
            Token::Identifier(id) => f.write_str(&format!("Identifier('{}')", id)),
            Token::Whitespace(_) => f.write_str("WS"),
            Token::LeftParen => f.write_str("LeftParen('(')"),
            Token::RightParen => f.write_str("RightParen(')')"),
            Token::LeftCurly => f.write_str("LeftCurly('{')"),
            Token::RightCurly => f.write_str("RightCurly('}')"),
            Token::Semicolon => f.write_str("Semicolon(';')"),
            Token::Colon => f.write_str("Colon(':')"),
            Token::Comma => f.write_str("Comma(',')"),
            Token::DoubleQuotes => f.write_str("DoubleQuotes('\"')"),
            Token::RightArrow => f.write_str("RightArrrow('->')"),
            Token::TripleDot => f.write_str("TripleDot('...')"),
            Token::Comment(c) => f.write_str(&format!("Comment('{}')", c)),
            Token::Other(c) => f.write_str(&format!("Other('{}')", *c)),
        }
    }
}

impl Token {
    pub fn as_str(&self) -> String {
        match self {
            Token::Eof => "".to_string(),
            Token::Integer(n) => n.to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Mul => "*".to_string(),
            Token::Div => "/".to_string(),
            Token::Fn => "fn".to_string(),
            Token::Extern => "extern".to_string(),
            Token::Identifier(id) => id.clone(),
            Token::Whitespace(c) => c.clone(),
            Token::LeftParen => "(".to_string(),
            Token::RightParen => ")".to_string(),
            Token::LeftCurly => "}".to_string(),
            Token::RightCurly => "}".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Colon => ":".to_string(),
            Token::Comma => ",".to_string(),
            Token::DoubleQuotes => "\"".to_string(),
            Token::RightArrow => "->".to_string(),
            Token::TripleDot => "...".to_string(),
            Token::Comment(c) => c.clone(),
            Token::Other(c) => c.to_string(),
        }
    }

    pub fn get_precedence(&self) -> i32 {
        match self {
            Token::Plus => 20,
            Token::Minus => 20,
            Token::Mul => 30,
            Token::Div => 30,
            _ => -1,
        }
    }
}
