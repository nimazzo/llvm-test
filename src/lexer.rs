use crate::token::{Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    idx: usize,
    idx_token_start: usize,
    row: usize,
    col: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        #[cfg(windows)]
        ansi_term::enable_ansi_support()
            .unwrap_or_else(|_| eprintln!("unable to enable ansi support"));

        let input = source.chars().collect();
        Self {
            input,
            idx: 0,
            idx_token_start: 0,
            row: 1,
            col: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.idx_token_start = self.idx;
        if !self.has_more_tokens() {
            return Token::new(self.get_token_pos(), TokenType::Eof);
        }
        self.identify_token()
    }

    pub fn get_context(&self) -> String {
        let mut context = String::new();

        // todo: might want to hide unrelated lines in the beginning
        for c in self.input.iter().take(self.idx_token_start) {
            context.push(*c);
        }

        let mut current_token = String::new();
        for c in self.input.iter().skip(self.idx_token_start).take(self.idx - self.idx_token_start) {
            current_token.push(*c);
        }

        context.push_str(&ansi_term::Color::Red.paint(current_token).to_string());

        for c in self.input.iter().skip(self.idx).take(30) {
            context.push(*c);
        }

        context
    }

    pub fn tokens(&mut self) -> TokenIterator {
        TokenIterator { lexer: self }
    }

    /*========== Private Functions ==========*/
    fn get_token_pos(&self) -> (usize, usize) {
        // let token_len = self.idx - self.idx_token_start;
        // (self.row, self.col - token_len)
        (self.row, self.col)
    }

    fn identify_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        let pos = self.get_token_pos();
        if self.has_more_tokens() {
            let start = self.get_current_char();
            self.idx_token_start = self.idx;

            match start {
                '(' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::LeftParen)
                },
                ')' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::RightParen)
                },
                '{' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::LeftCurly)
                },
                '}' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::RightCurly)
                },
                _ => (),
            }

            return if start.is_ascii_alphabetic() {
                let identifier = self.parse_identifier();
                match identifier.as_str() {
                    "fn" => Token::new(pos, TokenType::Fn),
                    _ => Token::new(pos, TokenType::Identifier(identifier)),
                }
            } else if start.is_ascii_digit() {
                let number = self.parse_number();
                Token::new(pos, TokenType::Number(number))
            } else if start == '+' {
                self.advance_index();
                Token::new(pos, TokenType::Plus)
            } else {
                self.advance_index();
                Token::new(pos, TokenType::Other(start))
            }
        }

        Token::new(pos, TokenType::Eof)
    }

    fn parse_number(&mut self) -> i64 {
        let mut number = String::new();
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_ascii_digit() {
                number.push(c);
                self.advance_index();
            } else {
                break;
            }
        }
        number.parse().expect("number must be an integer")
    }

    fn parse_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_ascii_alphabetic() {
                identifier.push(c);
                self.advance_index();
            } else {
                break;
            }
        }
        identifier
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_whitespace() || c == '#' {
                if c == '#' {
                    self.skip_comment();
                } else {
                    self.skip_whitespace();
                }
            } else {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_whitespace() {
                if c == '\n' || c == '\r' {
                    self.advance_index();
                    self.next_line();
                } else {
                    self.advance_index();
                }
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c == '\n' || c == '\r' {
                break;
            }
            self.advance_index();
        }
    }

    fn has_more_tokens(&self) -> bool {
        self.idx < self.input.len()
    }

    fn advance_index(&mut self) {
        self.idx += 1;
        self.col += 1;
    }

    fn peek(&self, n: usize) -> Option<char> {
        self.input.get(self.idx + n).copied()
    }

    fn get_current_char(&self) -> char {
        self.input[self.idx]
    }

    fn next_line(&mut self) {
        self.row += 1;
        self.col = 1;
    }
}

/*============ Token Iterator =============*/
pub struct TokenIterator<'a> {
    lexer: &'a mut Lexer,
}

impl Iterator for TokenIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let next_token = self.lexer.next_token();
        match next_token.token_type {
            TokenType::Eof => None,
            _ => Some(next_token),
        }
    }
}
