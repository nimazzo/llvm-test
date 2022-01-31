use crate::token::{Token, TokenType};
use crate::Console;

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    idx: usize,
    idx_token_start: usize,
    row: usize,
    col: usize,

    console: Console,
}

impl Lexer {
    pub fn new(source: &str, console: Console) -> Self {
        #[cfg(windows)]
        ansi_term::enable_ansi_support()
            .unwrap_or_else(|_| console.force_println("[Warning] Unable to enable ansi support"));

        let input = source.chars().collect();
        Self {
            input,
            idx: 0,
            idx_token_start: 0,
            row: 1,
            col: 1,
            console,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.idx_token_start = self.idx;
        let token = if !self.has_more_tokens() {
            return Token::new(self.get_token_pos(), TokenType::Eof);
        } else {
            self.identify_token()
        };

        self.console.println_verbose(format!(
            "[Lexer] Parsed token: {} at:({}:{})",
            token.token_type, token.pos.0, token.pos.1
        ));
        token
    }

    pub fn get_token_idx(&self) -> usize {
        self.idx
    }

    pub fn get_context(&self, start: Option<usize>, end: Option<usize>) -> String {
        let start = start.unwrap_or(self.idx_token_start);
        let end = end.unwrap_or(self.idx);

        let mut context = String::new();

        // todo: might want to hide unrelated lines in the beginning
        for c in self.input.iter().take(start) {
            context.push(*c);
        }

        let mut current_token = String::new();
        for c in self
            .input
            .iter()
            .skip(start)
            .take(end - start)
        {
            current_token.push(*c);
        }

        context.push_str(&ansi_term::Color::Red.paint(current_token).to_string());

        for c in self.input.iter().skip(end).take(30) {
            context.push(*c);
        }

        let context = context.lines().map(|line| format!("    {}\n", line)).collect::<String>();
        context
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
                    return Token::new(pos, TokenType::LeftParen);
                }
                ')' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::RightParen);
                }
                '{' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::LeftCurly);
                }
                '}' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::RightCurly);
                }
                ';' => {
                    self.advance_index();
                    return Token::new(pos, TokenType::Semicolon);
                }
                '-' => {
                    if self.peek(1) == Some('>') {
                        self.advance_index();
                        self.advance_index();
                        return Token::new(pos, TokenType::RightArrow);
                    }
                }
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
            };
        }

        Token::new(pos, TokenType::Eof)
    }

    fn parse_number(&mut self) -> i32 {
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
            if c.is_whitespace() || c == '/' && self.peek(1) == Some('/') {
                if c == '/' && self.peek(1) == Some('/') {
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
        let pos = self.get_token_pos();
        let mut comment = String::new();
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c == '\n' || c == '\r' {
                break;
            }
            comment.push(c);
            self.advance_index();
        }
        let token = Token::new(pos, TokenType::Comment(comment));
        self.console.println_verbose(format!(
            "[Lexer] Parsed token: {} at:({}:{})",
            token.token_type, token.pos.0, token.pos.1
        ));
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