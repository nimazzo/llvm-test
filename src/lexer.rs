use crate::token::{Token, TokenType};
use crate::Console;

#[derive(Clone)]
pub struct Lexer {
    input: Vec<char>,
    idx: usize,
    idx_token_start: usize,
    row: usize,
    col: usize,
    current_token_pos: (usize, usize),

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
            current_token_pos: (0, 0),
            console,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.idx_token_start = self.idx;
        self.current_token_pos = (self.row, self.col);

        let token = if !self.has_more_tokens() {
            return Token::new(self.get_token_idx(), TokenType::Eof);
        } else {
            self.identify_token()
        };

        let pos = self.get_token_pos();
        self.console.println_verbose(format!(
            "[Lexer] Parsed token: {} at:({}:{})",
            token.token_type, pos.0, pos.1
        ));
        token
    }

    pub fn get_token_idx(&self) -> (usize, usize) {
        (self.idx_token_start, self.idx)
    }

    pub fn get_context(&self, idx: (Option<usize>, Option<usize>)) -> String {
        // todo: consider rewriting this whole thing
        let start = idx.0.unwrap_or(self.idx_token_start);
        let end = idx.1.unwrap_or(self.idx);

        let mut context = String::new();

        // context before error
        for c in self.input.iter().take(start) {
            context.push(*c);
        }

        // mark error red
        let mut current_token = String::new();
        for c in self.input.iter().skip(start).take(end - start) {
            current_token.push(*c);
        }
        context.push_str(&ansi_term::Color::Red.paint(current_token).to_string());

        // context after error
        for c in self.input.iter().skip(end) {
            context.push(*c);
        }

        let context = context
            .lines()
            .map(|line| format!("    {}\n", line))
            .collect::<String>();
        context
    }

    /*========== Private Functions ==========*/
    pub fn get_token_pos(&self) -> (usize, usize) {
        // let token_len = self.idx - self.idx_token_start;
        // (self.row, self.col - token_len)
        // (self., self.col)
        self.current_token_pos
    }

    fn identify_token(&mut self) -> Token {
        if let Some(token) = self.skip_whitespace_and_comments() {
            return token;
        }

        if self.has_more_tokens() {
            let start = self.get_current_char();
            self.idx_token_start = self.idx;

            match start {
                '"' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::DoubleQuotes);
                }
                '(' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::LeftParen);
                }
                ')' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::RightParen);
                }
                '{' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::LeftCurly);
                }
                '}' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::RightCurly);
                }
                ';' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Semicolon);
                }
                ':' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Colon);
                }
                ',' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Comma);
                }
                '+' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Plus);
                }
                '-' => {
                    return if self.peek(1) == Some('>') {
                        self.advance_index();
                        self.advance_index();
                        Token::new(self.get_token_idx(), TokenType::RightArrow)
                    } else {
                        self.advance_index();
                        Token::new(self.get_token_idx(), TokenType::Minus)
                    }
                }
                '*' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Mul);
                }
                '/' => {
                    self.advance_index();
                    return Token::new(self.get_token_idx(), TokenType::Div);
                }
                _ => (),
            }

            return if start.is_ascii_alphabetic() {
                let identifier = self.parse_identifier();
                match identifier.as_str() {
                    "fn" => Token::new(self.get_token_idx(), TokenType::Fn),
                    _ => Token::new(self.get_token_idx(), TokenType::Identifier(identifier)),
                }
            } else if start.is_ascii_digit() {
                let number = self.parse_number();
                Token::new(self.get_token_idx(), TokenType::Integer(number))
            } else {
                self.advance_index();
                Token::new(self.get_token_idx(), TokenType::Other(start))
            };
        }

        Token::new(self.get_token_idx(), TokenType::Eof)
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

    fn skip_whitespace_and_comments(&mut self) -> Option<Token> {
        if self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_whitespace() || c == '/' && self.peek(1) == Some('/') {
                return if c == '/' && self.peek(1) == Some('/') {
                    Some(self.skip_comment())
                } else {
                    Some(self.skip_whitespace())
                };
            }
        }
        None
    }

    fn skip_whitespace(&mut self) -> Token {
        let mut ws = String::new();
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c.is_whitespace() {
                ws.push(c);
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
        Token::new(self.get_token_idx(), TokenType::Whitespace(ws))
    }

    fn skip_comment(&mut self) -> Token {
        let mut comment = String::new();
        while self.has_more_tokens() {
            let c = self.get_current_char();
            if c == '\n' || c == '\r' {
                break;
            }
            comment.push(c);
            self.advance_index();
        }
        Token::new(self.get_token_idx(), TokenType::Comment(comment))
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
