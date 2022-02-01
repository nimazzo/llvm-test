use crate::token::Token;
use crate::{token, Console, Lexer, TokenType};

use crate::ast::{ASTPrimitive, BinOp, ExprAST, ExprType, FunctionAST, PrototypeAST, AST};
use crate::error::ParseError;
use crate::here;
use anyhow::Result;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

macro_rules! parse {
    ($self: ident, $tk: path) => {{
        skip_whitespace_and_comments!($self);

        match $self.curr_token_type() {
            $tk => {
                let token = $self.advance_token();
                Ok(token)
            }
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                &format!("{}", $tk),
                here!(),
            )
            .with_pos($self.current_token.pos)),
        }
    }};
}

macro_rules! parse_identifier {
    ($self: ident) => {{
        skip_whitespace_and_comments!($self);

        match $self.curr_token_type() {
            TokenType::Identifier(id) => {
                $self.advance_token();
                Ok(id)
            }
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                "Identifier",
                here!(),
            )
            .with_pos($self.current_token.pos)),
        }
    }};
}

macro_rules! peek_identifier {
    ($self: ident) => {{
        skip_whitespace_and_comments!($self);

        match $self.curr_token_type() {
            TokenType::Identifier(id) => Ok(id),
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                "Identifier",
                here!(),
            )
            .with_pos($self.current_token.pos)),
        }
    }};
}

macro_rules! skip_whitespace_and_comments {
    ($self: ident) => {{
        loop {
            match $self.curr_token_type() {
                TokenType::Whitespace(_) => $self.advance_token(),
                TokenType::Comment(_) => $self.advance_token(),
                _ => break,
            };
        }
    }};
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    console: Console,
}

impl Parser {
    pub fn new(mut lexer: Lexer, console: Console) -> Self {
        let token = lexer.next_token();
        Self {
            lexer,
            current_token: token,
            console,
        }
    }

    pub fn parse(&mut self) -> Result<AST> {
        let mut program = vec![];
        loop {
            match self.curr_token_type() {
                TokenType::Whitespace(_) => {
                    self.advance_token();
                }
                TokenType::Comment(_) => {
                    self.advance_token();
                }

                TokenType::Fn => {
                    let fun = self.parse_function()?;
                    program.push(ASTPrimitive::Function(fun));
                }
                TokenType::Eof => {
                    break;
                }
                _ => {
                    let fun = self.parse_top_level_expression()?;
                    program.push(ASTPrimitive::Function(fun));
                }
            }
        }
        Ok(program)
    }

    /*========== Private Functions ==========*/
    fn advance_token(&mut self) -> Token {
        let current_token = self.current_token.clone();
        self.current_token = self.lexer.next_token();
        current_token
    }

    fn curr_token_type(&self) -> TokenType {
        self.current_token.token_type.clone()
    }

    fn parse_function(&mut self) -> Result<FunctionAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function");
        let proto = self.parse_function_prototype()?;
        let context_start = self.lexer.get_token_idx().0.expect("These are always set");
        let body = self.parse_function_body()?;
        let context_end = self.lexer.get_token_idx().1.expect("These are always set");

        self.type_check_function(&proto, &body, context_start, context_end)?;

        self.console
            .println_verbose("[Parser] Successfully parsed function");
        Ok(FunctionAST::new(proto, body))
    }

    fn parse_function_prototype(&mut self) -> Result<PrototypeAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function prototype");
        parse!(self, TokenType::Fn)?;
        let function_name = parse_identifier!(self)?;
        // todo: parse arguments
        parse!(self, TokenType::LeftParen)?;
        parse!(self, TokenType::RightParen)?;
        parse!(self, TokenType::RightArrow)?;
        let type_identifier = peek_identifier!(self)?;

        let pos = self.current_token.pos;
        let idx = self.current_token.idx;

        let ret_type = ExprType::try_from(type_identifier.as_str()).map_err(|_| {
            ParseError::missing_token(self.lexer.get_context((None, None)), "Type", here!())
                .with_pos(pos)
        })?;

        self.advance_token(); // eat type token
        self.console
            .println_verbose("[Parser] Successfully parsed function prototype");

        // todo: Move type checking to procedure over AST
        if function_name == "main" && ret_type != ExprType::Integer {
            return Err(ParseError::unexpected_type(
                self.lexer.get_context(idx),
                "Integer",
                ret_type.as_str(),
                here!(),
            )
            .with_pos(pos)
            .into());
        }

        Ok(PrototypeAST::new(function_name, vec![], ret_type))
    }

    fn parse_function_body(&mut self) -> Result<ExprAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function body");
        parse!(self, TokenType::LeftCurly)?;

        // Function has empty body
        if self.curr_token_type() == TokenType::RightCurly {
            parse!(self, TokenType::RightCurly)?;
            return Ok(ExprAST::Nop);
        }

        let body = self.parse_expression()?;
        parse!(self, TokenType::Semicolon)?;
        parse!(self, TokenType::RightCurly)?;
        self.console
            .println_verbose("[Parser] Successfully parsed function body");
        Ok(body)
    }

    fn parse_top_level_expression(&mut self) -> Result<FunctionAST> {
        skip_whitespace_and_comments!(self);
        self.console
            .println_verbose("[Parser] Trying to parse top level expression");
        let body = self.parse_expression()?;
        parse!(self, TokenType::Semicolon)?;
        let proto = PrototypeAST::new(ANONYMOUS_FUNCTION_NAME.into(), vec![], ExprType::Void);
        self.console
            .println_verbose("[Parser] Successfully parsed top level expression");
        Ok(FunctionAST::new(proto, body))
    }

    fn parse_expression(&mut self) -> Result<ExprAST> {
        let lhs = self.parse_primary()?;
        self.parse_binop_rhs(0, lhs)
    }

    fn parse_primary(&mut self) -> Result<ExprAST> {
        skip_whitespace_and_comments!(self);
        match self.curr_token_type() {
            TokenType::Integer(n) => self.parse_integer_expr(n),
            TokenType::DoubleQuotes => self.parse_string(),
            TokenType::Other('(') => self.parse_paren_expr(),
            TokenType::Eof => Err(ParseError::unexpected_eof(
                self.lexer.get_context((None, None)),
                here!(),
            )
            .with_pos(self.current_token.pos)
            .into()),
            token => {
                return Err(ParseError::no_primary_expression(
                    self.lexer.get_context((None, None)),
                    format!("{}", token),
                    here!(),
                )
                .with_pos(self.current_token.pos)
                .into());
            }
        }
    }

    fn parse_string(&mut self) -> Result<ExprAST> {
        let pos = self.current_token.pos;
        let start = self.current_token.idx.0;
        let mut result = String::new();
        parse!(self, TokenType::DoubleQuotes)?;

        loop {
            match self.curr_token_type() {
                TokenType::DoubleQuotes => break,
                TokenType::Eof => {
                    return Err(ParseError::unexpected_eof(
                        self.lexer.get_context((start, None)),
                        here!(),
                    )
                    .with_pos(pos)
                    .into())
                }
                token => result.push_str(&token.as_str()),
            }
            self.advance_token();
        }
        parse!(self, TokenType::DoubleQuotes)?;

        Ok(ExprAST::String(result))
    }

    fn parse_integer_expr(&mut self, n: i32) -> Result<ExprAST> {
        let result = ExprAST::Integer(n);
        self.advance_token();
        Ok(result)
    }

    fn parse_paren_expr(&mut self) -> Result<ExprAST> {
        parse!(self, TokenType::LeftParen)?;
        let expr = self.parse_expression()?;
        parse!(self, TokenType::RightParen)?;
        Ok(expr)
    }

    fn parse_binop_rhs(&mut self, expr_prec: i32, mut lhs: ExprAST) -> Result<ExprAST> {
        // if this is a binary operation, find its precedence
        loop {
            skip_whitespace_and_comments!(self);

            let token_prec = token::get_token_precedence(&self.current_token);

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            if token_prec < expr_prec {
                return Ok(lhs);
            }

            let binop = match self.curr_token_type() {
                TokenType::Plus => BinOp::Add,
                _ => {
                    return Err(ParseError::missing_token(
                        self.lexer.get_context((None, None)),
                        "binary operator",
                        here!(),
                    )
                    .with_pos(self.current_token.pos)
                    .into())
                }
            };
            // we know this is a binop
            self.advance_token(); // eat binop token

            // Parse the primary expression after the binary operator.
            let mut rhs = self.parse_primary()?;

            skip_whitespace_and_comments!(self);
            let next_prec = token::get_token_precedence(&self.current_token);
            if token_prec < next_prec {
                rhs = self.parse_binop_rhs(token_prec + 1, rhs)?;
            }

            lhs = ExprAST::new_binary_expr(binop, Box::new(lhs), Box::new(rhs));
        }
    }

    fn type_check_function(
        &self,
        proto: &PrototypeAST,
        body: &ExprAST,
        start: usize,
        end: usize,
    ) -> Result<()> {
        let fn_ret_type = proto.ty;
        let body_type = body.type_of();

        // todo: Move type checking to procedure over AST
        if fn_ret_type != body_type {
            return Err(ParseError::unexpected_type(
                self.lexer.get_context((Some(start), Some(end))),
                fn_ret_type.as_str(),
                body_type.as_str(),
                here!(),
            )
            .with_pos(self.current_token.pos)
            .into());
        }

        Ok(())
    }
}
