use crate::token::Token;
use crate::{token, Lexer, TokenType};

use crate::ast::{ASTPrimitive, BinOp, ExprAST, FunctionAST, AST, PrototypeAST};
use crate::error::ParseError;
use anyhow::Result;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

macro_rules! parse {
    ($self: ident, $tk: path) => {{
        match $self.curr_token_type() {
            $tk => {
                let token = $self.advance_token();
                Ok(token)
            },
            _ => Err(
                ParseError::missing_token($self.lexer.get_context(), &format!("{}", $tk))
                    .with_pos($self.current_token.pos),
            ),
        }
    }};
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let token = lexer.next_token();
        Self {
            lexer,
            current_token: token,
        }
    }

    pub fn parse(&mut self) -> Result<AST> {
        let mut program = vec![];
        loop {
            match self.curr_token_type() {
                TokenType::Fn => {
                    let fun = self.parse_function()?;
                    program.push(ASTPrimitive::FunctionAST(fun));
                }
                TokenType::Eof => {
                    break;
                }
                _ => {
                    let fun = self.parse_top_level_expression()?;
                    program.push(ASTPrimitive::FunctionAST(fun));
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
        let proto = self.parse_function_prototype()?;
        let body = self.parse_function_body()?;
        Ok(FunctionAST::new(proto, body))
    }

    fn parse_function_prototype(&mut self) -> Result<PrototypeAST> {
        parse!(self, TokenType::Fn)?;
        let function_name = self.get_identifier_token()?;
        self.advance_token(); // eat fn identifier
        // todo: parse arguments
        parse!(self, TokenType::LeftParen)?;
        parse!(self, TokenType::RightParen)?;
        Ok(PrototypeAST::new(function_name, vec![]))
    }

    fn parse_function_body(&mut self) -> Result<ExprAST> {
        parse!(self, TokenType::LeftCurly)?;

        // Function has empty body
        if self.curr_token_type() == TokenType::RightCurly {
            parse!(self, TokenType::RightCurly)?;
            return Ok(ExprAST::NOP);
        }

        let body = self.parse_expression()?;
        parse!(self, TokenType::Semicolon)?;
        parse!(self, TokenType::RightCurly)?;
        Ok(body)
    }

    fn parse_top_level_expression(&mut self) -> Result<FunctionAST> {
        let body = self.parse_expression()?;
        parse!(self, TokenType::Semicolon)?;
        let proto = PrototypeAST::new(ANONYMOUS_FUNCTION_NAME.into(), vec![]);
        Ok(FunctionAST::new(proto, body))
    }

    fn parse_expression(&mut self) -> Result<ExprAST> {
        let lhs = self.parse_primary()?;
        self.parse_binop_rhs(0, lhs)
    }

    fn parse_primary(&mut self) -> Result<ExprAST> {
        match self.curr_token_type() {
            TokenType::Number(_) => self.parse_number_expr(),
            TokenType::Other('(') => self.parse_paren_expr(),
            TokenType::Eof => Err(ParseError::unexpected_eof(self.lexer.get_context())
                .with_pos(self.current_token.pos)
                .into()),
            token => {
                return Err(ParseError::no_primary_expression(
                    self.lexer.get_context(),
                    format!("{}", token),
                )
                .with_pos(self.current_token.pos)
                .into());
            }
        }
    }

    fn parse_number_expr(&mut self) -> Result<ExprAST> {
        let n = self.get_number_token()?;
        let result = ExprAST::new_number_expr(n);
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
                        self.lexer.get_context(),
                        "binary operator",
                    )
                    .with_pos(self.current_token.pos)
                    .into())
                }
            };
            // we know this is a binop
            self.advance_token(); // eat binop token

            // Parse the primary expression after the binary operator.
            let mut rhs = self.parse_primary()?;

            let next_prec = token::get_token_precedence(&self.current_token);
            if token_prec < next_prec {
                rhs = self.parse_binop_rhs(token_prec + 1, rhs)?;
            }

            lhs = ExprAST::new_binary_expr(binop, Box::new(lhs), Box::new(rhs));
        }
    }

    fn get_number_token(&self) -> Result<i64> {
        self.current_token.token_type.number().ok_or_else(|| {
            ParseError::missing_token(self.lexer.get_context(), "number")
                .with_pos(self.current_token.pos)
                .into()
        })
    }

    fn get_identifier_token(&self) -> Result<String> {
        self.current_token.token_type.identifier().ok_or_else(|| {
            ParseError::missing_token(self.lexer.get_context(), "Identifier")
                .with_pos(self.current_token.pos)
                .into()
        })
    }
}
