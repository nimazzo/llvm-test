use crate::token::Token;

use crate::ast::{ASTPrimitive, BinOp, ExprAST, ExprType, FunctionAST, PrototypeAST, AST};
use crate::console::Console;
use crate::error::ParseError;
use crate::here;
use crate::lexer::Lexer;
use crate::util::optionize;
use anyhow::Result;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

macro_rules! parse {
    ($self: ident, $tk: path) => {{
        skip_whitespace_and_comments!($self);

        match $self.current_token.clone() {
            $tk => {
                let token = $self.advance_token();
                Ok(token)
            }
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                &format!("{}", $tk),
                here!(),
            )
            .with_pos($self.lexer.get_token_pos())),
        }
    }};
}

macro_rules! peek {
    ($self: ident) => {{
        skip_whitespace_and_comments!($self);
        $self.current_token.clone()
    }};
}

macro_rules! parse_identifier {
    ($self: ident) => {{
        skip_whitespace_and_comments!($self);

        match $self.current_token.clone() {
            Token::Identifier(id) => {
                $self.advance_token();
                Ok(id)
            }
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                "Identifier",
                here!(),
            )
            .with_pos($self.lexer.get_token_pos())),
        }
    }};
}

#[allow(unused_macros)]
macro_rules! peek_identifier {
    ($self: ident) => {{
        skip_whitespace_and_comments!($self);

        match $self.current_token.clone() {
            Token::Identifier(id) => Ok(id),
            _ => Err(ParseError::missing_token(
                $self.lexer.get_context((None, None)),
                "Identifier",
                here!(),
            )
            .with_pos($self.lexer.get_token_pos())),
        }
    }};
}

macro_rules! skip_whitespace_and_comments {
    ($self: ident) => {{
        loop {
            match &$self.current_token {
                Token::Whitespace(_) => $self.advance_token(),
                Token::Comment(_) => $self.advance_token(),
                _ => break,
            };
        }
    }};
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    console: Console,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer, console: Console) -> Self {
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
            match peek!(self) {
                Token::Fn => {
                    let fun = self.parse_function()?;
                    program.push(ASTPrimitive::Function(fun));
                }
                Token::Extern => {
                    let proto = self.parse_function_prototype()?;
                    parse!(self, Token::Semicolon)?;
                    program.push(ASTPrimitive::Extern(proto));
                }
                Token::Eof => {
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

    fn parse_function(&mut self) -> Result<FunctionAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function");
        let (context_start, _) = self.lexer.get_token_idx();
        let pos = self.lexer.get_token_pos();
        let proto = self.parse_function_prototype()?;
        let body = self.parse_function_body()?;
        let (_, context_end) = self.lexer.get_token_idx();

        self.console
            .println_verbose("[Parser] Successfully parsed function");
        Ok(FunctionAST::new(
            proto,
            body,
            (context_start, context_end),
            pos,
        ))
    }

    fn parse_function_prototype(&mut self) -> Result<PrototypeAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function prototype");
        let (context_start, _) = self.lexer.get_token_idx();
        let pos = self.lexer.get_token_pos();

        if peek!(self) == Token::Extern {
            parse!(self, Token::Extern)?;
        }

        parse!(self, Token::Fn)?;
        let function_name = parse_identifier!(self)?;

        // Parse function arguments
        let function_args = self.parse_function_arguments()?;

        // VarArgs: Check if Token is TripleDot
        let mut is_var_args = false;
        if peek!(self) == Token::TripleDot {
            is_var_args = true;
            parse!(self, Token::TripleDot)?;
            parse!(self, Token::RightParen)?;
        }

        // Parse function return type
        let ret_type = self.parse_function_return_type()?;
        let (_, context_end) = self.lexer.get_token_idx();

        self.console
            .println_verbose("[Parser] Successfully parsed function prototype");
        Ok(PrototypeAST::new(
            function_name,
            function_args,
            ret_type,
            (context_start, context_end),
            pos,
        )
        .set_var_args(is_var_args))
    }

    fn parse_function_return_type(&mut self) -> Result<ExprType> {
        parse!(self, Token::RightArrow)?;
        skip_whitespace_and_comments!(self);
        let idx = optionize(self.lexer.get_token_idx());
        let pos = self.lexer.get_token_pos();
        let type_ident = parse_identifier!(self)?;

        ExprType::from(type_ident.as_str()).map_err(|_| {
            ParseError::unknown_type(self.lexer.get_context(idx), &type_ident, here!())
                .with_pos(pos)
                .into()
        })
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<(String, ExprType)>> {
        let mut function_args = vec![];
        parse!(self, Token::LeftParen)?;
        if peek!(self) != Token::RightParen {
            loop {
                if peek!(self) == Token::TripleDot {
                    return Ok(function_args);
                }

                let arg_name = parse_identifier!(self)?;
                parse!(self, Token::Colon)?;
                skip_whitespace_and_comments!(self);
                let idx = optionize(self.lexer.get_token_idx());
                let pos = self.lexer.get_token_pos();
                let type_ident = parse_identifier!(self)?;
                function_args.push((
                    arg_name,
                    ExprType::from(type_ident.as_str()).map_err(|_| {
                        ParseError::unknown_type(self.lexer.get_context(idx), &type_ident, here!())
                            .with_pos(pos)
                    })?,
                ));
                if peek!(self) == Token::Comma {
                    parse!(self, Token::Comma)?;
                } else {
                    break;
                }
            }
        }
        parse!(self, Token::RightParen)?;
        Ok(function_args)
    }

    fn parse_function_body(&mut self) -> Result<ExprAST> {
        self.console
            .println_verbose("[Parser] Trying to parse function body");
        parse!(self, Token::LeftCurly)?;

        // Function has empty body
        if peek!(self) == Token::RightCurly {
            parse!(self, Token::RightCurly)?;
            return Ok(ExprAST::new_nop((0, 0), (0, 0)));
        }

        let mut expressions = vec![];
        loop {
            let body = self.parse_expression()?;
            if body.variant.requires_semicolon() {
                parse!(self, Token::Semicolon)?;
            }
            expressions.push(body);
            match peek!(self) {
                Token::RightCurly | Token::Eof => break,
                _ => {}
            };
        }

        let body = self.parse_sequence(expressions)?;
        parse!(self, Token::RightCurly)?;

        self.console
            .println_verbose("[Parser] Successfully parsed function body");
        Ok(body)
    }

    fn parse_sequence(&self, mut expressions: Vec<ExprAST>) -> Result<ExprAST> {
        match expressions.len() {
            0 => panic!("[CRITICAL ERROR] This is a compiler error and should never happen!"),
            1 => Ok(expressions.remove(0)),
            _ => {
                let first = expressions.remove(0);
                let second = self.parse_sequence(expressions)?;
                let (context_start, context_end) = (first.context.0, second.context.1);
                let pos = first.pos;
                let context = (context_start, context_end);
                Ok(ExprAST::new_sequence(
                    Box::new(first),
                    Box::new(second),
                    context,
                    pos,
                ))
            }
        }
    }

    fn parse_top_level_expression(&mut self) -> Result<FunctionAST> {
        let context = optionize(self.lexer.get_token_idx());
        let pos = self.lexer.get_token_pos();
        return Err(ParseError::missing_token(
            self.lexer.get_context((None, None)),
            "Function Definition or Extern Definition",
            here!(),
        )
        .with_pos(pos)
        .into());
        // skip_whitespace_and_comments!(self);
        // self.console
        //     .println_verbose("[Parser] Trying to parse top level expression");
        //
        // let (context_start, _) = self.lexer.get_token_idx();
        // let pos = self.lexer.get_token_pos();
        // let mut expressions = vec![];
        // loop {
        //     let body = self.parse_expression()?;
        //     if body.variant.requires_semicolon() {
        //         parse!(self, Token::Semicolon)?;
        //     }
        //     expressions.push(body);
        //     match peek!(self) {
        //         Token::Eof | Token::Fn => break,
        //         _ => {}
        //     }
        // }
        // let (_, context_end) = self.lexer.get_token_idx();
        // expressions.push(ExprAST::new_nop((0, 0), (0, 0)));
        //
        // let body = self.parse_sequence(expressions)?;
        //
        // let proto = PrototypeAST::new(
        //     ANONYMOUS_FUNCTION_NAME.into(),
        //     vec![],
        //     ExprType::Void,
        //     (0, 0),
        //     (0, 0),
        // );
        // self.console
        //     .println_verbose("[Parser] Successfully parsed top level expression");
        // Ok(FunctionAST::new(
        //     proto,
        //     body,
        //     (context_start, context_end),
        //     pos,
        // ))
    }

    fn parse_expression(&mut self) -> Result<ExprAST> {
        let lhs = self.parse_primary()?;
        self.parse_binop_rhs(0, lhs)
    }

    fn parse_primary(&mut self) -> Result<ExprAST> {
        // skip_whitespace_and_comments!(self);
        match peek!(self) {
            Token::Integer(n) => self.parse_integer_expr(n),
            Token::DoubleQuotes => self.parse_string(),
            Token::Identifier(_) => self.parse_identifier(),
            Token::LeftParen => self.parse_paren_expr(),
            Token::Eof => Err(ParseError::unexpected_eof(
                self.lexer.get_context((None, None)),
                here!(),
            )
            .with_pos(self.lexer.get_token_pos())
            .into()),
            token => {
                return Err(ParseError::no_primary_expression(
                    self.lexer.get_context((None, None)),
                    format!("{}", token),
                    here!(),
                )
                .with_pos(self.lexer.get_token_pos())
                .into());
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<ExprAST> {
        let context_start = self.lexer.get_token_idx().0;
        let pos = self.lexer.get_token_pos();
        let ident = parse_identifier!(self)?;
        if peek!(self) == Token::LeftParen {
            let call_args = self.parse_call_arguments()?;
            let context_end = self.lexer.get_token_idx().1;
            Ok(ExprAST::new_function_call(
                ident,
                call_args,
                None,
                (context_start, context_end),
                pos,
            ))
        } else {
            let context_end = self.lexer.get_token_idx().1;
            Ok(ExprAST::new_variable(
                ident,
                None,
                (context_start, context_end),
                pos,
            ))
        }
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<ExprAST>> {
        let mut call_args = vec![];
        parse!(self, Token::LeftParen)?;

        if peek!(self) != Token::RightParen {
            loop {
                let arg = self.parse_expression()?;

                call_args.push(arg);
                if peek!(self) == Token::Comma {
                    parse!(self, Token::Comma)?;
                } else {
                    break;
                }
            }
        }
        parse!(self, Token::RightParen)?;
        Ok(call_args)
    }

    fn parse_string(&mut self) -> Result<ExprAST> {
        let pos = self.lexer.get_token_pos();
        let start = self.lexer.get_token_idx().0;
        let mut result = String::new();
        parse!(self, Token::DoubleQuotes)?;

        loop {
            match &self.current_token {
                Token::DoubleQuotes => break,
                Token::EscapedChar(c) => {
                    if *c == 'n' {
                        result.push('\n');
                    } else {
                        let context = self.lexer.get_token_idx();
                        let context = (context.0, context.1 + 1);
                        let context = optionize(context);
                        let pos = self.lexer.get_token_pos();
                        return Err(ParseError::missing_token(
                            self.lexer.get_context(context),
                            "Escape Character (one of: 'n')",
                            here!(),
                        )
                        .with_pos(pos)
                        .into());
                    }
                }
                Token::Eof => {
                    return Err(ParseError::unexpected_eof(
                        self.lexer.get_context((Some(start), None)),
                        here!(),
                    )
                    .with_pos(pos)
                    .into())
                }
                token => result.push_str(&token.as_str()),
            }
            self.advance_token();
        }
        parse!(self, Token::DoubleQuotes)?;
        let end = self.lexer.get_token_idx().1;
        Ok(ExprAST::new_string(result, (start, end), pos))
    }

    fn parse_integer_expr(&mut self, n: i32) -> Result<ExprAST> {
        let context = self.lexer.get_token_idx();
        let pos = self.lexer.get_token_pos();
        let result = ExprAST::new_integer(n, context, pos);
        self.advance_token();
        Ok(result)
    }

    fn parse_paren_expr(&mut self) -> Result<ExprAST> {
        parse!(self, Token::LeftParen)?;
        let expr = self.parse_expression()?;
        parse!(self, Token::RightParen)?;
        Ok(expr)
    }

    fn parse_binop_rhs(&mut self, expr_prec: i32, mut lhs: ExprAST) -> Result<ExprAST> {
        // if this is a binary operation, find its precedence
        loop {
            skip_whitespace_and_comments!(self);
            let token_prec = self.current_token.get_precedence();

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            if token_prec < expr_prec {
                return Ok(lhs);
            }

            let binop = match peek!(self) {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Minus,
                Token::Mul => BinOp::Mul,
                Token::Div => BinOp::Div,
                _ => {
                    return Err(ParseError::missing_token(
                        self.lexer.get_context((None, None)),
                        "binary operator",
                        here!(),
                    )
                    .with_pos(self.lexer.get_token_pos())
                    .into())
                }
            };
            // we know this is a binop
            self.advance_token(); // eat binop token

            // Parse the primary expression after the binary operator.
            let mut rhs = self.parse_primary()?;

            skip_whitespace_and_comments!(self);
            let next_prec = self.current_token.get_precedence();
            if token_prec < next_prec {
                rhs = self.parse_binop_rhs(token_prec + 1, rhs)?;
            }

            let start = lhs.context.0;
            let pos = lhs.pos;
            let end = rhs.context.1;
            lhs = ExprAST::new_binary_expr(binop, Box::new(lhs), Box::new(rhs), (start, end), pos);
        }
    }
}
