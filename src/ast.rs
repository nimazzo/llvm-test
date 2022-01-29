use std::fmt::{Debug, Formatter};

pub type AST = Vec<ASTPrimitive>;

pub enum ASTPrimitive {
    ExternAST(PrototypeAST),
    FunctionAST(FunctionAST),
}

impl Debug for ASTPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTPrimitive::ExternAST(proto) => {
                if f.alternate() {
                    f.write_fmt(format_args!("{:#?}", proto))
                } else {
                    f.write_fmt(format_args!("{:?}", proto))
                }
            }
            ASTPrimitive::FunctionAST(function) => {
                if f.alternate() {
                    f.write_fmt(format_args!("{:#?}", function))
                } else {
                    f.write_fmt(format_args!("{:?}", function))
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct PrototypeAST {
    pub name: String,
    pub args: Vec<String>,
}

impl PrototypeAST {
    pub fn new(name: String, args: Vec<String>) -> Self {
        Self { name, args }
    }
}

#[derive(Debug)]
pub struct FunctionAST {
    pub proto: PrototypeAST,
    pub body: ExprAST,
}

impl FunctionAST {
    pub fn new(proto: PrototypeAST, body: ExprAST) -> Self {
        Self { proto, body }
    }
}

#[derive(Debug)]
pub enum ExprAST {
    NumberExprAST {
        value: i64,
    },
    BinaryExprAST {
        op: BinOp,
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
    },
    Nop,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
}

impl ExprAST {
    pub fn new_number_expr(value: i64) -> Self {
        Self::NumberExprAST { value }
    }

    pub fn new_binary_expr(op: BinOp, lhs: Box<ExprAST>, rhs: Box<ExprAST>) -> Self {
        Self::BinaryExprAST { op, lhs, rhs }
    }
}