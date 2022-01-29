use std::fmt::{Debug, Formatter};

#[allow(clippy::upper_case_acronyms)]
pub type AST = Vec<ASTPrimitive>;

pub enum ASTPrimitive {
    Extern(PrototypeAST),
    Function(FunctionAST),
}

impl Debug for ASTPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTPrimitive::Extern(proto) => {
                if f.alternate() {
                    f.write_fmt(format_args!("{:#?}", proto))
                } else {
                    f.write_fmt(format_args!("{:?}", proto))
                }
            }
            ASTPrimitive::Function(function) => {
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
    NumberExpr {
        value: i64,
    },
    BinaryExpr {
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
        Self::NumberExpr { value }
    }

    pub fn new_binary_expr(op: BinOp, lhs: Box<ExprAST>, rhs: Box<ExprAST>) -> Self {
        Self::BinaryExpr { op, lhs, rhs }
    }
}