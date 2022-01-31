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
    pub args: Vec<(String, ExprType)>,
    pub ty: ExprType,
}

impl PrototypeAST {
    pub fn new(name: String, args: Vec<(String, ExprType)>, ty: ExprType) -> Self {
        Self { name, args, ty }
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
    Integer {
        value: i32,
    },
    BinaryExpr {
        op: BinOp,
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
    },
    Nop,
}

impl ExprAST {
    pub fn new_integer_expr(value: i32) -> Self {
        Self::Integer { value }
    }

    pub fn new_binary_expr(op: BinOp, lhs: Box<ExprAST>, rhs: Box<ExprAST>) -> Self {
        Self::BinaryExpr { op, lhs, rhs }
    }

    pub fn type_of(&self) -> ExprType {
        match self {
            ExprAST::Integer { .. } => ExprType::Integer,
            ExprAST::BinaryExpr { op, lhs, rhs } => op.type_of(lhs, rhs),
            ExprAST::Nop => ExprType::Void,
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
}

impl BinOp {
    pub fn type_of(&self, lhs: &ExprAST, rhs: &ExprAST) -> ExprType {
        match self {
            BinOp::Add => {
                let ltype = lhs.type_of();
                let rtype = rhs.type_of();

                if ltype == rtype {
                    ltype
                } else {
                    panic!("Type mismatch, handle this later");
                }
            }
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum ExprType {
    String,
    Integer,
    Void,
}

impl Debug for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl TryFrom<&str> for ExprType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "String" => Ok(ExprType::String),
            "int" => Ok(ExprType::Integer),
            "void" => Ok(ExprType::Void),
            _ => Err(())
        }
    }
}

impl ExprType {
    pub fn as_str(&self) -> &'static str {
        match self {
            ExprType::String => "String",
            ExprType::Integer => "int",
            ExprType::Void => "void",
        }
    }
}
