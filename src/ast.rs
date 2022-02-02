use std::fmt::{Debug, Formatter};

#[allow(clippy::upper_case_acronyms)]
pub type AST = Vec<ASTPrimitive>;

#[derive(Clone)]
pub enum ASTPrimitive {
    #[allow(dead_code)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct FunctionAST {
    pub proto: PrototypeAST,
    pub body: ExprAST,
}

impl FunctionAST {
    pub fn new(proto: PrototypeAST, body: ExprAST) -> Self {
        Self { proto, body }
    }
}

#[derive(Debug, Clone)]
pub enum ExprAST {
    Integer(i32),
    String(String),
    Variable {
        ident: String,
        ty: Option<ExprType>,
    },
    FunctionCall {
        fn_name: String,
        args: Vec<ExprAST>,
        ty: Option<ExprType>,
    },
    BinaryExpr {
        op: BinOp,
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
    },
    Sequence {
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
    },
    Nop,
}

impl ExprAST {
    pub fn new_binary_expr(op: BinOp, lhs: Box<ExprAST>, rhs: Box<ExprAST>) -> Self {
        Self::BinaryExpr { op, lhs, rhs }
    }

    pub fn new_sequence(lhs: Box<ExprAST>, rhs: Box<ExprAST>) -> Self {
        Self::Sequence { lhs, rhs }
    }

    pub fn new_function_call(fn_name: String, args: Vec<ExprAST>, ty: Option<ExprType>) -> Self {
        Self::FunctionCall {
            fn_name,
            args,
            ty,
        }
    }

    pub fn new_variable(ident: String, ty: Option<ExprType>) -> Self {
        Self::Variable {
            ident,
            ty
        }
    }

    pub fn type_of(&self) -> ExprType {
        match self {
            ExprAST::Integer(_) => ExprType::Integer,
            ExprAST::String(_) => ExprType::String,
            ExprAST::Variable { ty, .. } => ty.unwrap(),
            ExprAST::FunctionCall { ty, .. } => ty.unwrap(),
            ExprAST::BinaryExpr { op, lhs, rhs } => op.type_of(lhs, rhs),
            ExprAST::Sequence { rhs, .. } => rhs.type_of(),
            ExprAST::Nop => ExprType::Void,
        }
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            ExprAST::Integer(_) => true,
            ExprAST::String(_) => true,
            ExprAST::Variable { .. } => true,
            ExprAST::FunctionCall { .. } => true,
            ExprAST::BinaryExpr { .. } => true,
            ExprAST::Sequence { rhs, .. } => rhs.requires_semicolon(),
            ExprAST::Nop => false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
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
                    // todo: type check
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

impl ExprType {
    pub fn from(value: &str) -> Result<Self, ()> {
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
