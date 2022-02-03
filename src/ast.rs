use std::collections::HashMap;
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
        internal: bool,
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
            internal: false,
        }
    }

    pub fn set_internal(mut self) -> Self {
        if let ExprAST::FunctionCall { ref mut internal, .. } = self {
            *internal = true;
        }
        self
    }

    pub fn new_variable(ident: String, ty: Option<ExprType>) -> Self {
        Self::Variable {
            ident,
            ty
        }
    }

    pub fn type_of(&self) -> Option<ExprType> {
        match self {
            ExprAST::Integer(_) => Some(ExprType::Integer),
            ExprAST::String(_) => Some(ExprType::String),
            ExprAST::Variable { ty, .. } => *ty,
            ExprAST::FunctionCall { ty, .. } => *ty,
            ExprAST::BinaryExpr { lhs, .. } => lhs.type_of(),
            ExprAST::Sequence { rhs, .. } => rhs.type_of(),
            ExprAST::Nop => Some(ExprType::Void),
        }
    }

    pub fn resolve_type(&mut self, context: &TypeContext, unresolved: &mut usize) -> Option<ExprType> {
        match self {
            ExprAST::Integer(_) => Some(ExprType::Integer),
            ExprAST::String(_) => Some(ExprType::String),
            ExprAST::Variable { ident, ty } => {
                match ty {
                    Some(t) => Some(*t),
                    None => {
                        match context.variables.get(ident).map(|t| { *ty = Some(*t); *t }) {
                            Some(t) => Some(t),
                            None => {
                                *unresolved += 1;
                                None
                            },
                        }
                    }
                }
            },
            ExprAST::FunctionCall { fn_name, args, ty, .. } => {
                let resolved_args = args.iter_mut().filter_map(|arg| {
                    match arg.resolve_type(context, unresolved) {
                        Some(t) => Some(t),
                        None => {
                            *unresolved += 1;
                            None
                        }
                    }
                }).count();
                if resolved_args != args.len() {
                    return None;
                }

                match ty {
                    Some(t) => Some(*t),
                    None => {
                        match context.functions.get(fn_name).map(|proto| { *ty = Some(proto.ty); proto.ty }) {
                            Some(t) => Some(t),
                            None => {
                                *unresolved += 1;
                                None
                            }
                        }
                    }
                }
            },
            ExprAST::BinaryExpr { op, lhs, rhs } => {
                match op {
                    BinOp::Add | BinOp::Minus | BinOp::Mul | BinOp::Div => {
                        lhs.resolve_type(context, unresolved).and(rhs.resolve_type(context, unresolved))
                    }
                }
            },
            ExprAST::Sequence { lhs, rhs, .. } => {
                lhs.resolve_type(context, unresolved).and(rhs.resolve_type(context, unresolved))
            },
            ExprAST::Nop => Some(ExprType::Void),
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
    Minus,
    Mul,
    Div,
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

pub struct TypeContext {
    pub functions: HashMap<String, PrototypeAST>,
    pub variables: HashMap<String, ExprType>
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new()
        }
    }

    pub fn add_variables(&mut self, vars: HashMap<String, ExprType>) {
        self.variables.clear();
        self.variables.extend(vars.into_iter());
    }

    pub fn add_functions(&mut self, funs: HashMap<String, PrototypeAST>) {
        self.functions.extend(funs.into_iter());
    }
}