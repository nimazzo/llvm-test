use crate::util::{resolve_function, INTERNAL_ERROR};
use inkwell::AddressSpace;
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
    pub context: (usize, usize),
    pub pos: (usize, usize),
}

impl PrototypeAST {
    pub fn new(
        name: String,
        args: Vec<(String, ExprType)>,
        ty: ExprType,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            name,
            args,
            ty,
            context,
            pos,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionAST {
    pub proto: PrototypeAST,
    pub body: ExprAST,
    pub context: (usize, usize),
    pub pos: (usize, usize),
}

impl FunctionAST {
    pub fn new(
        proto: PrototypeAST,
        body: ExprAST,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            proto,
            body,
            context,
            pos,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprAST {
    pub variant: ExprVariant,
    pub context: (usize, usize),
    pub pos: (usize, usize),
}

impl ExprVariant {
    pub fn type_of(&self) -> Option<ExprType> {
        match self {
            ExprVariant::Integer(_) => Some(ExprType::Integer),
            ExprVariant::String(_) => Some(ExprType::String),
            ExprVariant::Variable { ty, .. } => ty.clone(),
            ExprVariant::FunctionCall { ty, .. } => ty.clone(),
            ExprVariant::BinaryExpr { lhs, .. } => lhs.variant.type_of(),
            ExprVariant::Sequence { rhs, .. } => rhs.variant.type_of(),
            ExprVariant::Nop => Some(ExprType::Void),
        }
    }

    pub fn resolve_type(
        &mut self,
        context: &TypeContext,
        unresolved: &mut usize,
    ) -> Option<ExprType> {
        match self {
            ExprVariant::Integer(_) => Some(ExprType::Integer),
            ExprVariant::String(_) => Some(ExprType::String),
            ExprVariant::Variable { ident, ty } => match ty {
                Some(t) => Some(t.clone()),
                None => {
                    match context.variables.get(ident).map(|t| {
                        *ty = Some(t.clone());
                        t.clone()
                    }) {
                        Some(t) => Some(t),
                        None => {
                            *unresolved += 1;
                            None
                        }
                    }
                }
            },
            ExprVariant::FunctionCall {
                fn_name, args, ty, ..
            } => {
                let resolved_args = args
                    .iter_mut()
                    .filter_map(|arg| match arg.variant.resolve_type(context, unresolved) {
                        Some(t) => Some(t),
                        None => {
                            *unresolved += 1;
                            None
                        }
                    })
                    .count();
                if resolved_args != args.len() {
                    return None;
                }

                match ty {
                    Some(t) => Some(t.clone()),
                    None => {
                        let arg_types = args
                            .iter()
                            .cloned()
                            .map(|expr| expr.variant.type_of().expect(INTERNAL_ERROR))
                            .collect::<Vec<_>>();
                        match resolve_function(&context.functions, fn_name, &arg_types).map(
                            |proto| {
                                *ty = Some(proto.ty.clone());
                                &proto.ty
                            },
                        ) {
                            Some(t) => Some(t.clone()),
                            None => {
                                *unresolved += 1;
                                None
                            }
                        }
                    }
                }
            }
            ExprVariant::BinaryExpr { op, lhs, rhs } => match op {
                BinOp::Add | BinOp::Minus | BinOp::Mul | BinOp::Div => lhs
                    .variant
                    .resolve_type(context, unresolved)
                    .and(rhs.variant.resolve_type(context, unresolved)),
            },
            ExprVariant::Sequence { lhs, rhs, .. } => lhs
                .variant
                .resolve_type(context, unresolved)
                .and(rhs.variant.resolve_type(context, unresolved)),
            ExprVariant::Nop => Some(ExprType::Void),
        }
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            ExprVariant::Integer(_) => true,
            ExprVariant::String(_) => true,
            ExprVariant::Variable { .. } => true,
            ExprVariant::FunctionCall { .. } => true,
            ExprVariant::BinaryExpr { .. } => true,
            ExprVariant::Sequence { rhs, .. } => rhs.variant.requires_semicolon(),
            ExprVariant::Nop => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprVariant {
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
    pub fn new_binary_expr(
        op: BinOp,
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            variant: ExprVariant::BinaryExpr { op, lhs, rhs },
            context,
            pos,
        }
    }

    pub fn new_string(s: String, context: (usize, usize), pos: (usize, usize)) -> Self {
        Self {
            variant: ExprVariant::String(s),
            context,
            pos,
        }
    }

    pub fn new_integer(n: i32, context: (usize, usize), pos: (usize, usize)) -> Self {
        Self {
            variant: ExprVariant::Integer(n),
            context,
            pos,
        }
    }

    pub fn new_nop(context: (usize, usize), pos: (usize, usize)) -> Self {
        Self {
            variant: ExprVariant::Nop,
            context,
            pos,
        }
    }

    pub fn new_sequence(
        lhs: Box<ExprAST>,
        rhs: Box<ExprAST>,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            variant: ExprVariant::Sequence { lhs, rhs },
            context,
            pos,
        }
    }

    pub fn new_function_call(
        fn_name: String,
        args: Vec<ExprAST>,
        ty: Option<ExprType>,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            variant: ExprVariant::FunctionCall {
                fn_name,
                args,
                ty,
                internal: false,
            },
            context,
            pos,
        }
    }

    pub fn new_variable(
        ident: String,
        ty: Option<ExprType>,
        context: (usize, usize),
        pos: (usize, usize),
    ) -> Self {
        Self {
            variant: ExprVariant::Variable { ident, ty },
            context,
            pos,
        }
    }

    pub fn set_internal(mut self) -> Self {
        if let ExprVariant::FunctionCall {
            ref mut internal, ..
        } = self.variant
        {
            *internal = true;
        }
        self
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Minus,
    Mul,
    Div,
}

#[derive(PartialEq, Clone)]
pub enum ExprType {
    Integer,
    String,
    Void,

    // internal llvm types
    I8,
    I32,
    Ptr {
        inner_type: Box<ExprType>,
        address_space: AddressSpace,
    },
}

impl Debug for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_str())
    }
}

impl ExprType {
    pub fn from(value: &str) -> Result<Self, ()> {
        match value {
            "String" => Ok(ExprType::String),
            "int" => Ok(ExprType::Integer),
            "void" => Ok(ExprType::Void),
            _ => Err(()),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            ExprType::String => "String".to_string(),
            ExprType::Integer => "int".to_string(),
            ExprType::Void => "void".to_string(),
            // internal_llvm_types
            ExprType::I8 => "i8".to_string(),
            ExprType::I32 => "i32".to_string(),
            ExprType::Ptr { inner_type, .. } => format!("{}*", inner_type.as_str()),
        }
    }
}

pub struct TypeContext {
    pub functions: HashMap<String, Vec<PrototypeAST>>,
    pub variables: HashMap<String, ExprType>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn add_variables(&mut self, vars: HashMap<String, ExprType>) {
        self.variables.clear();
        self.variables.extend(vars.into_iter());
    }

    pub fn add_functions(&mut self, funs: HashMap<String, Vec<PrototypeAST>>) {
        self.functions.extend(funs.into_iter());
    }
}
