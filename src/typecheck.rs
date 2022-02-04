use crate::ast::{
    ASTPrimitive, BinOp, ExprAST, ExprType, FunctionAST, PrototypeAST, TypeContext, AST,
};
use crate::error::ParseError;
use crate::{here, CompileError, Console};
use anyhow::Result;
use std::collections::HashMap;

pub struct TypeChecker {
    context: TypeContext,
    console: Console,
}

const INTERNAL_ERROR: &str = "[CRITICAL ERROR] Internal Compiler Error";

impl TypeChecker {
    pub fn new(console: Console) -> Self {
        Self {
            context: TypeContext::new(),
            console,
        }
    }

    pub fn run(&mut self, ast: &mut AST) -> Result<()> {
        self.resolve_types(ast)?;
        self.assert_all_types_resolved(ast)?;
        self.check_types(ast)
    }

    fn resolve_types(&mut self, ast: &mut AST) -> Result<()> {
        self.console
            .println("[Type Checker] Starting Type Resolution Process");
        let internal_defs = crate::core::get_internal_definitions();

        let mut functions = HashMap::new();
        internal_defs.iter().for_each(|proto| {
            let name = proto.name.clone();
            functions.insert(name, proto.clone());
        });

        // store all known function types
        let function_definitions = ast.iter().filter_map(|node| {
            if let ASTPrimitive::Function(fun) = node {
                Some(fun)
            } else {
                None
            }
        });

        let mut main_function = None;
        for fun in function_definitions {
            if fun.proto.name == "main" {
                main_function = Some(fun);
            }
            if let Some(old) = functions.insert(fun.proto.name.clone(), fun.proto.clone()) {
                return Err(ParseError::duplicate_function_definition(
                    "TODO".to_string(),
                    &old.name,
                    here!(),
                )
                .into());
            };
        }

        match main_function {
            Some(fun) => {
                let arg_count = fun.proto.args.len();
                if arg_count != 0 {
                    return Err(ParseError::wrong_main_function_signature(
                        "TODO".to_string(),
                        &format!(
                            "Main Function expects '0' parameters, found '{}'",
                            arg_count
                        ),
                        here!(),
                    )
                    .into());
                }
                let ret_type = fun.proto.ty;
                if ret_type != ExprType::Integer {
                    return Err(ParseError::wrong_main_function_signature(
                        "TODO".to_string(),
                        &format!("Return Type must be 'int', found '{}'", ret_type.as_str()),
                        here!(),
                    )
                    .into());
                }
            }
            None => {
                return Err(ParseError::missing_main("TODO".to_string(), here!()).into());
            }
        }

        self.context.add_functions(functions);

        let mut unresolved_functions = ast
            .iter_mut()
            .filter_map(|node| {
                if let ASTPrimitive::Function(fun) = node {
                    Some(fun)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        self.console.println(format!(
            "[Type Checker] Found {} Function Definitions",
            self.context.functions.len()
        ));
        let mut last_unresolved = 0;
        let mut round = 1;
        loop {
            let mut unresolved = 0;
            unresolved_functions.iter_mut().for_each(|fun| {
                self.type_check_function(fun, &mut unresolved);
            });

            if unresolved == 0 {
                break;
            }

            if unresolved == last_unresolved {
                return Err(CompileError::generic_compilation_error(
                    "Infinite Loop during Type Resolving",
                    here!(),
                )
                .into());
            }
            last_unresolved = unresolved;
            self.console.println(format!(
                "[Type Checker] Round {}: {} Unresolved Symbols Left",
                round, last_unresolved
            ));
            round += 1;
        }
        Ok(())
    }

    // todo: add context to errors
    fn check_types(&self, ast: &AST) -> Result<()> {
        self.console
            .println("[Type Checker] Starting Type Checking Process");
        for node in ast {
            match node {
                ASTPrimitive::Extern(proto) => {
                    self.check_fn_proto_types(proto)?;
                }
                ASTPrimitive::Function(fun) => {
                    self.check_fn_proto_types(&fun.proto)?;
                    self.check_expr_types(&fun.body)?;
                    let body_type = fun.body.type_of().expect(INTERNAL_ERROR);
                    if body_type != fun.proto.ty {
                        return Err(ParseError::unexpected_type(
                            "TODO".to_string(),
                            fun.proto.ty.as_str(),
                            body_type.as_str(),
                            here!(),
                        )
                        .into());
                    }
                }
            }
        }
        Ok(())
    }

    fn check_fn_proto_types(&self, proto: &PrototypeAST) -> Result<()> {
        for (_, arg_type) in &proto.args {
            if *arg_type == ExprType::Void {
                return Err(ParseError::illegal_type(
                    "TODO".to_string(),
                    "void type not allowed as function arguments",
                    here!(),
                )
                .into());
            }
        }
        Ok(())
    }

    fn check_expr_types(&self, expr: &ExprAST) -> Result<()> {
        match expr {
            ExprAST::FunctionCall { fn_name, args, .. } => {
                let proto = self.context.functions.get(fn_name).expect(INTERNAL_ERROR);
                if args.len() != proto.args.len() {
                    return Err(ParseError::wrong_argument_count(
                        "TODO".to_string(),
                        proto.args.len(),
                        args.len(),
                        here!(),
                    )
                    .into());
                }
                for (arg, (_, expected)) in args.iter().zip(proto.args.iter()) {
                    let arg_type = arg.type_of().expect(INTERNAL_ERROR);
                    if arg_type != *expected {
                        return Err(ParseError::unexpected_type(
                            "TODO".to_string(),
                            expected.as_str(),
                            arg_type.as_str(),
                            here!(),
                        )
                        .into());
                    }
                }
                Ok(())
            }
            ExprAST::BinaryExpr { op, lhs, rhs } => match op {
                BinOp::Add | BinOp::Minus | BinOp::Mul | BinOp::Div => {
                    let left_type = lhs.type_of().expect(INTERNAL_ERROR);
                    let right_type = rhs.type_of().expect(INTERNAL_ERROR);
                    if left_type == right_type {
                        Ok(())
                    } else {
                        Err(ParseError::unexpected_type(
                            "TODO".to_string(),
                            left_type.as_str(),
                            right_type.as_str(),
                            here!(),
                        )
                        .into())
                    }
                }
            },
            ExprAST::Sequence { lhs, rhs } => {
                self.check_expr_types(lhs).and(self.check_expr_types(rhs))
            }
            _ => Ok(()),
        }
    }

    fn type_check_function(&mut self, fun: &mut FunctionAST, unresolved: &mut usize) -> bool {
        let local_variables = fun
            .proto
            .args
            .iter()
            .cloned()
            .collect::<HashMap<String, ExprType>>();
        self.context.add_variables(local_variables);
        fun.body.resolve_type(&self.context, unresolved).is_some()
    }

    fn assert_all_types_resolved(&self, ast: &AST) -> Result<()> {
        for node in ast {
            if let ASTPrimitive::Function(fun) = node {
                if !self.is_resolved(&fun.body) {
                    return Err(ParseError::unknown_type(
                        "TODO".to_string(),
                        "Could not resolve type (this is an internal compiler error)",
                        here!(),
                    )
                    .into());
                }
            }
        }
        self.console
            .println("[Type Checker] All types successfully resolved");
        Ok(())
    }

    fn is_resolved(&self, expr: &ExprAST) -> bool {
        match expr {
            ExprAST::Integer(_) => true,
            ExprAST::String(_) => true,
            ExprAST::Variable { ty, .. } => ty.is_some(),
            ExprAST::FunctionCall { ty, .. } => ty.is_some(),
            ExprAST::BinaryExpr { lhs, rhs, .. } => self.is_resolved(lhs) && self.is_resolved(rhs),
            ExprAST::Sequence { lhs, rhs } => self.is_resolved(lhs) && self.is_resolved(rhs),
            ExprAST::Nop => true,
        }
    }
}
