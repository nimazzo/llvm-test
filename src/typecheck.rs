use crate::ast::{
    ASTPrimitive, BinOp, ExprAST, ExprType, ExprVariant, FunctionAST, PrototypeAST, TypeContext,
    AST,
};
use crate::error::ParseError;
use crate::util::optionize;
use crate::{here, Console, Lexer};
use anyhow::Result;
use std::collections::HashMap;

pub struct TypeChecker<'a> {
    context: TypeContext,
    console: Console,
    lexer: &'a Lexer,
}

const INTERNAL_ERROR: &str = "[CRITICAL ERROR] Internal Compiler Error";

impl<'a> TypeChecker<'a> {
    pub fn new(console: Console, lexer: &'a Lexer) -> Self {
        Self {
            context: TypeContext::new(),
            console,
            lexer,
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
                let context = optionize(fun.context);
                return Err(ParseError::duplicate_function_definition(
                    self.lexer.get_context(context),
                    &old.name,
                    here!(),
                )
                .with_pos(fun.pos)
                .into());
            };
        }

        match main_function {
            Some(fun) => {
                let arg_count = fun.proto.args.len();
                if arg_count != 0 {
                    let context = optionize(fun.proto.context);
                    return Err(ParseError::wrong_main_function_signature(
                        self.lexer.get_context(context),
                        &format!(
                            "Main Function expects '0' parameters, found '{}'",
                            arg_count
                        ),
                        here!(),
                    )
                    .with_pos(fun.proto.pos)
                    .into());
                }
                let ret_type = &fun.proto.ty;
                if *ret_type != ExprType::Integer {
                    let context = optionize(fun.proto.context);
                    return Err(ParseError::wrong_main_function_signature(
                        self.lexer.get_context(context),
                        &format!("Return Type must be 'int', found '{}'", ret_type.as_str()),
                        here!(),
                    )
                    .with_pos(fun.proto.pos)
                    .into());
                }
            }
            None => {
                return Err(ParseError::missing_main("".to_string(), here!()).into());
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

            self.console.println(format!(
                "[Type Checker] Round {}: {} Unresolved Symbols Left",
                round, unresolved
            ));

            if unresolved == last_unresolved {
                let unresolved_symbol = unresolved_functions
                    .into_iter()
                    .map(|v| &v.body)
                    .filter_map(get_unresolved_symbol)
                    .next()
                    .expect(INTERNAL_ERROR);

                let context = optionize(unresolved_symbol.context);
                return Err(ParseError::unknown_type(
                    self.lexer.get_context(context),
                    "Cannot resolve type of symbol",
                    here!(),
                )
                .with_pos(unresolved_symbol.pos)
                .into());
            }
            last_unresolved = unresolved;
            round += 1;
        }
        Ok(())
    }

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
                    let body_type = fun.body.variant.type_of().expect(INTERNAL_ERROR);
                    if body_type != fun.proto.ty {
                        let context = optionize(fun.proto.context);
                        return Err(ParseError::unexpected_type(
                            self.lexer.get_context(context),
                            &fun.proto.ty.as_str(),
                            &body_type.as_str(),
                            here!(),
                        )
                        .with_pos(fun.proto.pos)
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
                let context = optionize(proto.context);
                return Err(ParseError::illegal_type(
                    self.lexer.get_context(context),
                    "void type not allowed as function arguments",
                    here!(),
                )
                .with_pos(proto.pos)
                .into());
            }
        }
        Ok(())
    }

    fn check_expr_types(&self, expr: &ExprAST) -> Result<()> {
        match &expr.variant {
            ExprVariant::FunctionCall { fn_name, args, .. } => {
                let proto = self.context.functions.get(fn_name).expect(INTERNAL_ERROR);
                if args.len() != proto.args.len() {
                    let context = optionize(expr.context);
                    return Err(ParseError::wrong_argument_count(
                        self.lexer.get_context(context),
                        proto.args.len(),
                        args.len(),
                        here!(),
                    )
                    .with_pos(expr.pos)
                    .into());
                }
                for (arg, (_, expected)) in args.iter().zip(proto.args.iter()) {
                    let arg_type = arg.variant.type_of().expect(INTERNAL_ERROR);
                    if arg_type != *expected {
                        let context = optionize(expr.context);
                        return Err(ParseError::unexpected_type(
                            self.lexer.get_context(context),
                            &expected.as_str(),
                            &arg_type.as_str(),
                            here!(),
                        )
                        .with_pos(expr.pos)
                        .into());
                    }
                }
                Ok(())
            }
            ExprVariant::BinaryExpr { op, lhs, rhs } => match op {
                BinOp::Add | BinOp::Minus | BinOp::Mul | BinOp::Div => {
                    let left_type = lhs.variant.type_of().expect(INTERNAL_ERROR);
                    let right_type = rhs.variant.type_of().expect(INTERNAL_ERROR);
                    if left_type == right_type {
                        Ok(())
                    } else {
                        let context = optionize(expr.context);
                        Err(ParseError::unexpected_type(
                            self.lexer.get_context(context),
                            &left_type.as_str(),
                            &right_type.as_str(),
                            here!(),
                        )
                        .with_pos(expr.pos)
                        .into())
                    }
                }
            },
            ExprVariant::Sequence { lhs, rhs } => {
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
        fun.body
            .variant
            .resolve_type(&self.context, unresolved)
            .is_some()
    }

    fn assert_all_types_resolved(&self, ast: &AST) -> Result<()> {
        for node in ast {
            if let ASTPrimitive::Function(fun) = node {
                if !is_resolved(&fun.body) {
                    let context = optionize(fun.body.context);
                    return Err(ParseError::unknown_type(
                        self.lexer.get_context(context),
                        "Could not resolve type (this is an internal compiler error)",
                        here!(),
                    )
                    .with_pos(fun.body.pos)
                    .into());
                }
            }
        }
        self.console
            .println("[Type Checker] All types successfully resolved");
        Ok(())
    }
}

fn is_resolved(expr: &ExprAST) -> bool {
    match &expr.variant {
        ExprVariant::Integer(_) => true,
        ExprVariant::String(_) => true,
        ExprVariant::Variable { ty, .. } => ty.is_some(),
        ExprVariant::FunctionCall { ty, .. } => ty.is_some(),
        ExprVariant::BinaryExpr { lhs, rhs, .. } => is_resolved(lhs) && is_resolved(rhs),
        ExprVariant::Sequence { lhs, rhs } => is_resolved(lhs) && is_resolved(rhs),
        ExprVariant::Nop => true,
    }
}

fn get_unresolved_symbol(expr: &ExprAST) -> Option<ExprAST> {
    match &expr.variant {
        ExprVariant::Integer(_) => None,
        ExprVariant::String(_) => None,
        ExprVariant::Variable { ty, .. } => {
            if ty.is_none() {
                Some(expr.clone())
            } else {
                None
            }
        }
        ExprVariant::FunctionCall { ty, .. } => {
            if ty.is_none() {
                Some(expr.clone())
            } else {
                None
            }
        }
        ExprVariant::BinaryExpr { lhs, rhs, .. } => {
            get_unresolved_symbol(lhs).or_else(|| get_unresolved_symbol(rhs))
        }
        ExprVariant::Sequence { lhs, rhs } => {
            get_unresolved_symbol(lhs).or_else(|| get_unresolved_symbol(rhs))
        }
        ExprVariant::Nop => None,
    }
}
