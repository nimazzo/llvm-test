use crate::ast::{
    ASTPrimitive, BinOp, ExprAST, ExprType, ExprVariant, FunctionAST, PrototypeAST, TypeContext,
    AST,
};
use crate::console::Console;
use crate::core::CoreLib;
use crate::error::ParseError;
use crate::here;
use crate::lexer::Lexer;
use crate::util::{optionize, resolve_function};
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

    pub fn run(&mut self, ast: &mut AST, requires_main: bool, core_lib: &CoreLib) -> Result<()> {
        self.resolve_types(ast, requires_main, core_lib)?;
        self.assert_all_types_resolved(ast)?;
        self.check_types(ast)
    }

    fn resolve_types(
        &mut self,
        ast: &mut AST,
        requires_main: bool,
        core_lib: &CoreLib,
    ) -> Result<()> {
        self.console
            .println("[Type Checker] Starting Type Resolution Process");
        let internal_defs = core_lib.get_internal_definitions();

        let mut functions = HashMap::new();
        internal_defs.iter().for_each(|proto| {
            let name = proto.name.clone();
            functions
                .entry(name)
                .or_insert_with(Vec::new)
                .push(proto.clone());
        });

        let mut main_function = None;

        let mut function_definitions = vec![];
        for node in ast.iter() {
            match node {
                ASTPrimitive::Extern(proto) => function_definitions.push(proto),
                ASTPrimitive::Function(fun) => {
                    if fun.proto.name == "main" {
                        if main_function.is_some() {
                            let context = optionize(fun.context);
                            return Err(ParseError::duplicate_function_definition(
                                self.lexer.get_context(context),
                                &fun.proto.name,
                                here!(),
                            )
                            .with_pos(fun.pos)
                            .into());
                        }

                        main_function = Some(fun.clone());
                    }
                    function_definitions.push(&fun.proto);
                }
            }
        }

        // check for duplicate function definitions
        for proto in function_definitions {
            let arg_types = proto
                .args
                .iter()
                .cloned()
                .map(|(_, t)| t)
                .collect::<Vec<_>>();
            if resolve_function(&functions, &proto.name, &arg_types).is_some() {
                let context = optionize(proto.context);
                return Err(ParseError::duplicate_function_definition(
                    self.lexer.get_context(context),
                    &proto.name,
                    here!(),
                )
                .with_pos(proto.pos)
                .into());
            }

            functions
                .entry(proto.name.clone())
                .or_insert_with(Vec::new)
                .push(proto.clone());
        }

        match &main_function {
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
                if requires_main {
                    return Err(ParseError::missing_main("".to_string(), here!()).into());
                }
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
            self.context
                .functions
                .iter()
                .fold(0, |acc, (_, funs)| acc + funs.len()),
        ));

        let mut last_unresolved = 0;
        let mut round = 1;
        loop {
            let mut unresolved = 0;
            unresolved_functions.iter_mut().for_each(|fun| {
                self.resolve_function_type(fun, &mut unresolved);
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
                let pos = unresolved_symbol.pos;
                return match &unresolved_symbol.variant {
                    ExprVariant::FunctionCall { fn_name, args, .. } => {
                        if let Some(functions) = self.context.functions.get(fn_name) {
                            self.check_args_resolved(args)?;
                            let found = args
                                .iter()
                                .map(|expr| {
                                    format!("{:?}", expr.variant.type_of().expect(INTERNAL_ERROR))
                                })
                                .collect::<Vec<String>>()
                                .join(", ");
                            let found = format!("[{}]", found);

                            let expected = functions
                                .iter()
                                .map(|fun| {
                                    fun.args
                                        .iter()
                                        .map(|(_, t)| t.clone())
                                        .collect::<Vec<ExprType>>()
                                })
                                .map(|t| {
                                    t.iter()
                                        .map(|t| format!("{:?}", t))
                                        .collect::<Vec<String>>()
                                        .join(", ")
                                })
                                .map(|lines| format!("[{}]", lines))
                                .collect::<Vec<String>>()
                                .join(", ");

                            Err(ParseError::wrong_arguments(
                                self.lexer.get_context(context),
                                &found,
                                &expected,
                                here!(),
                            )
                            .with_pos(pos)
                            .into())
                        } else {
                            Err(ParseError::unknown_function(
                                self.lexer.get_context(context),
                                fn_name,
                                here!(),
                            )
                            .with_pos(pos)
                            .into())
                        }
                    }
                    ExprVariant::Variable { ident, .. } => Err(ParseError::unknown_variable(
                        self.lexer.get_context(context),
                        ident,
                        here!(),
                    )
                    .with_pos(pos)
                    .into()),
                    _ => Err(ParseError::unknown_type(
                        self.lexer.get_context(context),
                        "Cannot resolve type of symbol",
                        here!(),
                    )
                    .with_pos(unresolved_symbol.pos)
                    .into()),
                };
            }
            last_unresolved = unresolved;
            round += 1;
        }
        Ok(())
    }

    fn check_args_resolved(&self, args: &[ExprAST]) -> Result<()> {
        if let Some(expr) = args.iter().find(|expr| expr.variant.type_of().is_none()) {
            let context = self.lexer.get_context(optionize(expr.context));
            let pos = expr.pos;
            match &expr.variant {
                ExprVariant::Variable { ident, .. } => {
                    return Err(ParseError::unknown_variable(context, ident, here!())
                        .with_pos(pos)
                        .into());
                }
                ExprVariant::FunctionCall { fn_name, args, .. } => {
                    self.check_args_resolved(args)?;
                    return Err(ParseError::unknown_function(context, fn_name, here!())
                        .with_pos(pos)
                        .into());
                }
                ExprVariant::BinaryExpr { lhs, rhs, .. } => {
                    self.check_args_resolved(&[lhs.as_ref().clone()])?;
                    self.check_args_resolved(&[rhs.as_ref().clone()])?;
                }
                ExprVariant::Sequence { lhs, rhs } => {
                    self.check_args_resolved(&[lhs.as_ref().clone()])?;
                    self.check_args_resolved(&[rhs.as_ref().clone()])?;
                }
                _ => {
                    return Ok(());
                }
            }
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
                        let context = optionize(fun.body.context);
                        return Err(ParseError::unexpected_type(
                            self.lexer.get_context(context),
                            &fun.proto.ty.as_str(),
                            &body_type.as_str(),
                            here!(),
                        )
                        .with_pos(fun.body.pos)
                        .into());
                    }
                }
            }
        }
        self.console
            .println("[Type Checker] Type Checking complete. All types match!");
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
                for arg in args {
                    self.check_expr_types(arg)?;
                }

                let arg_types = args
                    .iter()
                    .cloned()
                    .map(|expr| expr.variant.type_of().expect(INTERNAL_ERROR))
                    .collect::<Vec<_>>();
                let proto = resolve_function(&self.context.functions, fn_name, &arg_types);
                if proto.is_none() {
                    let context = optionize(expr.context);
                    return Err(ParseError::wrong_arguments(
                        self.lexer.get_context(context),
                        "todo",
                        "todo",
                        here!(),
                    )
                    .with_pos(expr.pos)
                    .into());
                }
                Ok(())
            }
            ExprVariant::BinaryExpr { op, lhs, rhs } => match op {
                BinOp::Add | BinOp::Minus | BinOp::Mul | BinOp::Div => {
                    self.check_expr_types(lhs)?;
                    self.check_expr_types(rhs)?;
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

    fn resolve_function_type(&mut self, fun: &mut FunctionAST, unresolved: &mut usize) -> bool {
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
