use std::collections::HashMap;
use anyhow::Result;
use crate::ast::{AST, ASTPrimitive, ExprType, FunctionAST, TypeContext};
use crate::{CompileError, Console, here};

pub struct TypeChecker {
    context: TypeContext,
    console: Console,
}

impl TypeChecker {
    pub fn new(console: Console) -> Self {
        Self { context: TypeContext::new(), console }
    }

    pub fn resolve_types(&mut self, ast: &mut AST) -> Result<()> {
        self.console.println_verbose("[Type Checker] Starting Type Resolution Process");
        let internal_defs = crate::core::get_internal_definitions();

        let mut functions = HashMap::new();
        internal_defs.iter().for_each(|proto| {
            let name = proto.name.clone();
            functions.insert(name, proto.ty);
        });

        // store all known function types
        ast.iter().filter_map(|node| {
            if let ASTPrimitive::Function(fun) = node {
                Some(fun)
            } else {
                None
            }
        }).for_each(|fun| { functions.insert(fun.proto.name.clone(), fun.proto.ty); });

        self.context.add_functions(functions);

        let mut unresolved_functions = ast.iter_mut().filter_map(|node| {
            if let ASTPrimitive::Function(fun) = node {
                Some(fun)
            } else {
                None
            }
        }).collect::<Vec<_>>();

        self.console.println_verbose(format!("[Type Checker] Found {} Function Definitions", self.context.functions.len()));
        let mut last_unresolved = unresolved_functions.len();
        let mut round = 1;
        loop {
            self.console.println_verbose(format!("[Type Checker] Round {}: {} Unresolved Functions Left", round, last_unresolved));
            let mut unresolved = 0;
            unresolved_functions.iter_mut().for_each(|fun| {
                if !self.type_check_function(fun) {
                    unresolved += 1;
                }
            });

            if unresolved == 0 {
                break;
            }

            if unresolved == last_unresolved {
                return Err(CompileError::generic_compilation_error("Infinite Loop during Type Resolving", here!()).into());
            }
            last_unresolved = unresolved;
            round += 1;
        }
        Ok(())
    }

    fn type_check_function(&mut self, fun: &mut FunctionAST) -> bool {
        let local_variables = fun.proto.args.iter().cloned().collect::<HashMap<String, ExprType>>();
        self.context.add_variables(local_variables);
        fun.body.type_of(&self.context).is_some()
    }
}