use crate::ast::{ExprType, PrototypeAST};
use std::collections::HashMap;

pub const INTERNAL_ERROR: &str = "[CRITICAL ERROR] Internal Compiler Error";

pub fn optionize(context: (usize, usize)) -> (Option<usize>, Option<usize>) {
    (Some(context.0), Some(context.1))
}

pub fn resolve_function<'a>(
    functions: &'a HashMap<String, Vec<PrototypeAST>>,
    name: &str,
    args: &[ExprType],
) -> Option<&'a PrototypeAST> {
    println!(
        "[DEBUG] Trying to resolve function:\n[DEBUG] '{}' called with args: {:#?}",
        name, args
    );
    if let Some(functions) = functions.get(name) {
        for fun in functions {
            if fun.args.len() == args.len()
                && fun
                    .args
                    .iter()
                    .map(|(_, t)| t)
                    .zip(args.iter())
                    .all(|(t1, t2)| *t1 == *t2)
            {
                println!("[DEBUG] Function '{}' resolved: {:?}", name, fun);
                return Some(fun);
            }
        }
    }
    println!(
        "[DEBUG] Could not resolve function '{}' :(\n[DEBUG] Available functions are: {:#?}",
        name,
        functions.get(name)
    );
    None
}
