use crate::ast::{ExprAST, ExprType, ExprVariant, FunctionAST, PrototypeAST};
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
    // println!("DEBUG: Trying to resolve '{}' with args: {:?}", name, args);
    // println!("DEBUG: Functions: {:#?}", functions);
    if let Some(functions) = functions.get(name) {
        for fun in functions {
            if function_matches_call(fun, args) {
                return Some(fun);
            }
        }
    }
    // println!("DEBUG: NOT FOUND");
    None
}

pub fn mark_function_call_internal(name: &str, proto: &mut ExprAST) {
    match proto.variant {
        ExprVariant::FunctionCall {
            ref mut internal, ..
        } => {
            *internal = true;
        }
        ExprVariant::BinaryExpr {
            ref mut lhs,
            ref mut rhs,
            ..
        } => {
            mark_function_call_internal(name, lhs.as_mut());
            mark_function_call_internal(name, rhs.as_mut());
        }
        ExprVariant::Sequence {
            ref mut lhs,
            ref mut rhs,
        } => {
            mark_function_call_internal(name, lhs.as_mut());
            mark_function_call_internal(name, rhs.as_mut());
        }
        _ => {}
    }
}

pub fn resolve_function_interpreter<'a>(
    functions: &'a HashMap<String, Vec<FunctionAST>>,
    name: &str,
    args: &[ExprType],
) -> Option<&'a FunctionAST> {
    // println!("DEBUG: Trying to resolve '{}' with args: {:?}", name, args);
    // println!("DEBUG: Functions: {:#?}", functions);
    if let Some(functions) = functions.get(name) {
        for fun in functions {
            if function_matches_call(&fun.proto, args) {
                return Some(fun);
            }
        }
    }
    // println!("DEBUG: NOT FOUND");
    None
}

fn function_matches_call(proto: &PrototypeAST, args: &[ExprType]) -> bool {
    if !proto.is_var_args && proto.args.len() != args.len() {
        return false;
    }
    if args.len() >= proto.args.len()
        && proto
            .args
            .iter()
            .map(|(_, t)| t)
            .zip(args.iter())
            .all(|(t1, t2)| *t1 == *t2)
    {
        return true;
    }
    false
}

pub fn find_proto(
    name: &str,
    ret: &ExprType,
    args: &[ExprType],
    prototypes: &[PrototypeAST],
) -> Option<usize> {
    for (i, proto) in prototypes.iter().enumerate() {
        if proto.name == name
            && proto.ty == *ret
            && proto.args.iter().map(|(_, t)| t).eq(args.iter())
        {
            return Some(i);
        }
    }
    None
}

#[macro_export]
macro_rules! here {
    () => {
        format!("{}:{}:{} ", file!(), line!(), column!())
    };
}

#[macro_export]
macro_rules! start_timer {
    ($timer: expr, $desc: expr, $time: expr) => {{
        if $time {
            $timer.start($desc);
        }
    }};
}

#[macro_export]
macro_rules! stop_timer {
    ($timer: expr, $time: expr) => {{
        if $time {
            $timer.stop();
        }
    }};
}

#[macro_export]
macro_rules! display_timer {
    ($timer: expr, $time: expr) => {{
        if $time {
            println!("{}", $timer);
        }
    }};
}
