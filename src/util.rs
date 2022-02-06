use crate::ast::{ExprType, FunctionAST, PrototypeAST};
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
    if let Some(functions) = functions.get(name) {
        for fun in functions {
            if !fun.is_var_args && fun.args.len() != args.len() {
                return None;
            }

            if args.len() >= fun.args.len()
                && fun
                    .args
                    .iter()
                    .map(|(_, t)| t)
                    .zip(args.iter())
                    .all(|(t1, t2)| *t1 == *t2)
            {
                return Some(fun);
            }
        }
    }
    None
}

pub fn resolve_function_interpreter<'a>(
    functions: &'a HashMap<String, Vec<FunctionAST>>,
    name: &str,
    args: &[ExprType],
) -> Option<&'a FunctionAST> {
    if let Some(functions) = functions.get(name) {
        for fun in functions {
            if !fun.proto.is_var_args && fun.proto.args.len() != args.len() {
                return None;
            }
            if args.len() >= fun.proto.args.len()
                && fun
                    .proto
                    .args
                    .iter()
                    .map(|(_, t)| t)
                    .zip(args.iter())
                    .all(|(t1, t2)| *t1 == *t2)
            {
                return Some(fun);
            }
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
