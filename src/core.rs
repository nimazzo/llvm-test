use crate::ast::{ExprAST, ExprType, PrototypeAST};
use crate::core::InternalFunction::{PrintInteger, PrintString};
use crate::Compiler;
use inkwell::AddressSpace;

use crate::core::ExternalFuncton::Printf;
use anyhow::Result;

pub enum InternalFunction {
    PrintString,
    PrintInteger,
}

pub enum ExternalFuncton {
    Printf,
}

// Internal Prototype Definitions
pub fn get_internal_definitions() -> Vec<PrototypeAST> {
    let mut ast = vec![];
    let functions = [PrintString, PrintInteger]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            PrintString => {
                ast.push(get_print_string_definition());
            }
            PrintInteger => {
                ast.push(get_print_integer_definition());
            }
        }
    }
    ast
}

fn get_print_string_definition() -> PrototypeAST {
    PrototypeAST::new(
        "print".to_string(),
        vec![("s".to_string(), ExprType::String)],
        ExprType::Integer,
        (0, 0),
        (0, 0),
    )
    .set_var_args(true)
}

fn get_print_integer_definition() -> PrototypeAST {
    PrototypeAST::new(
        "print".to_string(),
        vec![("s".to_string(), ExprType::Integer)],
        ExprType::Integer,
        (0, 0),
        (0, 0),
    )
    .set_var_args(true)
}

// Define external Functions
pub fn define_external_functions(compiler: &mut Compiler) -> Result<()> {
    let functions = [Printf]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            Printf => {
                define_printf(compiler)?;
            }
        }
    }
    Ok(())
}

fn define_printf(compiler: &Compiler) -> Result<()> {
    let name = "printf";
    let i8_ptr = compiler
        .context
        .i8_type()
        .ptr_type(AddressSpace::Generic)
        .into();
    let fn_type = compiler.context.i32_type().fn_type(&[i8_ptr], true);
    compiler.module.add_function(name, fn_type, None);
    Ok(())
}

// Compile Internal Functions
pub fn compile_internal_functions(compiler: &mut Compiler) -> Result<()> {
    let functions = [PrintString, PrintInteger]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            PrintString => {
                compile_print_string(compiler)?;
            }
            PrintInteger => {
                compile_print_integer(compiler)?;
            }
        }
    }
    Ok(())
}

fn compile_print_string(compiler: &mut Compiler) -> Result<()> {
    let internal_name = "__internal_print_string";
    let public_name = "print";

    let args = vec![("s".to_string(), ExprType::String)];
    let ret_type = ExprType::Integer;
    let is_var_args = true;
    let linkage = None;

    // compile function body
    let body = ExprAST::new_function_call(
        "printf".to_string(),
        vec![
            ExprAST::new_string("%s\n".to_string(), (0, 0), (0, 0)),
            ExprAST::new_variable("s".to_string(), Some(ExprType::String), (0, 0), (0, 0)),
        ],
        Some(ExprType::Integer),
        (0, 0),
        (0, 0),
    )
    .set_internal();

    let mut proto = PrototypeAST::new(internal_name.to_string(), args, ret_type, (0, 0), (0, 0))
        .with_linkage(linkage)
        .set_var_args(is_var_args);

    let fun = compiler.compile_fn_prototype(public_name, &mut proto)?;
    compiler.compile_fn_body(&proto, fun, &body).map(|_| ())
}

fn compile_print_integer(compiler: &mut Compiler) -> Result<()> {
    let internal_name = "__internal_print_integer";
    let public_name = "print";

    let args = vec![("n".to_string(), ExprType::Integer)];
    let ret_type = ExprType::Integer;
    let is_var_args = true;
    let linkage = None;

    // compile function body
    let body = ExprAST::new_function_call(
        "printf".to_string(),
        vec![
            ExprAST::new_string("%d\n".to_string(), (0, 0), (0, 0)),
            ExprAST::new_variable("n".to_string(), Some(ExprType::Integer), (0, 0), (0, 0)),
        ],
        Some(ExprType::Integer),
        (0, 0),
        (0, 0),
    )
    .set_internal();

    let mut proto = PrototypeAST::new(internal_name.to_string(), args, ret_type, (0, 0), (0, 0))
        .with_linkage(linkage)
        .set_var_args(is_var_args);

    let fun = compiler.compile_fn_prototype(public_name, &mut proto)?;
    compiler.compile_fn_body(&proto, fun, &body).map(|_| ())
}
