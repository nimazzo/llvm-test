use inkwell::AddressSpace;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::FunctionValue;
use crate::ast::{ExprAST, ExprType, PrototypeAST};
use crate::{CompileError, Compiler, here};
use crate::core::InternalFunction::PrintString;

use anyhow::Result;
use crate::core::ExternalFuncton::Printf;

pub enum InternalFunction {
    PrintString,
}

pub enum ExternalFuncton {
    Printf,
}

// Internal Prototype Definitions
pub fn get_internal_definitions() -> Vec<PrototypeAST> {
    let mut ast = vec![];
    let functions = [PrintString]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            PrintString => { ast.push(get_print_string_definition()); }
        }
    }
    ast
}

fn get_print_string_definition() -> PrototypeAST {
    PrototypeAST::new("print".to_string(), vec![("s".to_string(), ExprType::String)], ExprType::Integer)
}

// Define external Functions
pub fn define_external_functions(compiler: &mut Compiler) -> Result<()> {
    let functions = [Printf]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            Printf => { define_printf(compiler)?; }
        }
    }
    Ok(())
}

fn define_printf(compiler: &Compiler) -> Result<()> {
    let name = "printf";
    let i8_ptr = compiler.context.i8_type().ptr_type(AddressSpace::Generic).into();
    let fn_type = compiler.context.i32_type().fn_type(&[i8_ptr], true);
    compiler.module.add_function(name, fn_type, None);
    Ok(())
}

// Compile Internal Functions
pub fn compile_internal_functions(compiler: &mut Compiler) -> Result<()> {
    let functions = [PrintString]; // !!!Add new Values here!!!
    for function in functions {
        match function {
            PrintString => { compile_print_string(compiler)?;},
        }
    }
    Ok(())
}

fn compile_print_string<'ctx>(compiler: &'ctx mut Compiler) -> Result<FunctionValue<'ctx>> {
    // compile function prototype
    let internal_name = "__internal_print_string".to_string();
    let public_name = "print".to_string();

    let args = vec![("s".to_string(), ExprType::String)];
    let proto = PrototypeAST::new(internal_name.clone(), args, ExprType::Integer);

    let function = {
        let args_types = vec![BasicMetadataTypeEnum::from(compiler.context.i8_type().ptr_type(AddressSpace::Generic))];
        let fn_type = compiler.context.i32_type().fn_type(&args_types, false);
        compiler.module.add_function(&internal_name, fn_type, None)
    };

    // Compile function body
    compiler.alloc_fn_arguments(function, &proto)?;

    // compile function body
    let body = ExprAST::FunctionCall {
        fn_name: "printf".to_string(),
        args: vec![ExprAST::String("%s\n".to_string()), ExprAST::Variable {
            ident: "s".to_string(),
            ty: Some(ExprType::String),
        }],
        ty: Some(ExprType::Integer),
        internal: true,
    };

    let compiled_body = compiler.compile_expr(&body)?;
    compiler.builder.build_return(Some(&compiled_body));

    // reset fn specific fields to default
    compiler.curr_fn = None;
    compiler.variables.clear();

    if function.verify(true) {
        compiler.functions.insert(public_name, proto);
        Ok(function)
    } else {
        unsafe {
            function.delete();
        }
        Err(CompileError::generic_compilation_error("Could not build function", here!()).into())
    }
}