use crate::ast::{ASTPrimitive, BinOp, ExprAST, ExprType, PrototypeAST, AST};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;
use std::error::Error;

use crate::error::{CompileError, CompileErrorType};
use crate::program::{CompiledFunction, CompiledProgram, ProgramBuilder};
use anyhow::Result;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::{AddressSpace, OptimizationLevel};
use crate::here;

#[allow(dead_code)]
pub struct Compiler<'ctx> {
    context: &'static Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashMap<String, PrototypeAST>,
    curr_fn: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new() -> Result<Self, Box<dyn Error>> {
        let ctx: &'static mut Context = Box::leak(Box::new(Context::create()));
        let module = ctx.create_module("module");
        let builder = ctx.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(Self {
            context: ctx,
            module,
            builder,
            execution_engine,

            variables: HashMap::new(),
            functions: HashMap::new(),
            curr_fn: None,
        })
    }

    pub fn compile(&mut self, ast: &AST) -> Result<CompiledProgram> {
        let mut program_builder = ProgramBuilder::new();

        self.debug_declare_print();

        for node in ast {
            let (fun, params, ty) = match node {
                ASTPrimitive::Extern(proto) => {
                    let fun = self.compile_fn_prototype(proto)?;
                    (fun, &proto.args, proto.ty)
                }
                ASTPrimitive::Function(fun) => {
                    let proto = &fun.proto;
                    let body = &fun.body;
                    let fun = self.compile_fn(proto, body)?;
                    (fun, &proto.args, proto.ty)
                }
            };

            let mut function = CompiledFunction::new(fun.get_name().to_str()?.to_string(), ty);
            params.iter().cloned().for_each(|arg| {
                function.add_argument(arg.0, arg.1);
            });
            program_builder.add_function(function);
        }
        let code = self.module.write_bitcode_to_memory();
        program_builder.add_bitcode(code);
        program_builder.build()
    }

    fn compile_fn_prototype(&mut self, proto: &PrototypeAST) -> Result<FunctionValue<'ctx>> {
        let name = &proto.name;
        let args = &proto.args;

        let args_types = args
            .iter()
            .map(|(_, ty)| match ty {
                ExprType::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::Generic)),
                ExprType::Integer => BasicTypeEnum::from(self.context.i32_type()),
                ExprType::Void => {
                    unreachable!("Function arguments can't have void type")
                }
            })
            .map(|ty| ty.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = match &proto.ty {
            ExprType::String => self.context.i8_type().ptr_type(AddressSpace::Generic).fn_type(&args_types, false),
            ExprType::Integer => self.context.i32_type().fn_type(&args_types, false),
            ExprType::Void => self.context.void_type().fn_type(&args_types, false),
        };

        let fn_val = self.module.add_function(name, fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(&args[i].0)
        }

        // add function prototype to map of defined functions
        self.functions.insert(name.clone(), proto.clone());
        Ok(fn_val)
    }

    fn compile_fn(&mut self, proto: &PrototypeAST, body: &ExprAST) -> Result<FunctionValue<'ctx>> {
        let function = self.compile_fn_prototype(proto)?;

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.curr_fn = Some(function);
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let (arg_name, arg_type) = &proto.args[i];
            let alloca = self.create_entry_block_alloca(arg_name, arg_type)?;

            self.builder.build_store(alloca, arg);
            self.variables.insert(arg_name.clone(), alloca);
        }

        // compile function body
        match self.compile_expr(body) {
            Ok(body) => {
                self.builder.build_return(Some(&body));
            },
            Err(e) => {
                if let Some(CompileError{ error_type: CompileErrorType::VoidReturn, ..}) = e.downcast_ref::<CompileError>() {
                    self.builder.build_return(None);
                } else {
                    return Err(e);
                }
            }
        }

        // reset fn specific fields to default
        self.curr_fn = None;
        self.variables.clear();

        if function.verify(true) {
            Ok(function)
        } else {
            unsafe {
                function.delete();
            }
            Err(CompileError::generic_compilation_error("Could not build function", here!()).into())
        }
    }

    fn create_entry_block_alloca(&self, name: &str, arg_type: &ExprType) -> Result<PointerValue<'ctx>> {
        let builder = self.context.create_builder();
        let entry = self
            .curr_fn
            .and_then(|f| f.get_first_basic_block())
            .expect("must exist in order to alloc fn param");

        match entry.get_first_instruction() {
            Some(instr) => {
                builder.position_before(&instr);
            }
            None => {
                builder.position_at_end(entry);
            }
        }

        let arg_type = match arg_type {
            ExprType::String => BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::Generic)),
            ExprType::Integer => BasicTypeEnum::from(self.context.i32_type()),
            ExprType::Void => {
                return Err(CompileError::illegal_type("Void Type in Function Argument", here!()).into());
            }
        };
        Ok(builder.build_alloca(arg_type, name))
    }

    fn compile_expr(&self, expr: &ExprAST) -> Result<BasicValueEnum<'ctx>> {
        let value = match expr {
            ExprAST::Integer(value) => {
                self.context
                    .i32_type()
                    .const_int(*value as u64, false) // todo: maybe change this to true?
                    .into()
            }
            ExprAST::String(s) => self.compile_string(s)?,
            ExprAST::Variable { ident, .. } => {
                match self.variables.get(ident) {
                    Some(ptr) => {
                        self.builder.build_load(*ptr, ident)
                    }
                    None => {
                        return Err(CompileError::unknown_variable(ident, here!()).into());
                    }
                }
            }
            ExprAST::FunctionCall { fn_name, args, .. } => {
                let (internal_fn_name, ret_type, is_internal) = match self.functions.get(fn_name) {
                    Some(proto) => (&proto.name, &proto.ty, proto.internal),
                    None => {
                        return Err(CompileError::unknown_function(fn_name, here!()).into());
                    }
                };
                let fun = self.module.get_function(internal_fn_name).expect("[CRITICAL ERROR] Internal Compiler Error");

                let argsv = if is_internal {
                    self.compile_call_args_internal(internal_fn_name, args)?
                } else {
                    self.compile_call_args(args)?
                };
                // Compile function call arguments
                // let mut compiled_args = Vec::with_capacity(args.len());
                // for arg in args {
                //     compiled_args.push(self.compile_expr(arg)?);
                // }
                // let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                //     .iter().by_ref().map(|&val| val.into()).collect();

                // Build function call
                let call = self.builder.build_call(fun, &argsv, "tmp");
                match ret_type {
                    ExprType::String => call.try_as_basic_value().left().expect("[CRITICAL ERROR] Internal Compiler Error"),
                    ExprType::Integer => call.try_as_basic_value().left().expect("[CRITICAL ERROR] Internal Compiler Error"),
                    ExprType::Void => {
                        return Err(CompileError::void_return(here!()).into());
                    }
                }
            }
            ExprAST::BinaryExpr { op, lhs, rhs } => {
                let t1 = lhs.type_of();
                let t2 = lhs.type_of();

                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                match op {
                    BinOp::Add => match (lhs, rhs) {
                        (BasicValueEnum::IntValue(v1), BasicValueEnum::IntValue(v2)) => {
                            self.builder.build_int_add(v1, v2, "tmpadd").into()
                        }
                        _ => unimplemented!(
                            "BinOp::Add not implemented for {} and {}",
                            t1.as_str(),
                            t2.as_str()
                        ),
                    },
                }
            }
            ExprAST::Sequence { lhs, rhs } => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?
            }
            ExprAST::Nop => {
                return Err(CompileError::void_return(here!()).into());
            }
        };
        Ok(value)
    }

    fn compile_call_args_internal(&self, internal_fn_name: &str, args: &[ExprAST]) -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
        match internal_fn_name {
            "printf" => self.compile_call_args_internal_printf(args),
            _ => Err(CompileError::unknown_function(internal_fn_name, here!()).into()),
        }
    }

    fn compile_call_args_internal_printf(&self, args: &[ExprAST]) -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
        let fmt_string = "%s\n";
        let fmt_val = self.compile_string(fmt_string)?;
        let mut argsv = self.compile_call_args(args)?;
        argsv.insert(0, BasicMetadataValueEnum::from(fmt_val));
        Ok(argsv)
    }

    fn compile_call_args(&self, args: &[ExprAST]) -> Result<Vec<BasicMetadataValueEnum<'ctx>>> {
        let mut compiled_args = Vec::with_capacity(args.len());
        for arg in args {
            compiled_args.push(self.compile_expr(arg)?);
        }
        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
            .iter().by_ref().map(|&val| val.into()).collect();
        Ok(argsv)
    }

    fn compile_string(&self, string: &str) -> Result<BasicValueEnum<'ctx>> {
        let string = self.context.const_string(string.as_bytes(), true);
        let ptr = self.builder.build_alloca(string.get_type(), "alloc_string");
        self.builder.build_store(ptr, string);

        let zero = self.context.i8_type().const_zero();
        let pp = unsafe { self.builder.build_gep(ptr, &[zero, zero], "gep") };
        Ok(BasicValueEnum::from(pp))
    }

    fn debug_declare_print(&mut self) {
        let name = "printf";
        let i8_ptr = self.context.i8_type().ptr_type(AddressSpace::Generic).into();
        let fn_type = self.context.i32_type().fn_type(&[i8_ptr], true);
        self.module.add_function(name, fn_type, None);
        self.functions.insert("print".into(), PrototypeAST::new("printf".into(), vec![], ExprType::Integer).set_internal());
    }
}
