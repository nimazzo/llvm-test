use crate::ast::{ASTPrimitive, BinOp, ExprAST, AST, PrototypeAST};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{BasicValue, FunctionValue, IntValue, PointerValue};
use std::collections::HashMap;
use std::error::Error;

use crate::error::CompileError;
use anyhow::Result;
use inkwell::OptimizationLevel;
use inkwell::types::BasicMetadataTypeEnum;
use crate::program::{CompiledFunction, CompiledProgram, ProgramBuilder};

#[allow(dead_code)]
pub struct Compiler<'ctx> {
    context: &'static Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
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
            curr_fn: None,
        })
    }

    pub fn compile(&mut self, ast: &AST) -> Result<CompiledProgram> {
        let mut program_builder = ProgramBuilder::new();

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
            params.iter().cloned().for_each(|arg| { function.add_argument(arg.0, arg.1); });
            program_builder.add_function(function);
        }
        let code = self.module.write_bitcode_to_memory();
        program_builder.add_bitcode(code);
        program_builder.build()
    }

    fn compile_fn_prototype(&self, proto: &PrototypeAST) -> Result<FunctionValue<'ctx>> {
        let name = &proto.name;
        let args = &proto.args;

        // return type of every function is i32
        let ret_type = self.context.i32_type();
        // type of every function argument is i32
        let args_types = std::iter::repeat(ret_type)
            .map(|ty| ty.into())
            .take(args.len())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        // todo: Remove hardcoded i32 types and replace with real types
        let fn_type = self.context.i32_type().fn_type(&args_types, false);
        let fn_val = self.module.add_function(name, fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            assert!(arg.is_int_value(), "function argument was not a int");
            arg.into_int_value().set_name(&args[i].0);
        }

        Ok(fn_val)
    }

    fn compile_fn(&mut self, proto: &PrototypeAST, body: &ExprAST) -> Result<FunctionValue<'ctx>> {
        let function = self.compile_fn_prototype(proto)?;

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.curr_fn = Some(function);
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = &proto.args[i];
            let alloca = self.create_entry_block_alloca(&arg_name.0);

            self.builder.build_store(alloca, arg);

            // todo: when to remove them?
            self.variables.insert(arg_name.0.clone(), alloca);
        }

        // compile functoin body
        let body = self.compile_expr(body)?;

        self.builder.build_return(Some(&body));

        if function.verify(true) {
            Ok(function)
        } else {
            unsafe {
                function.delete();
            }
            Err(CompileError::GenericCompilationError("Could not build function".into()).into())
        }
    }

    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
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

        builder.build_alloca(self.context.i32_type(), name)
    }

    fn compile_expr(&self, expr: &ExprAST) -> Result<IntValue<'ctx>> {
        let value = match expr {
            ExprAST::NumberExpr { value } => {
                self.context.i32_type().const_int(*value as u64, false) // todo: maybe change this to true?
            }
            ExprAST::BinaryExpr { op, lhs, rhs } => {
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;
                match op {
                    BinOp::Add => self.builder.build_int_add(lhs, rhs, "tmpadd"),
                }
            }
            ExprAST::Nop => {
                self.context.i32_type().const_int(0, false)
            }
        };
        Ok(value)
    }
}
