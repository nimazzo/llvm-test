use crate::ast::{ASTPrimitive, BinOp, ExprAST, ExprType, ExprVariant, PrototypeAST, AST};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use std::collections::HashMap;
use std::error::Error;

use crate::core::CoreLib;
use crate::error::CompileError;
use crate::here;
use crate::program::{CompiledFunction, CompiledProgram, ProgramBuilder};
use crate::util::{resolve_function, INTERNAL_ERROR};
use anyhow::Result;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::{AddressSpace, OptimizationLevel};

#[allow(dead_code)]
pub struct Compiler<'ctx> {
    pub context: &'static Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    pub variables: HashMap<String, PointerValue<'ctx>>,
    pub functions: HashMap<String, Vec<PrototypeAST>>,
    pub curr_fn: Option<FunctionValue<'ctx>>,
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

    pub fn compile(&mut self, ast: &AST, mut core_lib: CoreLib) -> Result<CompiledProgram> {
        core_lib.define_external_functions(self)?;
        core_lib.compile_internal_functions(self)?;

        let mut functions = HashMap::new();

        // compile prototypes
        for node in ast {
            match node {
                ASTPrimitive::Extern(proto) => {
                    let mut proto = proto.clone();
                    let public_name = proto.name.clone();
                    self.compile_fn_prototype(&public_name, &mut proto)?;
                }
                ASTPrimitive::Function(fun) => {
                    let mut proto = fun.proto.clone();
                    let public_name = proto.name.clone();
                    let body = &fun.body;
                    let fun = self.compile_fn_prototype(&public_name, &mut proto)?;
                    // functions.insert(public_name, (fun, proto, body));
                    functions
                        .entry(public_name)
                        .or_insert_with(Vec::new)
                        .push((fun, proto, body));
                }
            }
        }

        let mut program_builder = ProgramBuilder::new();

        // compile function bodies
        for (public_name, overloads) in functions {
            for (fun, proto, body) in overloads {
                self.compile_fn_body(&proto, fun, body)?;
                let function = CompiledFunction::new(public_name.clone(), &proto);
                program_builder.add_user_function(function);
            }
        }

        // add core functions to CompiledProgram
        core_lib
            .get_core_functions()
            .iter()
            .for_each(|(public_name, overloads)| {
                for fun in overloads {
                    let function = CompiledFunction::new(public_name.clone(), &fun.proto);
                    program_builder.add_core_function(function);
                }
            });

        // add external functions to CompiledProgram
        core_lib
            .get_external_functions()
            .iter()
            .for_each(|(public_name, overloads)| {
                for proto in overloads {
                    let function = CompiledFunction::new(public_name.clone(), proto);
                    program_builder.add_external_function(function);
                }
            });

        let code = self.module.write_bitcode_to_memory();
        program_builder.add_bitcode(code);
        program_builder.build()
    }

    pub fn compile_fn_prototype(
        &mut self,
        public_name: &str,
        proto: &mut PrototypeAST,
    ) -> Result<FunctionValue<'ctx>> {
        let args = &proto.args;
        let is_var_args = proto.is_var_args;
        let linkage = proto.linkage;

        let mut args_types = vec![];
        for (_, arg) in &proto.args {
            if let Ok(bte) = BasicTypeEnum::try_from(self.to_llvm_type(arg)) {
                args_types.push(BasicMetadataTypeEnum::from(bte));
            } else {
                return Err(
                    CompileError::generic_compilation_error("Invalid Type", here!()).into(),
                );
            }
        }

        let fn_type = if proto.ty == ExprType::Void {
            self.context.void_type().fn_type(&args_types, is_var_args)
        } else if let Ok(bt) = BasicTypeEnum::try_from(self.to_llvm_type(&proto.ty)) {
            bt.fn_type(&args_types, is_var_args)
        } else {
            return Err(CompileError::generic_compilation_error("Invalid Type", here!()).into());
        };

        let fn_val = self.module.add_function(&proto.name, fn_type, linkage);
        proto.name =
            unsafe { std::str::from_utf8_unchecked(fn_val.get_name().to_bytes()).to_string() };

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(&args[i].0)
        }

        // add function prototype to map of defined functions
        if !proto.internal {
            self.functions
                .entry(public_name.to_string())
                .or_insert_with(Vec::new)
                .push(proto.clone());
        }
        Ok(fn_val)
    }

    pub fn alloc_fn_arguments(
        &mut self,
        function: FunctionValue<'ctx>,
        proto: &PrototypeAST,
    ) -> Result<()> {
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

        Ok(())
    }

    pub fn create_entry_block_alloca(
        &self,
        name: &str,
        arg_type: &ExprType,
    ) -> Result<PointerValue<'ctx>> {
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
            ExprType::String => {
                BasicTypeEnum::from(self.context.i8_type().ptr_type(AddressSpace::Generic))
            }
            ExprType::Integer => BasicTypeEnum::from(self.context.i32_type()),
            _ => unreachable!("{}", INTERNAL_ERROR),
        };
        Ok(builder.build_alloca(arg_type, name))
    }

    pub fn compile_fn_body(
        &mut self,
        proto: &PrototypeAST,
        function: FunctionValue<'ctx>,
        body: &ExprAST,
    ) -> Result<FunctionValue> {
        // Compile function body
        self.alloc_fn_arguments(function, proto)?;

        match self.compile_expr(body) {
            Some(bev) => self.builder.build_return(Some(&bev)),
            None => self.builder.build_return(None),
        };

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

    pub fn compile_expr(&self, expr: &ExprAST) -> Option<BasicValueEnum<'ctx>> {
        let value = match &expr.variant {
            ExprVariant::Integer(value) => self
                .context
                .i32_type()
                .const_int(*value as u64, true)
                .into(),
            ExprVariant::String(s) => self.compile_string(s),
            ExprVariant::Variable { ident, .. } => {
                let ptr = self.variables.get(ident).expect(INTERNAL_ERROR);
                self.builder.build_load(*ptr, ident)
            }
            ExprVariant::FunctionCall {
                fn_name,
                args,
                internal,
                ty,
                ..
            } => {
                let (internal_fn_name, ret_type) = if *internal {
                    (
                        fn_name,
                        ty.as_ref()
                            .expect("[CRITICAL ERROR] Internal Compiler Error"),
                    )
                } else {
                    let arg_types = args
                        .iter()
                        .map(|expr| expr.variant.type_of().expect(INTERNAL_ERROR))
                        .collect::<Vec<_>>();
                    let proto = resolve_function(&self.functions, fn_name, &arg_types)
                        .expect(INTERNAL_ERROR);
                    (&proto.name, &proto.ty)
                };
                let fun = self
                    .module
                    .get_function(internal_fn_name)
                    .expect("[CRITICAL ERROR] Internal Compiler Error");

                let argsv = self.compile_call_args(args);

                // Build function call
                let call = self.builder.build_call(fun, &argsv, "tmp");
                match ret_type {
                    ExprType::String => call
                        .try_as_basic_value()
                        .left()
                        .expect("[CRITICAL ERROR] Internal Compiler Error"),
                    ExprType::Integer => call
                        .try_as_basic_value()
                        .left()
                        .expect("[CRITICAL ERROR] Internal Compiler Error"),
                    ExprType::Void => {
                        return None;
                    }
                    _ => unreachable!("{}", INTERNAL_ERROR),
                }
            }
            ExprVariant::BinaryExpr { op, lhs, rhs } => {
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;
                match op {
                    BinOp::Add => self.compile_add(lhs, rhs),
                    BinOp::Minus => self.compile_minus(lhs, rhs),
                    BinOp::Mul => self.compile_mul(lhs, rhs),
                    BinOp::Div => self.compile_div(lhs, rhs),
                }
            }
            ExprVariant::Sequence { lhs, rhs } => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?
            }
            ExprVariant::Nop => {
                return None;
            }
        };
        Some(value)
    }

    fn compile_add(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(v1), BasicValueEnum::IntValue(v2)) => {
                self.builder.build_int_add(v1, v2, "tmpadd").into()
            }
            _ => panic!("[CRITICAL ERROR] Internal Compiler Error"),
        }
    }

    fn compile_minus(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(v1), BasicValueEnum::IntValue(v2)) => {
                self.builder.build_int_sub(v1, v2, "tmpsub").into()
            }
            _ => panic!("[CRITICAL ERROR] Internal Compiler Error"),
        }
    }

    fn compile_mul(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(v1), BasicValueEnum::IntValue(v2)) => {
                self.builder.build_int_mul(v1, v2, "tmpsub").into()
            }
            _ => panic!("[CRITICAL ERROR] Internal Compiler Error"),
        }
    }

    fn compile_div(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(v1), BasicValueEnum::IntValue(v2)) => {
                self.builder.build_int_signed_div(v1, v2, "tmpsub").into()
            }
            _ => panic!("[CRITICAL ERROR] Internal Compiler Error"),
        }
    }

    fn compile_call_args(&self, args: &[ExprAST]) -> Vec<BasicMetadataValueEnum<'ctx>> {
        let mut compiled_args = Vec::with_capacity(args.len());
        for arg in args {
            compiled_args.push(self.compile_expr(arg).expect(INTERNAL_ERROR));
        }
        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
            .iter()
            .by_ref()
            .map(|&val| val.into())
            .collect();
        argsv
    }

    fn compile_string(&self, string: &str) -> BasicValueEnum<'ctx> {
        let string = self.context.const_string(string.as_bytes(), true);
        let ptr = self
            .builder
            .build_malloc(string.get_type(), "malloc_string")
            .expect(INTERNAL_ERROR);

        self.builder.build_store(ptr, string);

        let zero = self.context.i8_type().const_zero();
        let pp = unsafe { self.builder.build_gep(ptr, &[zero, zero], "gep") };
        BasicValueEnum::from(pp)
    }

    pub fn to_llvm_type(&self, ty: &ExprType) -> AnyTypeEnum<'ctx> {
        match ty {
            ExprType::I8 => AnyTypeEnum::from(self.context.i8_type()),
            ExprType::I32 => AnyTypeEnum::from(self.context.i32_type()),
            ExprType::Ptr {
                inner_type,
                address_space,
            } => match inner_type.as_ref() {
                ExprType::I8 => AnyTypeEnum::from(self.context.i8_type().ptr_type(*address_space)),
                ExprType::I32 => {
                    AnyTypeEnum::from(self.context.i32_type().ptr_type(*address_space))
                }
                _ => unimplemented!("Chained Pointer Types are not defined yet"),
            },
            ExprType::Integer => self.to_llvm_type(&ExprType::I32),
            ExprType::String => self.to_llvm_type(&ExprType::Ptr {
                inner_type: Box::new(ExprType::I8),
                address_space: AddressSpace::Generic,
            }),
            ExprType::Void => AnyTypeEnum::from(self.context.void_type()),
        }
    }
}
