use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use std::fmt::{Display, Formatter};
use std::path::Path;

use crate::ast::{ExprType, PrototypeAST};
use crate::error::CompileError;
use crate::here;
use anyhow::Result;

pub struct CompiledProgram {
    user_functions: Vec<CompiledFunction>,
    core_functions: Vec<CompiledFunction>,
    external_functions: Vec<CompiledFunction>,
    bitcode: MemoryBuffer,
}

impl CompiledProgram {
    pub fn user_functions(&self) -> &[CompiledFunction] {
        &self.user_functions
    }

    pub fn core_functions(&self) -> &[CompiledFunction] {
        &self.core_functions
    }

    pub fn external_functions(&self) -> &[CompiledFunction] {
        &self.external_functions
    }

    pub fn get_llvm_ir(&self) -> Result<String> {
        let context = Context::create();
        let module = self.make_module(&context)?;
        Ok(module.print_to_string().to_string())
    }

    pub fn dump_ir<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let context = Context::create();
        let res = self.make_module(&context)?.print_to_file(path);
        if let Err(err) = res {
            return Err(CompileError::generic_compilation_error(&err.to_string(), here!()).into());
        }
        Ok(())
    }

    pub fn dump_bc<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let context = Context::create();
        if !self
            .make_module(&context)?
            .write_bitcode_to_path(path.as_ref())
        {
            return Err(CompileError::generic_compilation_error(
                "Could not write bitcode to file",
                here!(),
            )
            .into());
        }
        Ok(())
    }

    fn make_module<'ctx>(&self, context: &'ctx Context) -> Result<Module<'ctx>> {
        Module::parse_bitcode_from_buffer(&self.bitcode, context)
            .map_err(|err| CompileError::jit_compilation_error(&err.to_string(), here!()).into())
    }
}

#[derive(Default)]
pub struct ProgramBuilder {
    user_functions: Vec<CompiledFunction>,
    core_functions: Vec<CompiledFunction>,
    external_functions: Vec<CompiledFunction>,
    bitcode: Option<MemoryBuffer>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            user_functions: vec![],
            core_functions: vec![],
            external_functions: vec![],
            bitcode: None,
        }
    }

    pub fn add_user_function(&mut self, function: CompiledFunction) -> &Self {
        self.user_functions.push(function);
        self
    }

    pub fn add_core_function(&mut self, function: CompiledFunction) -> &Self {
        self.core_functions.push(function);
        self
    }

    pub fn add_external_function(&mut self, function: CompiledFunction) -> &Self {
        self.external_functions.push(function);
        self
    }

    pub fn add_bitcode(&mut self, bitcode: MemoryBuffer) -> &Self {
        self.bitcode = Some(bitcode);
        self
    }

    pub fn build(self) -> Result<CompiledProgram> {
        let buffer = self
            .bitcode
            .ok_or_else(|| CompileError::generic_compilation_error("Missing bitcode", here!()))?;
        Ok(CompiledProgram {
            user_functions: self.user_functions,
            core_functions: self.core_functions,
            external_functions: self.external_functions,
            bitcode: buffer,
        })
    }
}

pub struct CompiledFunction {
    public_name: String,
    internal_name: String,
    args: Vec<FunctionArgument>,
    ty: ExprType,
}

pub struct FunctionArgument {
    name: String,
    ty: ExprType,
}

impl Display for CompiledFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| format!("{}: {}", arg.name, arg.ty.as_str()))
            .collect::<Vec<String>>()
            .join(", ");
        write!(
            f,
            "    (internal: {}) fn {}({}) -> {}",
            self.internal_name,
            self.public_name,
            args,
            self.ty.as_str()
        )
    }
}

impl CompiledFunction {
    pub fn new(public_name: String, proto: &PrototypeAST) -> Self {
        let mut result = Self {
            public_name,
            internal_name: proto.name.clone(),
            args: vec![],
            ty: proto.ty.clone(),
        };

        proto.args.iter().cloned().for_each(|arg| {
            result.add_argument(arg.0, arg.1);
        });

        result
    }

    pub fn add_argument(&mut self, name: String, ty: ExprType) -> &Self {
        self.args.push(FunctionArgument { name, ty });
        self
    }
}
