use std::path::Path;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::OptimizationLevel;

use anyhow::Result;
use crate::error::CompileError;

pub struct CompiledProgram {
    functions: Vec<CompiledFunction>,
    bitcode: MemoryBuffer,
}

impl CompiledProgram {
    pub fn functions(&self) -> &[CompiledFunction] {
        &self.functions
    }

    pub unsafe fn call<T>(&self, name: &str) -> Result<T> {
        let context = Context::create();
        let ee = self.make_module(&context)?.create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|err| CompileError::JITCompilationError(err.to_string()))?;
        let function = ee.get_function::<unsafe extern "C" fn() -> T>(name)
            .map_err(|err| CompileError::JITCompilationError(err.to_string()))?;
        Ok(function.call())
    }

    pub fn print_llvm_ir(&self) -> Result<()> {
        let context = Context::create();
        self.make_module(&context)?.print_to_stderr();
        Ok(())
    }

    pub fn dump_bc<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let context = Context::create();
        if !self.make_module(&context)?.write_bitcode_to_path(path.as_ref()) {
            return Err(CompileError::GenericCompilationError.into());
        }
        Ok(())
    }

    fn make_module<'ctx>(&self, context: &'ctx Context) -> Result<Module<'ctx>> {
        Module::parse_bitcode_from_buffer(&self.bitcode, context)
            .map_err(|err| CompileError::JITCompilationError(err.to_string()).into())
    }
}

pub struct ProgramBuilder {
    functions: Vec<CompiledFunction>,
    bitcode: Option<MemoryBuffer>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            bitcode: None,
        }
    }

    pub fn add_function(&mut self, function: CompiledFunction) -> &Self {
        self.functions.push(function);
        self
    }

    pub fn add_bitcode(&mut self, bitcode: MemoryBuffer) -> &Self {
        self.bitcode = Some(bitcode);
        self
    }

    pub fn build(self) -> Result<CompiledProgram> {
        let buffer = self.bitcode.ok_or(CompileError::GenericCompilationError)?;
        Ok(CompiledProgram {
            functions: self.functions,
            bitcode: buffer,
        })
    }
}

pub struct CompiledFunction {
    name: String,
    args: Vec<String>,
}

impl CompiledFunction {
    pub fn new(name: String) -> Self {
        Self {
            name,
            args: vec![],
        }
    }

    pub fn add_argument(&mut self, argument: String) -> &Self {
        self.args.push(argument);
        self
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}