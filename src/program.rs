use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use std::fmt::{Display, Formatter};
use std::path::Path;

use crate::ast::ExprType;
use crate::error::CompileError;
use crate::here;
use anyhow::Result;

pub struct CompiledProgram {
    functions: Vec<CompiledFunction>,
    bitcode: MemoryBuffer,
}

impl CompiledProgram {
    pub fn functions(&self) -> &[CompiledFunction] {
        &self.functions
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
        let buffer = self
            .bitcode
            .ok_or_else(|| CompileError::generic_compilation_error("Missing bitcode", here!()))?;
        Ok(CompiledProgram {
            functions: self.functions,
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
    pub fn new(name: String, internal_name: String, ty: ExprType) -> Self {
        Self {
            public_name: name,
            internal_name,
            args: vec![],
            ty,
        }
    }

    pub fn add_argument(&mut self, name: String, ty: ExprType) -> &Self {
        self.args.push(FunctionArgument { name, ty });
        self
    }
}
