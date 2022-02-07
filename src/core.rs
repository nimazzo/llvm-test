use crate::ast::{ASTPrimitive, FunctionAST, PrototypeAST, AST};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::compiler::Compiler;
use crate::console::Console;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::typecheck::TypeChecker;
use crate::util::mark_function_call_internal;
use anyhow::Result;

pub struct CoreLib {
    source_dir: PathBuf,
    core_functions: HashMap<String, Vec<FunctionAST>>,
    external_functions: HashMap<String, Vec<PrototypeAST>>,
    console: Console,
}

impl CoreLib {
    pub fn new(source_dir: impl AsRef<Path>, console: Console) -> Result<Self> {
        let mut core_lib = Self {
            source_dir: source_dir.as_ref().to_path_buf(),
            core_functions: Default::default(),
            external_functions: Default::default(),
            console,
        };

        CoreLib::generate_core_lib(&mut core_lib)?;

        Ok(core_lib)
    }

    // Pass core functions to Interpreter
    pub fn get_core_functions(&self) -> &HashMap<String, Vec<FunctionAST>> {
        &self.core_functions
    }

    pub fn get_external_functions(&self) -> &HashMap<String, Vec<PrototypeAST>> {
        &self.external_functions
    }

    // Internal Prototype Definitions
    pub fn get_internal_definitions(&self) -> Vec<PrototypeAST> {
        let mut ast = vec![];
        for functions in self.core_functions.values() {
            for function in functions {
                ast.push(function.proto.clone());
            }
        }
        ast
    }

    // Define external Functions
    pub fn define_external_functions(&mut self, compiler: &mut Compiler) -> Result<()> {
        for functions in self.external_functions.values_mut() {
            for proto in functions.iter_mut() {
                compiler.compile_fn_prototype("", proto)?;
            }
        }
        Ok(())
    }

    // Compile Internal Functions
    pub fn compile_internal_functions(&mut self, compiler: &mut Compiler) -> Result<()> {
        for (name, functions) in &mut self.core_functions {
            for function in functions {
                let proto = &mut function.proto;
                let fun = compiler.compile_fn_prototype(name, proto)?;
                compiler.compile_fn_body(proto, fun, &function.body)?;
            }
        }
        Ok(())
    }

    fn generate_core_lib(&mut self) -> Result<()> {
        self.console.println("[Core Lib] Generating Core Lib");
        let mut files = vec![];
        if self.source_dir.exists() && self.source_dir.is_dir() {
            for entry in std::fs::read_dir(&self.source_dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_file() {
                    files.push(path);
                }
            }
        }
        self.console.println(format!(
            "[Core Lib] Found {} source files: {:?}",
            files.len(),
            files
        ));

        let ast = self.generate_ast(files)?;

        for node in ast {
            match node {
                ASTPrimitive::Extern(proto) => self
                    .external_functions
                    .entry(proto.name.clone())
                    .or_insert_with(Vec::new)
                    .push(proto.set_internal(true)),
                ASTPrimitive::Function(fun) => self
                    .core_functions
                    .entry(fun.proto.name.clone())
                    .or_insert_with(Vec::new)
                    .push(fun),
            }
        }

        // mark internal calls as internal
        if let Some(functions) = self.core_functions.get_mut("print") {
            for function in functions {
                mark_function_call_internal("printf", &mut function.body);
            }
        }

        Ok(())
    }

    fn generate_ast(&self, files: Vec<PathBuf>) -> Result<AST> {
        self.console.println("[Core Lib] Generating AST");
        let mut source = String::new();

        for file in files {
            source.push_str(&format!(
                "\n// File: {}\n",
                file.as_path().to_string_lossy()
            ));
            let content = std::fs::read_to_string(file)?;
            source.push_str(&content);
            source.push('\n');
        }
        let mut lexer = Lexer::new(&source, self.console);
        let mut parser = Parser::new(&mut lexer, self.console);

        let mut ast = parser.parse()?;

        let mut type_checker = TypeChecker::new(self.console, &lexer);
        type_checker.run(&mut ast, false, self)?;

        self.console.println(format!(
            "[Core Lib] Found {} functions in core library",
            ast.len()
        ));
        self.console
            .println_verbose(format!("[Core Lib] AST: {:#?}", ast));
        Ok(ast)
    }
}
