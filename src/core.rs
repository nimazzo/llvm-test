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

    // Internal Prototype Definitions
    pub fn get_internal_definitions(&self) -> Vec<PrototypeAST> {
        // let mut ast = vec![];
        // let functions = [PrintString, PrintInteger]; // !!!Add new Values here!!!
        // for function in functions {
        //     match function {
        //         PrintString => {
        //             ast.push(self.get_print_string_definition());
        //         }
        //         PrintInteger => {
        //             ast.push(self.get_print_integer_definition());
        //         }
        //     }
        // }
        let mut ast = vec![];
        for functions in self.core_functions.values() {
            for function in functions {
                ast.push(function.proto.clone());
            }
        }
        ast
    }

    // Define external Functions
    pub fn define_external_functions(&self, compiler: &mut Compiler) -> Result<()> {
        // let functions = [Printf]; // !!!Add new Values here!!!
        // for function in functions {
        //     match function {
        //         Printf => {
        //             self.define_printf(compiler)?;
        //         }
        //     }
        // }
        for functions in self.external_functions.values() {
            for proto in functions {
                let mut proto = proto.clone().set_internal(true);
                compiler.compile_fn_prototype("", &mut proto)?;
            }
        }
        Ok(())
    }

    // Compile Internal Functions
    pub fn compile_internal_functions(&self, compiler: &mut Compiler) -> Result<()> {
        // let functions = [PrintString, PrintInteger]; // !!!Add new Values here!!!
        // for function in functions {
        //     match function {
        //         PrintString => {
        //             self.compile_print_string(compiler)?;
        //         }
        //         PrintInteger => {
        //             self.compile_print_integer(compiler)?;
        //         }
        //     }
        // }
        for (name, functions) in &self.core_functions {
            for function in functions {
                let mut proto = function.proto.clone();
                let fun = compiler.compile_fn_prototype(name, &mut proto)?;
                compiler.compile_fn_body(&proto, fun, &function.body)?;
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
                    .push(proto),
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

    /*
    #[allow(dead_code)]
    fn get_print_string_definition(&self) -> PrototypeAST {
        PrototypeAST::new(
            "print".to_string(),
            vec![("s".to_string(), ExprType::String)],
            ExprType::Integer,
            (0, 0),
            (0, 0),
        )
    }

    #[allow(dead_code)]
    fn get_print_integer_definition(&self) -> PrototypeAST {
        PrototypeAST::new(
            "print".to_string(),
            vec![("n".to_string(), ExprType::Integer)],
            ExprType::Integer,
            (0, 0),
            (0, 0),
        )
    }

    #[allow(dead_code)]
    fn define_printf(&self, compiler: &Compiler) -> Result<()> {
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

    #[allow(dead_code)]
    fn compile_print_string(&self, compiler: &mut Compiler) -> Result<()> {
        let internal_name = "__internal_print_string";

        let mut proto = self.get_print_string_definition();
        let public_name = proto.name.clone();
        proto.name = internal_name.to_string();

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

        let fun = compiler.compile_fn_prototype(&public_name, &mut proto)?;
        compiler.compile_fn_body(&proto, fun, &body).map(|_| ())
    }

    #[allow(dead_code)]
    fn compile_print_integer(&self, compiler: &mut Compiler) -> Result<()> {
        let internal_name = "__internal_print_integer";

        let mut proto = self.get_print_integer_definition();
        let public_name = proto.name.clone();
        proto.name = internal_name.to_string();

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

        let fun = compiler.compile_fn_prototype(&public_name, &mut proto)?;
        compiler.compile_fn_body(&proto, fun, &body).map(|_| ())
    }
     */
}

pub enum InternalFunction {
    PrintString,
    PrintInteger,
}

pub enum ExternalFunction {
    Printf,
}
