// todo: prevent user from defining illegal identifiers (e.g. duplicate functions or variable called main)
// todo: implement escape char in strings (mainly \n)
// todo: implement print function for integers
// todo: improve compile errors (add context)
// todo: try to compile all function prototypes first so that they can be called before definition

extern crate inkwell;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::program::CompiledProgram;
use clap::{ErrorKind, IntoApp, Parser, Subcommand};
use std::error::Error;
use std::ffi::OsStr;
use std::fmt::{Arguments, Display};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use crate::error::CompileError;
use crate::interpreter::Interpreter;
use crate::typecheck::TypeChecker;
use anyhow::Result;

mod ast;
mod compiler;
mod core;
mod error;
mod interpreter;
mod lexer;
mod measurement;
mod parser;
mod program;
mod token;
mod typecheck;
mod util;

#[macro_export]
macro_rules! here {
    () => {
        format!("{}:{}:{} ", file!(), line!(), column!())
    };
}

macro_rules! start_timer {
    ($timer: expr, $desc: expr, $time: expr) => {{
        if $time {
            $timer.start($desc);
        }
    }};
}

macro_rules! stop_timer {
    ($timer: expr, $time: expr) => {{
        if $time {
            $timer.stop();
        }
    }};
}

macro_rules! display_timer {
    ($timer: expr, $time: expr) => {{
        if $time {
            println!("{}", $timer);
        }
    }};
}

#[derive(clap::Parser)]
#[clap(name = "Language Compiler", version, about = "Compiles source files")]
struct Cli {
    #[clap(
        short,
        long,
        help = "Runs the program after successful compilation",
        conflicts_with("interpret"),
        conflicts_with("parse-only")
    )]
    run: bool,

    #[clap(
        short,
        long,
        help = "Disables all compiler output",
        conflicts_with("verbose")
    )]
    quiet: bool,

    #[clap(short, long, help = "Show verbose output", conflicts_with("quiet"))]
    verbose: bool,

    #[clap(
        long,
        help = "Only parse source file, don't compile to native code",
        conflicts_with("run"),
        conflicts_with("out"),
        conflicts_with("print-ir")
    )]
    parse_only: bool,

    #[clap(
        short,
        long,
        help = "Interprets the program without compiling",
        conflicts_with("run"),
        conflicts_with("print-ir")
    )]
    interpret: bool,

    #[clap(long, help = "Print the parsed AST structure")]
    print_ast: bool,

    #[clap(
        long,
        help = "Print the LLVM IR code",
        conflicts_with("interpret"),
        conflicts_with("parse-only")
    )]
    print_ir: bool,

    #[clap(short, long, help = "Delete intermediate files")]
    clean: bool,

    #[clap(short, long, help = "Measure time of compilation steps")]
    time: bool,

    #[clap(
        short,
        long,
        help = "Output path for generated files",
        conflicts_with("parse-only")
    )]
    out: Option<PathBuf>,

    #[clap(help = "Path to source file")]
    source: PathBuf,

    #[clap(subcommand)]
    subcommand: Option<CliSubcommand>,
}

#[derive(Subcommand, Debug)]
enum CliSubcommand {
    #[clap(about = "Analyze Program AST")]
    Analyzer {
        #[clap(short, long, help = "Prints the functions contained in the program")]
        print_functions: bool,
    },
}

fn run(cli: Cli) -> Result<(), Box<dyn Error>> {
    let console = Console {
        quiet: cli.quiet,
        verbose: cli.verbose,
    };
    let mut timer = measurement::Timer::new();

    let mut err_app = Cli::into_app();
    err_app.set_bin_name(
        std::env::args()
            .next()
            .as_ref()
            .map(Path::new)
            .and_then(Path::file_name)
            .and_then(OsStr::to_str)
            .ok_or_else(|| {
                CompileError::generic_compilation_error("Could not get binary name", here!())
            })?,
    );

    // Check if Source file exists
    if !cli.source.exists() {
        err_app
            .error(
                ErrorKind::ArgumentConflict,
                format!("Could not find source file {:?}", cli.source),
            )
            .exit();
    }

    // Read Source File
    let input = std::fs::read_to_string(&cli.source)?;

    // AST Generation: Lexer + Parser
    start_timer!(timer, "Lexer + Parser", cli.time);
    let mut lexer = Lexer::new(&input, console);
    let mut parser = parser::Parser::new(&mut lexer, console);
    let mut ast = parser.parse()?;
    stop_timer!(timer, cli.time);

    // AST Type Checking
    start_timer!(timer, "Type Checker", cli.time);
    let mut type_checker = TypeChecker::new(console, &lexer);
    type_checker.run(&mut ast)?;
    stop_timer!(timer, cli.time);

    // Option --print-ast
    if cli.print_ast {
        console.force_println("\n=========== AST ===========");
        console.force_println(format!("{:#?}", ast));
    }

    // Option -i, --interpret
    if cli.interpret {
        start_timer!(timer, "Interpreter", cli.time);
        let mut interpreter = Interpreter::new(console);
        interpreter.run(ast);
        stop_timer!(timer, cli.time);
        display_timer!(timer, cli.time);
        return Ok(());
    }

    // Option --parse-only
    if cli.parse_only {
        display_timer!(timer, cli.time);
        return Ok(());
    }

    // Turn AST into LLVM IR
    start_timer!(timer, "LLVM IR Generation", cli.time);
    let mut compiler = Compiler::new()?;
    let program = compiler.compile(&ast)?;
    stop_timer!(timer, cli.time);

    // Option --print-ir
    if cli.print_ir {
        let ir = program.get_llvm_ir()?;
        console.force_println("\n=========== LLVM IR ===========");
        console.force_println(ir);
    }

    // Subcommands
    if let Some(cmd) = &cli.subcommand {
        match cmd {
            CliSubcommand::Analyzer { print_functions } => {
                start_timer!(timer, "Analyzer", cli.time);
                subcommand::analyzer(&program, *print_functions);
                stop_timer!(timer, cli.time);
            }
        }
    }

    let out_path = create_output_path(&cli)?;

    // Native Code Compilation
    start_timer!(timer, "LLVM IR Compilation", cli.time);
    compile_llvm_ir(&out_path, &program, console)?;
    stop_timer!(timer, cli.time);

    // Option -c, --clean
    if cli.clean {
        cleanup(&out_path, console);
    }

    // Option -r, --run
    if cli.run {
        start_timer!(timer, "Program Execution", cli.time);
        run_program(&out_path, console)?;
        stop_timer!(timer, cli.time);
    }

    display_timer!(timer, cli.time);
    Ok(())
}

fn create_output_path(cli: &Cli) -> Result<PathBuf, Box<dyn Error>> {
    match &cli.out {
        Some(out_path) => {
            let parent_path = out_path.parent();
            let file_name = PathBuf::from(out_path.file_stem().ok_or_else(|| {
                CompileError::generic_compilation_error("Invalid output path", here!())
            })?);
            if let Some(parent) = parent_path {
                Ok(parent.join(file_name))
            } else {
                Ok(file_name)
            }
        }
        None => Ok(cli.source.clone()),
    }
}

fn compile_llvm_ir(
    out_path: impl AsRef<Path>,
    program: &CompiledProgram,
    console: Console,
) -> Result<(), Box<dyn Error>> {
    create_ll_file(program, &out_path, console)?;
    create_bitcode(program, &out_path, console)?;
    create_obj_file(&out_path, console)?;
    create_executable(&out_path, console)?;

    Ok(())
}

fn create_ll_file(
    program: &CompiledProgram,
    out_path: impl AsRef<Path>,
    console: Console,
) -> Result<()> {
    console.println("[Compiler] Generating IR Code");
    program.dump_ir(out_path.as_ref().with_extension("ll"))
}

fn create_bitcode(
    program: &CompiledProgram,
    out_path: impl AsRef<Path>,
    console: Console,
) -> Result<()> {
    console.println("[Compiler] Generating Bitcode");
    program.dump_bc(out_path.as_ref().with_extension("bc"))
}

fn create_obj_file(out_path: impl AsRef<Path>, console: Console) -> Result<Output> {
    console.println("[Compiler] Generating Object File");
    let bc_path = out_path.as_ref().with_extension("bc");
    let obj_path = out_path.as_ref().with_extension("o");
    let mut cmd = Command::new("llc");
    cmd.arg("-filetype=obj")
        .arg("-o")
        .arg(obj_path)
        .arg(bc_path);
    console.println(format!("[CMD] {:?}", cmd));
    cmd.output()
        .map_err(|err| CompileError::generic_compilation_error(&err.to_string(), here!()).into())
}

fn create_executable(out_path: impl AsRef<Path>, console: Console) -> Result<Output> {
    console.println("[Compiler] Generating Executable File");
    let obj_path = out_path.as_ref().with_extension("o");
    let exe_path = out_path.as_ref().with_extension("exe");
    let mut cmd = Command::new("clang");
    cmd.arg(obj_path).arg("-o").arg(exe_path);
    console.println(format!("[CMD] {:?}", cmd));
    cmd.output()
        .map_err(|err| CompileError::generic_compilation_error(&err.to_string(), here!()).into())
}

fn cleanup(out_path: impl AsRef<Path>, console: Console) {
    let ll_file = out_path.as_ref().with_extension("ll");
    let bc_file = out_path.as_ref().with_extension("bc");
    let o_file = out_path.as_ref().with_extension("o");

    for file in &[ll_file, bc_file, o_file] {
        if file.exists() {
            match std::fs::remove_file(file) {
                Ok(_) => {
                    console.println(format!("[Compiler] Successfully removed file {:?}", file));
                }
                Err(e) => {
                    console.println(format!("[Warning] Could not remove file {:?}: {}", file, e));
                }
            }
        }
    }
}

fn run_program(out_path: impl AsRef<Path>, console: Console) -> Result<()> {
    console.println("[Compiler] Executing compiled program");
    let exe_path = out_path.as_ref().with_extension("exe");
    let mut cmd = Command::new(&exe_path);
    console.println(format!("[CMD] {:?}\n", cmd));
    let output = cmd.output().map_err(|e| {
        CompileError::runtime_error(
            &format!("Could not run program '{:?}': {}", exe_path, e),
            here!(),
        )
    })?;

    let program_output = String::from_utf8(output.stdout)?;
    program_output
        .lines()
        .for_each(|line| console.force_println(format!("[Program] {}", line)));

    if let Some(code) = output.status.code() {
        console.force_println(format!("\n[CMD] Program exited with status code: {}", code));
    }

    Ok(())
}

mod subcommand {
    use crate::{CompiledProgram, Console};

    pub fn analyzer(program: &CompiledProgram, print_functions: bool) {
        let console = Console::normal();
        if print_functions {
            print_functions_(program, console);
        }
    }

    fn print_functions_(program: &CompiledProgram, console: Console) {
        console.println("[Analyzer] Following functions are defined in the program:");
        for fun in program.functions() {
            console.println(fun);
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone)]
pub struct Console {
    quiet: bool,
    verbose: bool,
}

#[allow(dead_code)]
impl Console {
    fn quiet() -> Self {
        Self {
            quiet: true,
            verbose: false,
        }
    }
    fn normal() -> Self {
        Self {
            quiet: false,
            verbose: false,
        }
    }
    fn verbose() -> Self {
        Self {
            quiet: false,
            verbose: true,
        }
    }
}

impl Write for Console {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if !self.quiet {
            return Ok(0);
        }
        std::io::stdout().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if !self.quiet {
            return Ok(());
        }
        std::io::stdout().flush()
    }

    fn write_fmt(&mut self, fmt: Arguments<'_>) -> std::io::Result<()> {
        if !self.quiet {
            return Ok(());
        }
        std::io::stdout().write_fmt(fmt)
    }
}

impl Console {
    fn println(&self, s: impl Display) {
        if !self.quiet {
            println!("{}", s);
        }
    }

    fn force_println(&self, s: impl Display) {
        println!("{}", s);
    }

    fn println_verbose(&self, s: impl Display) {
        if self.verbose {
            println!("{}", s);
        }
    }
}

fn main() {
    let cli = Cli::parse();

    if let Err(e) = run(cli) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
