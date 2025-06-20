extern crate inkwell;

use clap::{CommandFactory, Parser, Subcommand};
use std::error::Error;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::Result;
use clap::error::ErrorKind;
use llvm_test::compiler::Compiler;
use llvm_test::console::Console;
use llvm_test::core::CoreLib;
use llvm_test::error::CompileError;
use llvm_test::interpreter::Interpreter;
use llvm_test::lexer::Lexer;
use llvm_test::program::CompiledProgram;
use llvm_test::typecheck::TypeChecker;
use llvm_test::{display_timer, here, measurement, parser, start_timer, stop_timer};

#[derive(clap::Parser)]
#[clap(name = "Language Compiler", version, about = "Compiles source files", color = clap::ColorChoice::Always)]
struct Cli {
    #[clap(
        short,
        long,
        help = "Runs the program after successful compilation",
        conflicts_with = "interpret",
        conflicts_with = "parse_only"
    )]
    run: bool,

    #[clap(
        short,
        long,
        help = "Disables all compiler output",
        conflicts_with = "verbose"
    )]
    quiet: bool,

    #[clap(short, long, help = "Show verbose output", conflicts_with = "quiet")]
    verbose: bool,

    #[clap(
        long,
        help = "Only parse source file, don't compile to native code",
        conflicts_with = "run",
        conflicts_with = "out",
        conflicts_with = "print_ir"
    )]
    parse_only: bool,

    #[clap(
        short,
        long,
        help = "Interprets the program without compiling",
        conflicts_with = "run",
        conflicts_with = "print_ir"
    )]
    interpret: bool,

    #[clap(long, help = "Print the parsed AST structure")]
    print_ast: bool,

    #[clap(
        long,
        help = "Print the LLVM IR code",
        conflicts_with = "interpret",
        conflicts_with = "parse_only"
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
        conflicts_with = "parse_only"
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

    let mut err_app = Cli::command();
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

    // create core lib
    let core_lib = CoreLib::new("./sources/std/", console)?;

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
    type_checker.run(&mut ast, true, &core_lib)?;
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
        interpreter.run(ast, core_lib);
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
    let program = compiler.compile(&ast, core_lib)?;
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
        display_timer!(timer, cli.time);
        return Ok(());
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
                if !parent.exists() {
                    std::fs::create_dir_all(parent)?;
                }
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

fn create_obj_file(out_path: impl AsRef<Path>, console: Console) -> Result<()> {
    console.println("[Compiler] Generating Object File");
    let bc_path = out_path.as_ref().with_extension("bc");
    let obj_path = out_path.as_ref().with_extension("o");
    let mut cmd = Command::new("llc");
    cmd.arg("-filetype=obj")
        .arg("-o")
        .arg(obj_path)
        .arg(bc_path);
    console.println(format!("[CMD] {:?}", cmd));
    match cmd.output() {
        Ok(output) => {
            if output.status.success() {
                Ok(())
            } else {
                let stdout = String::from_utf8_lossy(output.stdout.as_slice());
                let stderr = String::from_utf8_lossy(output.stderr.as_slice());
                let mut msg = format!("llc exited with status code: {}", output.status);
                if console.verbose {
                    msg = format!(
                        "{}\
                        \nclang stdout: {}\
                        \nclang stderr: {}",
                        msg, stdout, stderr
                    );
                }
                Err(CompileError::generic_compilation_error(&msg, here!()).into())
            }
        }
        Err(err) => Err(CompileError::generic_compilation_error(&err.to_string(), here!()).into()),
    }
}

fn create_executable(out_path: impl AsRef<Path>, console: Console) -> Result<()> {
    console.println("[Compiler] Generating Executable File");
    let obj_path = out_path.as_ref().with_extension("o");
    let exe_path = out_path.as_ref().with_extension("exe");
    let mut cmd = Command::new("clang");
    cmd.arg(obj_path).arg("-o").arg(exe_path);
    console.println(format!("[CMD] {:?}", cmd));
    match cmd.output() {
        Ok(output) => {
            if output.status.success() {
                Ok(())
            } else {
                let stdout = String::from_utf8_lossy(output.stdout.as_slice());
                let stderr = String::from_utf8_lossy(output.stderr.as_slice());
                let mut msg = format!("clang exited with status code: {}", output.status);
                if console.verbose {
                    msg = format!(
                        "{}\
                        \nclang stdout: {}\
                        \nclang stderr: {}",
                        msg, stdout, stderr
                    );
                }
                Err(CompileError::generic_compilation_error(&msg, here!()).into())
            }
        }
        Err(err) => Err(CompileError::generic_compilation_error(&err.to_string(), here!()).into()),
    }
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
    console.println("[Compiler] Executing compiled program...");
    let exe_path = out_path.as_ref().with_extension("exe");
    let mut cmd = Command::new(&exe_path);
    console.println(format!("[CMD] {:?}\n", cmd));
    let output = cmd.output().map_err(|e| {
        CompileError::runtime_error(
            &format!("Could not run program '{:?}': {}", exe_path, e),
            here!(),
        )
    })?;

    let program_output = String::from_utf8_lossy(output.stdout.as_slice());
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
        console.println("\n  =============== Core Functions ===============");
        for fun in program.core_functions() {
            console.println(fun);
        }
        console.println("\n  =============== User Functions ===============");
        for fun in program.user_functions() {
            console.println(fun);
        }
        console.println("\n  =============== External Functions ===============");
        for fun in program.external_functions() {
            console.println(fun);
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
