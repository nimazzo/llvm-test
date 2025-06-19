# llvm-test

This is a simple programming language and compiler built with Rust and LLVM. It is designed to demonstrate the workflow of lexing, parsing and compiling a custom programming language into native code. The language supports basic arithmetic operations, function definitions, and type checking. The compiler generates LLVM IR code, which is then compiled into native machine code using LLVM's tools. Additionally, it includes an interpreter for executing the code without compilation and various utilities for analyzing the program structure.

Sample source files are provided in the `sources` directory, and the core library is defined in `sources/std/core.txt` and `sources/std/externals.txt`.

## Table of Contents

- [Requirements](#requirements)
- [Setup](#setup)
- [Usage](#usage)
- [Language Specification](#language-specification)

## Requirements

In order to run the application, you need to have the following installed:

- Rust Compiler (tested with rustc 1.86.0)
- LLVM 13.0 installed

## Setup

To build and run this application, follow these steps:

### Clone the repository

```sh
$ git clone https://github.com/nimazzo/llvm-test.git
$ cd llvm-test
```

### Set up Environment Variables
`llvm-sys` needs to be able to find the local LLVM installation. It should normally be able to find it automatically, if `LLVM/bin` is in your `PATH`, but you may also have to set the `LLVM_SYS_130_PREFIX` variable to point to the LLVM installation directory.

+ Make sure `LLVM/bin` is in your `PATH`.
+ `LLVM_SYS_130_PREFIX`: Path to the LLVM 13 installation directory.

## Usage

### Print Help
```shell
$ cargo build
$ ./target/debug/llvm-test --help
```

```text
Compiles source files

Usage: llvm-test.exe [OPTIONS] <SOURCE> [COMMAND]

Commands:
  analyzer  Analyze Program AST
  help      Print this message or the help of the given subcommand(s)

Arguments:
  <SOURCE>  Path to source file

Options:
  -r, --run         Runs the program after successful compilation
  -q, --quiet       Disables all compiler output
  -v, --verbose     Show verbose output
      --parse-only  Only parse source file, don't compile to native code
  -i, --interpret   Interprets the program without compiling
      --print-ast   Print the parsed AST structure
      --print-ir    Print the LLVM IR code
  -c, --clean       Delete intermediate files
  -t, --time        Measure time of compilation steps
  -o, --out <OUT>   Output path for generated files
  -h, --help        Print help
  -V, --version     Print version
```

### Compile a Source File
```sh
$ ./target/debug/llvm-test -o out/program sources/simple.txt
$ ./out/program
```

### Interpret a Source File
```sh
$ ./target/debug/llvm-test -i sources/simple.txt
```
```text
[Core Lib] Generating Core Lib
[Core Lib] Found 2 source files: ["./sources/std/core.txt", "./sources/std/externals.txt"]
[Core Lib] Generating AST
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 3 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Core Lib] Found 3 functions in core library
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 4 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Interpreter] Interpreting program...

[Program] Correct
[Program] hello

[Interpreter] Main function exited with: Number(0)
```

### Run Analyzer to show defined functions
```shell
$ ./target/debug/llvm-test sources/simple.txt analyzer -p
```

```text
[Core Lib] Generating Core Lib
[Core Lib] Found 2 source files: ["./sources/std/core.txt", "./sources/std/externals.txt"]
[Core Lib] Generating AST
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 3 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Core Lib] Found 3 functions in core library
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 4 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Analyzer] Following functions are defined in the program:

  =============== Core Functions ===============
    (internal: print) fn print(s: String) -> int
    (internal: print.1) fn print(n: int) -> int

  =============== User Functions ===============
    (internal: hello) fn hello() -> String
    (internal: main) fn main() -> int

  =============== External Functions ===============
    (internal: printf) fn printf(format: String) -> int
```

### Enable Timing Information
```shell
$ ./target/debug/llvm-test -r -t -o ./out/program sources/simple.txt
```

```text
[Core Lib] Generating Core Lib
[Core Lib] Found 2 sourc9e files: ["./sources/std/core.txt", "./sources/std/externals.txt"]
[Core Lib] Generating AST
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 3 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Core Lib] Found 3 functions in core library
[Type Checker] Starting Type Resolution Process
[Type Checker] Found 4 Function Definitions
[Type Checker] All types successfully resolved
[Type Checker] Starting Type Checking Process
[Type Checker] Type Checking complete. All types match!
[Compiler] Generating IR Code
[Compiler] Generating Bitcode
[Compiler] Generating Object File
[CMD] "llc" "-filetype=obj" "-o" "./out\\program.o" "./out\\program.bc"
[Compiler] Generating Executable File
[CMD] "clang" "./out\\program.o" "-o" "./out\\program.exe"
[Compiler] Executing compiled program...
[CMD] "./out\\program.exe"

[Program] Correct
[Program] hello

[CMD] Program exited with status code: 0

                              Timing Information
        ╔═════════════════════╦═════════════╦══════════╦═══════════════╗
        ║  Compilation  Step  ║   Seconds   ║  Millis  ║     Nanos     ║
        ╠═════════════════════╬═════════════╬══════════╬═══════════════╣
        ║ Lexer + Parser      ║     0.00050 ║        0 ║        497800 ║
        ║ Type Checker        ║     0.00178 ║        1 ║       1784800 ║
        ║ LLVM IR Generation  ║     0.00108 ║        1 ║       1079500 ║
        ║ LLVM IR Compilation ║     0.16192 ║      161 ║     161919100 ║
        ║ Program Execution   ║     0.05759 ║       57 ║      57592700 ║
        ╚═════════════════════╩═════════════╩══════════╩═══════════════╝
```

## Language Specification
### Keywords
* fn
* extern

### Data Types
* int (i32)
* String (i8 ptr)
* void

### Operations
* \+ Plus
* \- Minus
* \* Mul
* / div

## Grammar
* \<top-level-expression> ::= \<extern> | \<function>
* \<extern> ::= extern fn\<ident>(\<ident>:\<ident>, ...) -> \<ident>;
* \<fn> ::= fn\<ident>(\<ident>:\<ident>, ...) -> \<ident> { (\<expr>;)* }
* \<ident> ::= [a-zA-Z]+