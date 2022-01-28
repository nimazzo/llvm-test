extern crate core;
extern crate inkwell;

use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;
use std::error::Error;

mod ast;
mod compiler;
mod error;
mod lexer;
mod parser;
mod program;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let input = include_str!("../sources/simple.txt");

    let mut lexer = Lexer::new(input);
    for token in lexer.tokens() {
    }

    // let mut parser = Parser::new(lexer);
    // let ast = parser.parse().expect("failed to parse");
    // let mut compiler = Compiler::new()?;
    // let program = compiler.compile(&ast)?;
    //
    // program.print_llvm_ir();
    // for fun in program.functions() {
    //     println!("Function: {}", fun.name());
    //     let result: i64 = unsafe { program.call(fun.name()) };
    //     println!("result was: {}", result);
    // }
    //
    // program.dump_bc("./output/program.bc")?;

    Ok(())
}
