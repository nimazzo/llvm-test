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

    let lexer = Lexer::new(input);

    println!("\n=============== Lexer ===============");
    for token in lexer.tokens() {
        println!("Parsed token: {} at:({}:{})", token.token_type, token.pos.0, token.pos.1);
    }

    let mut parser = Parser::new(lexer);
    let ast = parser.parse().expect("failed to parse");

    println!("\n=============== AST ===============");
    println!("{:#?}", ast);
    let mut compiler = Compiler::new()?;
    let program = compiler.compile(&ast)?;

    println!("\n=============== LLVM IR ===============");
    program.print_llvm_ir()?;

    println!("\n=============== Function Output ===============");
    for fun in program.functions() {
        println!("Function: {}", fun.name());
        let result: i64 = unsafe { program.call(fun.name())? };
        println!("result was: {}", result);
    }

    program.dump_bc("./output/program.bc")?;

    Ok(())
}
