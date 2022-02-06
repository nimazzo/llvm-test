use llvm_test::console::Console;
use llvm_test::lexer::Lexer;
use llvm_test::parser::Parser;
use llvm_test::typecheck::TypeChecker;

fn main() {
    let code = std::fs::read_to_string("./sources/std/externals.txt").unwrap();
    let mut lexer = Lexer::new(&code, Console::verbose());
    let mut parser = Parser::new(&mut lexer, Console::verbose());
    let mut ast = parser.parse().unwrap();
    let mut type_checker = TypeChecker::new(Console::verbose(), &lexer);
    type_checker.run(&mut ast, false).unwrap();

    println!("{:#?}", ast);
}
