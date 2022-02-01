use crate::ast::{AST, ASTPrimitive, BinOp, ExprAST, FunctionAST};
use crate::Console;
use crate::interpreter::ExprResult::Number;

pub struct Interpreter<'a> {
    functions: Vec<&'a FunctionAST>,
    main_function: Option<&'a FunctionAST>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            main_function: None,
        }
    }

    pub fn run(&mut self, ast: &'a AST, console: Console) {
        for node in ast {
            match node {
                ASTPrimitive::Extern(_) => {
                    unimplemented!("Extern functions are not yet implemented");
                }
                ASTPrimitive::Function(fun) => {
                    if fun.proto.name == "main" {
                        self.main_function = Some(fun);
                    } else {
                        self.functions.push(fun);
                    }
                }
            }
        }
        if let Some(main) = self.main_function {
            let result = self.eval_fn(main);
            console.force_println(format!("[Interpreter] Main function exited with: {:?}", result));
        } else {
            console.force_println("[Interpreter] Warning: Program does not contain main function.");
        }
    }

    fn eval_fn(&self, fun: &FunctionAST) -> ExprResult {
        self.eval_expr(&fun.body)
    }

    fn eval_expr(&self, expr: &ExprAST) -> ExprResult {
        match expr {
            ExprAST::Integer(value) => { ExprResult::Number(*value) }
            ExprAST::String(s) => { ExprResult::String(s.clone()) }
            ExprAST::BinaryExpr { op, lhs, rhs } => {
                match op {
                    BinOp::Add => {
                        self.eval_add(lhs, rhs)
                    }
                }
            }
            ExprAST::Sequence { lhs, rhs } => {
                self.eval_expr(lhs);
                self.eval_expr(rhs)
            }
            ExprAST::Nop => { ExprResult::Null }
        }
    }

    fn eval_add(&self, lhs: &ExprAST, rhs: &ExprAST) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs);
        let rhs_eval = self.eval_expr(rhs);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => {
                ExprResult::Number(a + b)
            },
            _ => {
                ExprResult::TypeMismatchError(format!("ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                BinOp::Add, lhs_eval, rhs_eval))
            },
        }
    }
}

#[derive(Debug)]
enum ExprResult {
    Number(i32),
    String(String),
    Null,
    TypeMismatchError(String),
}