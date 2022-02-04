use std::collections::HashMap;
use crate::ast::{AST, ASTPrimitive, BinOp, ExprAST, ExprType, FunctionAST, PrototypeAST};
use crate::Console;
use crate::core::InternalFunction;
use crate::interpreter::ExprResult::Number;

pub struct Interpreter {
    functions: HashMap<String, FunctionAST>,
    main_function: Option<FunctionAST>,
}

const INTERNAL_ERROR: &str = "[CRITICAL ERROR] Internal Compiler Error";

impl Interpreter {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            main_function: None,
        }
    }

    fn define_internal_functions(&mut self) {
        let functions = [InternalFunction::PrintString];
        for function in functions {
            match function {
                InternalFunction::PrintString => {
                    let proto = PrototypeAST::new("print".to_string(), vec![("s".to_string(), ExprType::String)], ExprType::Integer);
                    let body = ExprAST::new_function_call("print".to_string(), vec![ExprAST::new_variable("s".to_string(), Some(ExprType::String))], Some(ExprType::Integer)).set_internal();
                    self.functions.insert("print".to_string(), FunctionAST::new(proto, body));
                }
            }
        }
    }

    pub fn run(&mut self, ast: AST, console: Console) {
        self.define_internal_functions();

        for node in ast {
            match node {
                ASTPrimitive::Extern(_) => {
                    unimplemented!("Extern functions are not yet implemented");
                }
                ASTPrimitive::Function(fun) => {
                    if fun.proto.name == "main" {
                        self.main_function = Some(fun);
                    } else {
                        self.functions.insert(fun.proto.name.clone(), fun);
                    }
                }
            }
        }
        if let Some(main) = self.main_function.take() {
            let result = self.eval_expr(&main.body, &HashMap::new());
            console.force_println(format!("[Interpreter] Main function exited with: {:?}", result));
        } else {
            console.force_println("[Interpreter] Warning: Program does not contain main function.");
        }
    }

    fn eval_print(&self, args: &[ExprAST], local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        let s = args.get(0).expect(INTERNAL_ERROR);
        let result = self.eval_expr(s, local_variables);
        if let ExprResult::String(s) = result {
            println!("[Interpreter] {}", s);
            return ExprResult::Number(s.len() as i32);
        }
        panic!("{}", INTERNAL_ERROR);
    }

    fn eval_function_call(&self, fn_name: &str, args: &[ExprAST], internal: bool, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        if internal {
            match fn_name {
                "print" => self.eval_print(args, local_variables),
                _ => panic!("unknown internal function"),
            }
        } else {
            let fun = self.functions.get(fn_name).expect(INTERNAL_ERROR);

            let mut local_variables = HashMap::new();
            for (name, arg) in fun.proto.args.iter().map(|(a, _)| a).zip(args.iter()) {
                local_variables.insert(name.to_string(), arg.clone());
            }

            let result = self.eval_expr(&fun.body, &local_variables);
            local_variables.clear();
            result
        }
    }

    fn eval_expr(&self, expr: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        match expr {
            ExprAST::Integer(value) => { ExprResult::Number(*value) }
            ExprAST::String(s) => { ExprResult::String(s.clone()) }
            ExprAST::Variable { ident, .. } => {
                let expr = local_variables.get(ident).expect(INTERNAL_ERROR);
                self.eval_expr(expr, local_variables)
            }
            ExprAST::FunctionCall { fn_name, args, internal, .. } => {
                self.eval_function_call(fn_name, args, *internal, local_variables)
            }
            ExprAST::BinaryExpr { op, lhs, rhs } => {
                match op {
                    BinOp::Add => self.eval_add(lhs, rhs, local_variables),
                    BinOp::Minus => self.eval_minus(lhs, rhs, local_variables),
                    BinOp::Mul => self.eval_mul(lhs, rhs, local_variables),
                    BinOp::Div => self.eval_div(lhs, rhs, local_variables),
                }
            }
            ExprAST::Sequence { lhs, rhs } => {
                self.eval_expr(lhs, local_variables);
                self.eval_expr(rhs, local_variables)
            }
            ExprAST::Nop => { ExprResult::Null }
        }
    }

    fn eval_add(&self, lhs: &ExprAST, rhs: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

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

    fn eval_minus(&self, lhs: &ExprAST, rhs: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => {
                ExprResult::Number(a - b)
            },
            _ => {
                ExprResult::TypeMismatchError(format!("ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                                                      BinOp::Minus, lhs_eval, rhs_eval))
            },
        }
    }

    fn eval_mul(&self, lhs: &ExprAST, rhs: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => {
                ExprResult::Number(a * b)
            },
            _ => {
                ExprResult::TypeMismatchError(format!("ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                                                      BinOp::Mul, lhs_eval, rhs_eval))
            },
        }
    }

    fn eval_div(&self, lhs: &ExprAST, rhs: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => {
                ExprResult::Number(a / b)
            },
            _ => {
                ExprResult::TypeMismatchError(format!("ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                                                      BinOp::Div, lhs_eval, rhs_eval))
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