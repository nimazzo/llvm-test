// TODO: Bug with chained function calls and local variables
use crate::ast::{
    ASTPrimitive, BinOp, ExprAST, ExprType, ExprVariant, FunctionAST, PrototypeAST, AST,
};
use crate::core::InternalFunction;
use crate::interpreter::ExprResult::Number;
use crate::util::resolve_function_interpreter;
use crate::Console;
use std::collections::HashMap;

pub struct Interpreter {
    functions: HashMap<String, Vec<FunctionAST>>,
    main_function: Option<FunctionAST>,
    console: Console,
}

const INTERNAL_ERROR: &str = "[CRITICAL ERROR] Internal Compiler Error";

impl Interpreter {
    pub fn new(console: Console) -> Self {
        Self {
            functions: HashMap::new(),
            main_function: None,
            console,
        }
    }

    fn define_internal_functions(&mut self) {
        let functions = [
            InternalFunction::PrintString,
            InternalFunction::PrintInteger,
        ];
        for function in functions {
            match function {
                InternalFunction::PrintString => {
                    let proto = PrototypeAST::new(
                        "print".to_string(),
                        vec![("s".to_string(), ExprType::String)],
                        ExprType::Integer,
                        (0, 0),
                        (0, 0),
                    );
                    let body = ExprAST::new_function_call(
                        "print".to_string(),
                        vec![ExprAST::new_variable(
                            "s".to_string(),
                            Some(ExprType::String),
                            (0, 0),
                            (0, 0),
                        )],
                        Some(ExprType::Integer),
                        (0, 0),
                        (0, 0),
                    )
                    .set_internal();
                    self.functions
                        .entry("print".to_string())
                        .or_insert_with(Vec::new)
                        .push(FunctionAST::new(proto, body, (0, 0), (0, 0)));
                }
                InternalFunction::PrintInteger => {
                    let proto = PrototypeAST::new(
                        "print".to_string(),
                        vec![("n".to_string(), ExprType::Integer)],
                        ExprType::Integer,
                        (0, 0),
                        (0, 0),
                    );
                    let body = ExprAST::new_function_call(
                        "print".to_string(),
                        vec![ExprAST::new_variable(
                            "n".to_string(),
                            Some(ExprType::Integer),
                            (0, 0),
                            (0, 0),
                        )],
                        Some(ExprType::Integer),
                        (0, 0),
                        (0, 0),
                    )
                    .set_internal();
                    self.functions
                        .entry("print".to_string())
                        .or_insert_with(Vec::new)
                        .push(FunctionAST::new(proto, body, (0, 0), (0, 0)));
                }
            }
        }
    }

    pub fn run(&mut self, ast: AST) {
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
                        self.functions
                            .entry(fun.proto.name.clone())
                            .or_insert_with(Vec::new)
                            .push(fun);
                    }
                }
            }
        }
        if let Some(main) = self.main_function.take() {
            let result = self.eval_expr(&main.body, &HashMap::new());
            self.console.force_println(format!(
                "[Interpreter] Main function exited with: {:?}",
                result
            ));
        } else {
            self.console
                .force_println("[Interpreter] Warning: Program does not contain main function.");
        }
    }

    fn eval_print(
        &self,
        args: &[ExprAST],
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let s = args.get(0).expect(INTERNAL_ERROR);
        let result = self.eval_expr(s, local_variables);
        match result {
            ExprResult::Number(n) => {
                self.console.force_println(format!("[Interpreter] {}", n));
                ExprResult::Number(n.to_string().len() as i32)
            }
            ExprResult::String(s) => {
                self.console.force_println(format!("[Interpreter] {}", s));
                ExprResult::Number(s.len() as i32)
            }
            _ => {
                panic!("{}", INTERNAL_ERROR);
            }
        }
    }

    fn eval_function_call(
        &self,
        fn_name: &str,
        args: &[ExprAST],
        internal: bool,
        old_local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        if internal {
            match fn_name {
                "print" => self.eval_print(args, old_local_variables),
                _ => panic!("unknown internal function"),
            }
        } else {
            // let fun = self.functions.get(fn_name).expect(INTERNAL_ERROR);
            let arg_types = args
                .iter()
                .cloned()
                .map(|expr| expr.variant.type_of().expect(INTERNAL_ERROR))
                .collect::<Vec<_>>();
            let fun = resolve_function_interpreter(&self.functions, fn_name, &arg_types)
                .expect(INTERNAL_ERROR);

            let mut local_variables = HashMap::new();
            for (name, arg) in fun.proto.args.iter().map(|(a, _)| a).zip(args.iter()) {
                let arg = if let ExprVariant::Variable { ident, .. } = &arg.variant {
                    old_local_variables
                        .get(ident)
                        .expect(INTERNAL_ERROR)
                        .clone()
                } else {
                    arg.clone()
                };
                local_variables.insert(name.to_string(), arg);
            }

            let result = self.eval_expr(&fun.body, &local_variables);
            local_variables.clear();
            result
        }
    }

    fn eval_expr(&self, expr: &ExprAST, local_variables: &HashMap<String, ExprAST>) -> ExprResult {
        match &expr.variant {
            ExprVariant::Integer(value) => ExprResult::Number(*value),
            ExprVariant::String(s) => ExprResult::String(s.clone()),
            ExprVariant::Variable { ident, .. } => {
                let expr = local_variables.get(ident).expect(INTERNAL_ERROR);
                self.eval_expr(expr, local_variables)
            }
            ExprVariant::FunctionCall {
                fn_name,
                args,
                internal,
                ..
            } => self.eval_function_call(fn_name, args, *internal, local_variables),
            ExprVariant::BinaryExpr { op, lhs, rhs } => match op {
                BinOp::Add => self.eval_add(lhs, rhs, local_variables),
                BinOp::Minus => self.eval_minus(lhs, rhs, local_variables),
                BinOp::Mul => self.eval_mul(lhs, rhs, local_variables),
                BinOp::Div => self.eval_div(lhs, rhs, local_variables),
            },
            ExprVariant::Sequence { lhs, rhs } => {
                self.eval_expr(lhs, local_variables);
                self.eval_expr(rhs, local_variables)
            }
            ExprVariant::Nop => ExprResult::Null,
        }
    }

    fn eval_add(
        &self,
        lhs: &ExprAST,
        rhs: &ExprAST,
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => ExprResult::Number(a + b),
            _ => ExprResult::TypeMismatchError(format!(
                "ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                BinOp::Add,
                lhs_eval,
                rhs_eval
            )),
        }
    }

    fn eval_minus(
        &self,
        lhs: &ExprAST,
        rhs: &ExprAST,
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => ExprResult::Number(a - b),
            _ => ExprResult::TypeMismatchError(format!(
                "ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                BinOp::Minus,
                lhs_eval,
                rhs_eval
            )),
        }
    }

    fn eval_mul(
        &self,
        lhs: &ExprAST,
        rhs: &ExprAST,
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => ExprResult::Number(a * b),
            _ => ExprResult::TypeMismatchError(format!(
                "ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                BinOp::Mul,
                lhs_eval,
                rhs_eval
            )),
        }
    }

    fn eval_div(
        &self,
        lhs: &ExprAST,
        rhs: &ExprAST,
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let lhs_eval = self.eval_expr(lhs, local_variables);
        let rhs_eval = self.eval_expr(rhs, local_variables);

        match (&lhs_eval, &rhs_eval) {
            (Number(a), Number(b)) => ExprResult::Number(a / b),
            _ => ExprResult::TypeMismatchError(format!(
                "ERROR: Incompatible types: No {:?} implementation for lhs: {:?}, rhs: {:?}",
                BinOp::Div,
                lhs_eval,
                rhs_eval
            )),
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
