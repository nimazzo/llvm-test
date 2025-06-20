use crate::ast::{ASTPrimitive, BinOp, ExprAST, ExprVariant, FunctionAST, AST};
use crate::console::Console;
use crate::core::CoreLib;
use crate::interpreter::ExprResult::Number;
use crate::util::{resolve_function_interpreter, INTERNAL_ERROR};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Interpreter {
    functions: HashMap<String, Vec<FunctionAST>>,
    main_function: Option<FunctionAST>,
    stdout: RefCell<String>,
    console: Console,
}

impl Interpreter {
    pub fn new(console: Console) -> Self {
        Self {
            functions: HashMap::new(),
            main_function: None,
            stdout: RefCell::new(String::new()),
            console,
        }
    }

    pub fn run(&mut self, ast: AST, core_lib: CoreLib) {
        self.define_internal_functions(core_lib);

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
            self.console
                .println("[Interpreter] Interpreting program...\n");
            let result = self.eval_expr(&main.body, &HashMap::new());

            self.stdout
                .borrow()
                .lines()
                .for_each(|line| self.console.force_println(format!("[Program] {}", line)));

            self.console.force_println(format!(
                "\n[Interpreter] Main function exited with: {:?}",
                result
            ));
        } else {
            self.console
                .force_println("\n[Interpreter] Warning: Program does not contain main function.");
        }
    }

    fn define_internal_functions(&mut self, core_lib: CoreLib) {
        let internal_functions = core_lib.get_core_functions();
        self.functions.extend(internal_functions.clone());
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
                "printf" => self.eval_print(args, old_local_variables),
                _ => panic!("unknown internal function"),
            }
        } else {
            let args = args
                .iter()
                .map(|expr| match self.eval_expr(expr, old_local_variables) {
                    Number(n) => ExprAST::new_integer(n, (0, 0), (0, 0)),
                    ExprResult::String(s) => ExprAST::new_string(s, (0, 0), (0, 0)),
                    _ => panic!("{}", INTERNAL_ERROR),
                })
                .collect::<Vec<_>>();

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

    fn eval_print(
        &self,
        args: &[ExprAST],
        local_variables: &HashMap<String, ExprAST>,
    ) -> ExprResult {
        let s = args.get(1).expect(INTERNAL_ERROR);
        let result = self.eval_expr(s, local_variables);
        match result {
            ExprResult::Number(n) => {
                self.stdout.borrow_mut().push_str(&n.to_string());
                self.stdout.borrow_mut().push('\n');
                ExprResult::Number(n.to_string().len() as i32 + 1)
            }
            ExprResult::String(s) => {
                self.stdout.borrow_mut().push_str(&s);
                self.stdout.borrow_mut().push('\n');
                ExprResult::Number(s.len() as i32 + 1)
            }
            _ => {
                panic!("{}", INTERNAL_ERROR);
            }
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
            _ => panic!("{}", INTERNAL_ERROR),
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
            _ => panic!("{}", INTERNAL_ERROR),
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
            _ => panic!("{}", INTERNAL_ERROR),
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
            _ => panic!("{}", INTERNAL_ERROR),
        }
    }
}

#[derive(Debug)]
enum ExprResult {
    Number(i32),
    String(String),
    Null,
}
