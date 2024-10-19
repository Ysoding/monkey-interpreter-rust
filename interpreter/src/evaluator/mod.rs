use std::{cell::RefCell, rc::Rc};

use environment::Environment;
use object::Object;

use crate::ast::{
    BlockStatement, CallExpression, Expression, FunctionLiteral, Identifier, IfExpresion,
    InfixExpresion, PrefixExpresion, Program, Statement,
};

mod environment;
mod object;

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    fn returned(&mut self, obj: Object) -> Object {
        match obj {
            Object::Return(v) => *v,
            o => o,
        }
    }

    pub fn eval_program(&mut self, prog: Program) -> Object {
        let ret = self.eval_statements(prog.statements);
        self.returned(ret)
    }

    fn eval_statement(&mut self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression(expr) => self.eval_expr(expr.expression.unwrap()),
            Statement::Return(ret_stmt) => {
                Object::Return(Box::new(self.eval_expr(ret_stmt.return_value.unwrap())))
            }
            Statement::Let(let_stmt) => {
                let obj = self.eval_expr(let_stmt.value.unwrap());
                if !obj.is_error() {
                    self.environment
                        .borrow_mut()
                        .set(&let_stmt.name.name, obj.clone());
                }
                obj
            }
            v => Object::Error(format!("unsupport statement {}", v)),
        }
    }

    fn eval_expr(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::Identifier(v) => self.eval_ident_expr(v),
            Expression::IntegerLiteral(v) => Object::Integer(v.value),
            Expression::Boolean(v) => Object::Boolean(v.value),
            Expression::Prefix(v) => self.eval_prefix_expr(v),
            Expression::Infix(v) => self.eval_infix_expr(v),
            Expression::If(v) => self.eval_if_expr(v),
            Expression::Function(v) => self.eval_fun_expr(v),
            Expression::Call(v) => self.eval_call_expr(v),
        }
    }

    fn eval_statements(&mut self, mut stmts: Vec<Statement>) -> Object {
        match stmts.len() {
            0 => Object::Null,
            1 => self.eval_statement(stmts.remove(0)),
            _ => {
                let stmt = stmts.remove(0);
                let obj = self.eval_statement(stmt);
                if obj.is_returned() || obj.is_error() {
                    obj
                } else {
                    self.eval_statements(stmts)
                }
            }
        }
    }

    fn eval_block_stmt(&mut self, bstmt: BlockStatement) -> Object {
        self.eval_statements(bstmt.statements)
    }

    fn eval_call_expr(&mut self, call_expr: CallExpression) -> Object {
        let fun = self.eval_expr(*call_expr.function);
        match fun {
            Object::Function(params, body, f_env) => {
                self.eval_fn_call(call_expr.arguments, params, body, &f_env)
            }
            v => v,
        }
    }

    fn eval_fn_call(
        &mut self,
        arg_exprs: Vec<Expression>,
        params: Vec<Identifier>,
        body: BlockStatement,
        f_env: &Rc<RefCell<Environment>>,
    ) -> Object {
        if arg_exprs.len() != params.len() {
            Object::Error(format!(
                "wrong number of arguments: {} expected but got {}",
                params.len(),
                arg_exprs.len()
            ))
        } else {
            let args = arg_exprs
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();
            let mut new_env = Environment::new_enclosed(Rc::clone(f_env));

            for (ident, obj) in params.into_iter().zip(args) {
                new_env.set(&ident.name, obj);
            }

            let old_env = Rc::clone(&self.environment);

            self.environment = Rc::new(RefCell::new(new_env));
            let obj = self.eval_block_stmt(body);
            self.environment = old_env;

            self.returned(obj)
        }
    }

    fn eval_fun_expr(&mut self, fun_expr: FunctionLiteral) -> Object {
        Object::Function(
            fun_expr.parameters,
            fun_expr.body,
            Rc::clone(&self.environment),
        )
    }

    fn eval_if_expr(&mut self, if_expr: IfExpresion) -> Object {
        let obj = self.eval_expr(*if_expr.condition);
        match self.otb(obj, "") {
            Ok(v) => {
                if v {
                    self.eval_block_stmt(if_expr.consequence)
                } else {
                    match if_expr.alternative {
                        Some(alt) => self.eval_block_stmt(alt),
                        None => Object::Null,
                    }
                }
            }
            Err(err) => err,
        }
    }

    fn otb(&mut self, object: Object, operator: &str) -> Result<bool, Object> {
        match object {
            Object::Boolean(i) => Ok(i),
            Object::Integer(v) => Ok(v != 0),
            Object::Error(v) => Err(Object::Error(v)),
            v => Err(Object::Error(format!(
                "unknown operator: {}{}",
                operator,
                v.get_type_name()
            ))),
        }
    }

    fn eval_infix_expr(&mut self, infix: InfixExpresion) -> Object {
        match (infix.left, infix.right) {
            (Some(left_expr), Some(right_expr)) => {
                let obj1 = self.eval_expr(*left_expr);
                let obj2 = self.eval_expr(*right_expr);
                match infix.operator.as_str() {
                    "+" => self.object_add(obj1, obj2),
                    "-" => {
                        let i1 = self.oti(obj1, "-");
                        let i2 = self.oti(obj2, "-");
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 - v2),
                            (Err(err), _) | (_, Err(err)) => err,
                        }
                    }
                    "*" => {
                        let i1 = self.oti(obj1, "*");
                        let i2 = self.oti(obj2, "*");
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 * v2),
                            (Err(err), _) | (_, Err(err)) => err,
                        }
                    }
                    "/" => {
                        let i1 = self.oti(obj1, "/");
                        let i2 = self.oti(obj2, "/");
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 / v2),
                            (Err(err), _) | (_, Err(err)) => err,
                        }
                    }
                    "<" => {
                        let i1 = self.oti(obj1, "<");
                        let i2 = self.oti(obj2, "<");
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Boolean(v1 < v2),
                            (Err(err), _) | (_, Err(err)) => err,
                        }
                    }
                    ">" => {
                        let i1 = self.oti(obj1, ">");
                        let i2 = self.oti(obj2, ">");
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Boolean(v1 > v2),
                            (Err(err), _) | (_, Err(err)) => err,
                        }
                    }
                    "==" => Object::Boolean(obj1 == obj2),
                    "!=" => Object::Boolean(obj1 != obj2),
                    _ => Object::Null,
                }
            }
            (_, _) => Object::Null,
        }
    }

    fn oti(&mut self, object: Object, operator: &str) -> Result<i64, Object> {
        match object {
            Object::Integer(i) => Ok(i),
            Object::Error(v) => Err(Object::Error(v)),
            v => Err(Object::Error(format!(
                "unknown operator: {}{}",
                operator,
                v.get_type_name()
            ))),
        }
    }

    fn object_add(&mut self, obj1: Object, obj2: Object) -> Object {
        match (obj1, obj2) {
            (Object::Integer(v1), Object::Integer(v2)) => Object::Integer(v1 + v2),
            (Object::Error(v), _) | (_, Object::Error(v)) => Object::Error(v),
            (x, y) => {
                let msg = if x.get_type_name().eq(y.get_type_name()) {
                    format!(
                        "unknown operator: {} + {}",
                        x.get_type_name(),
                        y.get_type_name()
                    )
                } else {
                    format!(
                        "type mismatch: {} + {}",
                        x.get_type_name(),
                        y.get_type_name()
                    )
                };
                Object::Error(msg)
            }
        }
    }

    fn eval_ident_expr(&mut self, ident: Identifier) -> Object {
        match self.environment.borrow().get(&ident.name) {
            Some(v) => v,
            None => Object::Error(format!("identifier not found: {}", ident.name)),
        }
    }

    fn eval_prefix_expr(&mut self, prefix: PrefixExpresion) -> Object {
        let object = self.eval_expr(*prefix.right.unwrap());
        match prefix.operator.as_str() {
            "!" => match self.otb(object, "!") {
                Ok(v) => Object::Boolean(!v),
                Err(err) => err,
            },
            "-" => match self.oti(object, "-") {
                Ok(v) => Object::Integer(-v),
                Err(err) => err,
            },
            "+" => match self.oti(object, "+") {
                Ok(v) => Object::Integer(v),
                Err(err) => err,
            },
            v => Object::Error(format!("{} unknow prefix operation", v)),
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Node, lexer::Lexer, parser::Parser};

    #[test]
    fn test_clousures() {
        let input = "
let newAddr = fn(x) {
    fn(y) {x + y};
};

let addTwo = newAddr(2);
addTwo(2);
        ";

        test_integer_object(test_eval(input), 4);
    }

    #[test]
    fn test_function_application() {
        let test_cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5);", 5),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluted = test_eval(input);
        match evaluted {
            Object::Function(parms, body, _) => {
                assert_eq!(1, parms.len());
                assert_eq!("x", parms.first().unwrap().name);
                assert_eq!("(x + 2)", body.as_string());
            }
            _ => panic!("unexpected object"),
        }
    }

    #[test]
    fn test_error_handling() {
        let test_cases = vec![
            ("5 + true;", "type mismatch: Integer + Boolean"),
            ("5 + true; 5;", "type mismatch: Integer + Boolean"),
            ("-true", "unknown operator: -Boolean"),
            ("true + false", "unknown operator: Boolean + Boolean"),
            ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean + Boolean",
            ),
            (
                "
            if (10 > 1) {
            	if (10 > 1) {
            		return true + false;
            	}

            	return 1;
            }
            ",
                "unknown operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
            // ("\"Hello\" - \"World\"", "unknown operator: String - String"),
            //     ("1.5 + \"World\"", "unknown operator: Float + String"),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Error(ref v) => assert_eq!(expected, v),
                _ => panic!("invalid expected"),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let test_cases = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in test_cases {
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_return_stmt() {
        struct TestCase {
            input: &'static str,
            expected: Object,
        }
        let cases = vec![
            TestCase {
                input: "return 10;",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "return 10; 9;",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "return 2 * 5; 9;",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "9; return 2 * 5; 9;",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
                ",
                expected: Object::Integer(10),
            },
        ];

        for case in cases {
            let evaluated = test_eval(case.input);
            assert_eq!(evaluated, case.expected)
        }
    }

    #[test]
    fn test_if_else_expression() {
        struct TestCase {
            input: &'static str,
            expected: Object,
        }
        let cases = vec![
            TestCase {
                input: "if (true) { 10 }",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "if (false) { 10 }",
                expected: Object::Null,
            },
            TestCase {
                input: "if (1) { 10 }",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "if (1 < 2) { 10 }",
                expected: Object::Integer(10),
            },
            TestCase {
                input: "if (1 > 2) { 10 }",
                expected: Object::Null,
            },
            TestCase {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Object::Integer(20),
            },
            TestCase {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Object::Integer(10),
            },
        ];

        for case in cases {
            let evaluated = test_eval(case.input);
            assert_eq!(evaluated, case.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        struct TestCase {
            input: &'static str,
            expected: bool,
        }
        let cases = vec![
            TestCase {
                input: "!true",
                expected: false,
            },
            TestCase {
                input: "!false",
                expected: true,
            },
            TestCase {
                input: "!5",
                expected: false,
            },
            TestCase {
                input: "!!true",
                expected: true,
            },
            TestCase {
                input: "!!false",
                expected: false,
            },
            TestCase {
                input: "!!5",
                expected: true,
            },
        ];

        for case in cases {
            let evaluated = test_eval(case.input);
            test_bool_object(evaluated, case.expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct TestCase {
            input: &'static str,
            expected: bool,
        }
        let cases = vec![
            TestCase {
                input: "true",
                expected: true,
            },
            TestCase {
                input: "false",
                expected: false,
            },
            TestCase {
                input: "true",
                expected: true,
            },
            TestCase {
                input: "false",
                expected: false,
            },
            TestCase {
                input: "1 < 2",
                expected: true,
            },
            TestCase {
                input: "1 > 2",
                expected: false,
            },
            TestCase {
                input: "1 < 1",
                expected: false,
            },
            TestCase {
                input: "1 > 1",
                expected: false,
            },
            TestCase {
                input: "1 == 1",
                expected: true,
            },
            TestCase {
                input: "1 != 1",
                expected: false,
            },
            TestCase {
                input: "1 == 2",
                expected: false,
            },
            TestCase {
                input: "1 != 2",
                expected: true,
            },
            TestCase {
                input: "true == true",
                expected: true,
            },
            TestCase {
                input: "false == false",
                expected: true,
            },
            TestCase {
                input: "true == false",
                expected: false,
            },
            TestCase {
                input: "true != false",
                expected: true,
            },
            TestCase {
                input: "false != true",
                expected: true,
            },
            TestCase {
                input: "(1 < 2) == true",
                expected: true,
            },
            TestCase {
                input: "(1 < 2) == false",
                expected: false,
            },
            TestCase {
                input: "(1 > 2) == true",
                expected: false,
            },
            TestCase {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for case in cases {
            let evaluated = test_eval(case.input);
            test_bool_object(evaluated, case.expected);
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        struct TestCase {
            input: &'static str,
            expected: i64,
        }
        let cases = vec![
            TestCase {
                input: "5",
                expected: 5,
            },
            TestCase {
                input: "10",
                expected: 10,
            },
            TestCase {
                input: "-5",
                expected: -5,
            },
            TestCase {
                input: "-10",
                expected: -10,
            },
            TestCase {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            TestCase {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            TestCase {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            TestCase {
                input: "5 * 2 + 10",
                expected: 20,
            },
            TestCase {
                input: "5 + 2 * 10",
                expected: 25,
            },
            TestCase {
                input: "20 + 2 * -10",
                expected: 0,
            },
            TestCase {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            TestCase {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            TestCase {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            TestCase {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            TestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];

        for case in cases {
            let evaluated = test_eval(case.input);
            test_integer_object(evaluated, case.expected);
        }
    }

    fn test_bool_object(evaluated: Object, expected: bool) {
        match evaluated {
            Object::Boolean(v) => assert_eq!(v, expected),
            v => panic!("invalid object {}, need Boolean", v),
        }
    }

    fn test_integer_object(evaluated: Object, expected: i64) {
        match evaluated {
            Object::Integer(v) => assert_eq!(v, expected),
            _ => panic!("invalid object {}, need Integer", evaluated),
        }
    }

    fn test_eval(input: &'static str) -> Object {
        let mut lex = Lexer::new(input.to_owned());
        let mut parser = Parser::new(&mut lex);
        let prog = parser.parse_program();
        let mut e = Evaluator::new();

        e.eval_program(prog)
    }
}
