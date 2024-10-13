use object::Object;

use crate::ast::{Expression, InfixExpresion, PrefixExpresion, Program, Statement};

pub mod object;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_program(&mut self, mut prog: Program) -> Object {
        match prog.statements.len() {
            0 => Object::Null,
            _ => {
                let stmt = prog.statements.remove(0);
                self.eval_statement(stmt)
            }
        }
    }

    fn eval_statement(&mut self, stmt: Statement) -> Object {
        match stmt {
            Statement::Expression(expr) => self.eval_expr(expr.expression.unwrap()),
            _ => panic!("error"),
        }
    }

    fn eval_expr(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::IntegerLiteral(v) => Object::Integer(v.value),
            Expression::Boolean(v) => Object::Boolean(v.value),
            Expression::Prefix(v) => self.eval_prefix_expr(v),
            Expression::Infix(v) => self.eval_infix_expr(v),
            _ => panic!("error"),
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
                        let i1 = self.oti(obj1);
                        let i2 = self.oti(obj2);
                        match (i1, i2) {
                            (Some(v1), Some(v2)) => Object::Integer(v1 - v2),
                            (_, _) => Object::Null,
                        }
                    }
                    "*" => {
                        let i1 = self.oti(obj1);
                        let i2 = self.oti(obj2);
                        match (i1, i2) {
                            (Some(v1), Some(v2)) => Object::Integer(v1 * v2),
                            (_, _) => Object::Null,
                        }
                    }
                    "/" => {
                        let i1 = self.oti(obj1);
                        let i2 = self.oti(obj2);
                        match (i1, i2) {
                            (Some(v1), Some(v2)) => Object::Integer(v1 / v2),
                            (_, _) => Object::Null,
                        }
                    }
                    "<" => Object::Boolean(obj1 < obj2),
                    ">" => Object::Boolean(obj1 > obj2),
                    "==" => Object::Boolean(obj1 == obj2),
                    "!=" => Object::Boolean(obj1 != obj2),
                    _ => Object::Null,
                }
            }
            (_, _) => Object::Null,
        }
    }

    fn oti(&mut self, object: Object) -> Option<i64> {
        match object {
            Object::Integer(i) => Some(i),
            _ => None,
        }
    }

    fn object_add(&mut self, obj1: Object, obj2: Object) -> Object {
        match (obj1, obj2) {
            (Object::Integer(v1), Object::Integer(v2)) => Object::Integer(v1 + v2),
            (_, _) => Object::Null,
        }
    }

    fn eval_prefix_expr(&mut self, prefix: PrefixExpresion) -> Object {
        let object = self.eval_expr(*prefix.right.unwrap());
        match prefix.operator.as_str() {
            "!" => match object {
                Object::Integer(v) => Object::Boolean(v == 0),
                Object::Boolean(v) => Object::Boolean(!v),
                Object::Null => Object::Boolean(true),
            },
            "-" => match object {
                Object::Integer(v) => Object::Integer(-v),
                _ => Object::Null,
            },
            _ => panic!("error"),
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
    use crate::{lexer::Lexer, parser::Parser};

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
            _ => panic!("invalid object, need Boolean"),
        }
    }

    fn test_integer_object(evaluated: Object, expected: i64) {
        match evaluated {
            Object::Integer(v) => assert_eq!(v, expected),
            _ => panic!("invalid object, need Integer"),
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
