use std::{cell::RefCell, collections::HashMap, rc::Rc};

use environment::Environment;
use object::{BuiltinFunction, Object};

use crate::ast::{
    ArrayLiteral, BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
    HashLiteral, Identifier, IfExpresion, IndexExpression, InfixExpresion, Node, PrefixExpresion,
    Program, Statement, StringLiteral,
};

mod builtins;
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
            Expression::Integer(v) => Object::Integer(v.value),
            Expression::Boolean(v) => Object::Boolean(v.value),
            Expression::Prefix(v) => self.eval_prefix_expr(v),
            Expression::Infix(v) => self.eval_infix_expr(v),
            Expression::If(v) => self.eval_if_expr(v),
            Expression::Function(v) => self.eval_fun_expr(v),
            Expression::Call(v) => self.eval_call_expr(v),
            Expression::String(v) => self.eval_string_literal_expr(v),
            Expression::Array(v) => self.eval_array_literal_expr(v),
            Expression::Index(v) => self.eval_index_expr(v),
            Expression::Hash(v) => self.eval_hash_literal_expr(v),
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

    fn eval_hash_literal_expr(&mut self, expr: HashLiteral) -> Object {
        let mut map = HashMap::new();
        for (key, val) in expr.pairs.into_iter() {
            let k = self.eval_expr(key);
            if k.is_error() {
                return k;
            }

            let hk = self.oth(k);
            if hk.is_error() {
                return hk;
            }

            let v = self.eval_expr(val);
            if v.is_error() {
                return v;
            }

            map.insert(hk, v);
        }
        Object::Hash(map)
    }

    fn oth(&mut self, obj: Object) -> Object {
        match obj {
            Object::Integer(i) => Object::Integer(i),
            Object::Boolean(b) => Object::Boolean(b),
            Object::String(s) => Object::String(s),
            Object::Error(s) => Object::Error(s),
            x => Object::Error(format!("unusable as hash key: {}", x)),
        }
    }

    fn eval_index_expr(&mut self, expr: IndexExpression) -> Object {
        let left = self.eval_expr(*expr.left);
        let index = self.eval_expr(*expr.index);
        match left {
            Object::Array(arr) => match self.oti(&index) {
                Ok(idx) => arr.into_iter().nth(idx as usize).unwrap_or(Object::Null),
                Err(err) => err,
            },
            Object::Hash(mut hash) => {
                let k = self.oth(index);
                match k {
                    Object::Error(_) => k,
                    _ => hash.remove(&k).unwrap_or(Object::Null),
                }
            }
            o => Object::Error(format!("index operator not supported: {}", o)),
        }
    }

    fn eval_array_literal_expr(&mut self, expr: ArrayLiteral) -> Object {
        let v: Vec<Object> = expr
            .elements
            .into_iter()
            .map(|e| self.eval_expr(e))
            .collect();

        Object::Array(v)
    }

    fn eval_string_literal_expr(&mut self, expr: StringLiteral) -> Object {
        Object::String(expr.value)
    }

    fn quote(&mut self, expr: Expression) -> Object {
        Object::Quote(expr)
    }

    fn eval_call_expr(&mut self, call_expr: CallExpression) -> Object {
        if call_expr.function.token_literal() == "quote" {
            return self.quote(call_expr.arguments[0].clone());
        }

        let fun = self.eval_expr(*call_expr.function);
        match fun {
            Object::Function(params, body, f_env) => {
                self.eval_fn_call(call_expr.arguments, params, body, &f_env)
            }
            Object::Builtin(_, params_len, func) => {
                self.eval_builtin_fn(call_expr.arguments, params_len, func)
            }
            v => v,
        }
    }
    fn eval_builtin_fn(
        &mut self,
        arg_exprs: Vec<Expression>,
        params_len: usize,
        bfn: BuiltinFunction,
    ) -> Object {
        if arg_exprs.len() != params_len {
            Object::Error(format!(
                "wrong number of arguments: {} expected but got {}",
                params_len,
                arg_exprs.len()
            ))
        } else {
            let args = arg_exprs
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();
            bfn(args)
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
        match self.otb(&obj) {
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

    fn otb(&mut self, object: &Object) -> Result<bool, Object> {
        match object {
            Object::Boolean(i) => Ok(*i),
            Object::Integer(v) => Ok(*v != 0),
            Object::Error(v) => Err(Object::Error(v.clone())),
            v => Err(Object::Error(format!("{} is not a bool", v))),
        }
    }

    fn eval_infix_expr(&mut self, infix: InfixExpresion) -> Object {
        let get_error_info = |operator: String, obj1: Object, obj2: Object| -> Object {
            if obj1.get_type_name() != obj2.get_type_name() {
                Object::Error(format!(
                    "type mismatch: {} {} {}",
                    obj1.get_type_name(),
                    operator,
                    obj2.get_type_name()
                ))
            } else {
                Object::Error(format!(
                    "unknown operator: {} {} {}",
                    obj1.get_type_name(),
                    operator,
                    obj2.get_type_name(),
                ))
            }
        };

        match (infix.left, infix.right) {
            (Some(left_expr), Some(right_expr)) => {
                let obj1 = self.eval_expr(*left_expr);
                let obj2 = self.eval_expr(*right_expr);
                let operator = infix.operator;
                match operator.as_str() {
                    "+" => self.object_add(obj1, obj2),
                    "-" => {
                        let i1 = self.oti(&obj1);
                        let i2 = self.oti(&obj2);
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 - v2),
                            (Err(_), _) | (_, Err(_)) => get_error_info(operator, obj1, obj2),
                        }
                    }
                    "*" => {
                        let i1 = self.oti(&obj1);
                        let i2 = self.oti(&obj2);
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 * v2),
                            (Err(_), _) | (_, Err(_)) => get_error_info(operator, obj1, obj2),
                        }
                    }
                    "/" => {
                        let i1 = self.oti(&obj1);
                        let i2 = self.oti(&obj2);
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Integer(v1 / v2),
                            (Err(_), _) | (_, Err(_)) => get_error_info(operator, obj1, obj2),
                        }
                    }
                    "<" => {
                        let i1 = self.oti(&obj1);
                        let i2 = self.oti(&obj2);
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Boolean(v1 < v2),
                            (Err(_), _) | (_, Err(_)) => get_error_info(operator, obj1, obj2),
                        }
                    }
                    ">" => {
                        let i1 = self.oti(&obj1);
                        let i2 = self.oti(&obj2);
                        match (i1, i2) {
                            (Ok(v1), Ok(v2)) => Object::Boolean(v1 > v2),
                            (Err(_), _) | (_, Err(_)) => get_error_info(operator, obj1, obj2),
                        }
                    }
                    "==" => Object::Boolean(obj1 == obj2),
                    "!=" => Object::Boolean(obj1 != obj2),
                    _ => Object::Error(format!(
                        "unknown operator: {} {} {}",
                        obj1.get_type_name(),
                        operator,
                        obj2.get_type_name(),
                    )),
                }
            }
            (_, _) => Object::Null,
        }
    }

    fn oti(&mut self, object: &Object) -> Result<i64, Object> {
        match object {
            Object::Integer(i) => Ok(*i),
            Object::Error(v) => Err(Object::Error(v.to_string())),
            v => Err(Object::Error(format!("{} is not an integer", v))),
        }
    }

    fn object_add(&mut self, obj1: Object, obj2: Object) -> Object {
        match (obj1, obj2) {
            (Object::Integer(v1), Object::Integer(v2)) => Object::Integer(v1 + v2),
            (Object::String(v1), Object::String(v2)) => Object::String(format!("{}{}", v1, v2)),
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

        let get_error_info = |obj: &Object| -> Object {
            Object::Error(format!("unknown operator: -{}", obj.get_type_name()))
        };

        match prefix.operator.as_str() {
            "!" => match self.otb(&object) {
                Ok(v) => Object::Boolean(!v),
                Err(_) => get_error_info(&object),
            },
            "-" => match self.oti(&object) {
                Ok(v) => Object::Integer(-v),
                Err(_) => get_error_info(&object),
            },
            "+" => match self.oti(&object) {
                Ok(v) => Object::Integer(v),
                Err(_) => get_error_info(&object),
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

type ModifierFunc = fn(Box<dyn Node>) -> Box<dyn Node>;

fn modify(node: Box<dyn Node>, modifier: ModifierFunc) -> Box<dyn Node> {
    // if let Some(mut program) = node.as_any().downcast_ref::<Program>() {
    //     for statement in &mut program.statements {
    //         *statement = modify(Box::new(statement.clone()), modifier)
    //             .as_any()
    //             .downcast_ref::<Statement>()
    //             .unwrap()
    //             .clone();
    //     }
    //     return modifier(Box::new(*program));
    // }

    // if let Some(mut expr_stmt) = node.as_any().downcast_ref::<ExpressionStatement>().cloned() {
    //     expr_stmt.expression = Some(
    //         modify(expr_stmt.expression.unwrap(), modifier)
    //             .as_any()
    //             .downcast_ref::<Expression>()
    //             .unwrap()
    //             .clone(),
    //     );
    //     return modifier(Box::new(expr_stmt));
    // }

    modifier(node)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{
        ast::{ExpressionStatement, IntegerLiteral, Node},
        lexer::{
            token::{Token, TokenType},
            Lexer,
        },
        parser::Parser,
    };

    #[test]
    fn test_modify() {
        let one = || -> Expression {
            Expression::Integer(IntegerLiteral {
                token: Token {
                    typ: TokenType::Int,
                    literal: "1".to_string(),
                },
                value: 1,
            })
        };
        let two = || -> Expression {
            Expression::Integer(IntegerLiteral {
                token: Token {
                    typ: TokenType::Int,
                    literal: "2".to_string(),
                },
                value: 2,
            })
        };

        let tests: Vec<(Box<dyn Node>, Box<dyn Node>)> = vec![
            (Box::new(one()), Box::new(two())),
            (
                Box::new(Program {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token {
                            typ: TokenType::Int,
                            literal: "1".to_string(),
                        },
                        expression: Some(one()),
                    })],
                }),
                Box::new(Program {
                    statements: vec![Statement::Expression(ExpressionStatement {
                        token: Token {
                            typ: TokenType::Int,
                            literal: "2".to_string(),
                        },
                        expression: Some(two()),
                    })],
                }),
            ),
        ];

        let turn_one_into_two = |node: Box<dyn Node>| -> Box<dyn Node> {
            if let Some(integer) = node.as_any().downcast_ref::<IntegerLiteral>() {
                if integer.value == 1 {
                    return Box::new(IntegerLiteral {
                        value: 2,
                        token: Token {
                            typ: TokenType::Int,
                            literal: "2".to_string(),
                        },
                    });
                }
            }
            node
        };

        for (input, expected) in tests {
            let modified = modify(input, turn_one_into_two);
            // assert_eq!(modified, expected);
        }
    }

    #[test]
    fn test_quote_unquote() {
        let test_cases = vec![
            ("quote(unquote(4))", "4".to_string()),
            ("quote(unquote(4 + 4))", "8".to_string()),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)".to_string()),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)".to_string()),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Quote(obj) => {
                    assert_eq!(obj.as_string(), expected);
                }
                _ => panic!("unexpected object"),
            }
        }
    }

    #[test]
    fn test_quote() {
        let test_cases = vec![
            ("quote(5)", "5".to_string()),
            ("quote(5 + 8)", "(5 + 8)".to_string()),
            ("quote(foobar)", "foobar".to_string()),
            ("quote(foobar + barfoo)", "(foobar + barfoo)".to_string()),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Quote(obj) => {
                    assert_eq!(obj.as_string(), expected);
                }
                _ => panic!("unexpected object"),
            }
        }
    }

    #[test]
    fn test_hash_index_expression() {
        let test_cases = vec![
            (r#"{"foo": 5}["foo"]"#, Object::Integer(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Object::Integer(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, Object::Integer(5)),
            (r#"{true: 5}[true]"#, Object::Integer(5)),
            (r#"{false: 5}[false]"#, Object::Integer(5)),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
let two = "two";
{
"one": 10 - 9,
two: 1 + 1,
"thr" + "ee": 6 / 2,
4: 4,
true: 5,
false: 6
}
        "#;
        let evaluated = test_eval(input);

        let mut expected: HashMap<Object, i64> = HashMap::new();
        expected.insert(Object::String("one".to_string()), 1);
        expected.insert(Object::String("two".to_string()), 2);
        expected.insert(Object::String("three".to_string()), 3);
        expected.insert(Object::Integer(4), 4);
        expected.insert(Object::Boolean(true), 5);
        expected.insert(Object::Boolean(false), 6);

        match evaluated {
            Object::Hash(v) => {
                assert_eq!(v.len(), expected.len());

                for (key, val) in v.iter() {
                    test_integer_object(val, *expected.get(key).unwrap());
                }
            }
            _ => panic!("unexpected Object {}", evaluated.get_type_name()),
        }
    }

    #[test]
    fn test_array_index_expression() {
        let test_cases = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        match evaluated {
            Object::Array(v) => {
                assert_eq!(v.len(), 3);
                test_integer_object(&v[0], 1);
                test_integer_object(&v[1], 4);
                test_integer_object(&v[2], 6);
            }
            _ => panic!("unexpected Object {}", evaluated.get_type_name()),
        }
    }

    #[test]
    fn test_builtin_functions() {
        let test_cases = vec![
            // len for strings
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (
                r#"len(1)"#,
                Object::Error("argument to `len` not supported, got Integer".to_string()),
            ),
            (
                r#"len("one", "two")"#,
                Object::Error("wrong number of arguments: 1 expected but got 2".to_string()),
            ),
            // len for arrays
            ("len([])", Object::Integer(0)),
            ("len([1])", Object::Integer(1)),
            ("len([1, 1 + 2 * 3, true])", Object::Integer(3)),
            // first for arrays
            ("first([])", Object::Null),
            ("first([1])", Object::Integer(1)),
            ("first([1, 2])", Object::Integer(1)),
            (
                "first(1)",
                Object::Error("argument to `first` must be Array, got Integer".to_string()),
            ),
            // last for arrays
            ("last([])", Object::Null),
            ("last([1])", Object::Integer(1)),
            ("last([1, 2])", Object::Integer(2)),
            (
                "last(1)",
                Object::Error("argument to `last` must be Array, got Integer".to_string()),
            ),
            // rest for arrays
            ("rest([])", Object::Null),
            ("rest([1])", Object::Array(vec![])),
            (
                "rest([1, 2, 3])",
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            (
                "rest(1)",
                Object::Error("argument to `rest` must be Array, got Integer".to_string()),
            ),
            // push for arrays
            ("push([], 1)", Object::Array(vec![Object::Integer(1)])),
            (
                "push([1, 2], 3)",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                "push([])",
                Object::Error("wrong number of arguments: 2 expected but got 1".to_string()),
            ),
            (
                "push(1, 2)",
                Object::Error("first argument to `push` must be Array, got Integer".to_string()),
            ),
        ];

        for (input, expected) in test_cases {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;

        let evaluted = test_eval(input);
        match evaluted {
            Object::String(ref v) => assert_eq!(v, "Hello World!"),
            _ => panic!("unexpected Object"),
        }
    }

    #[test]
    fn test_string_literal() {
        let input = r#""hello world!""#;

        let evaluted = test_eval(input);
        match evaluted {
            Object::String(ref v) => assert_eq!(v, "hello world!"),
            _ => panic!("unexpected Object"),
        }
    }

    #[test]
    fn test_clousures() {
        let input = "
let newAddr = fn(x) {
    fn(y) {x + y};
};

let addTwo = newAddr(2);
addTwo(2);
        ";

        test_integer_object(&test_eval(input), 4);
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
            test_integer_object(&evaluated, expected);
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
            (r#""Hello" - "World""#, "unknown operator: String - String"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: [function]",
            ),
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
            test_integer_object(&test_eval(input), expected);
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
            test_integer_object(&evaluated, case.expected);
        }
    }

    fn test_bool_object(evaluated: Object, expected: bool) {
        match evaluated {
            Object::Boolean(v) => assert_eq!(v, expected),
            v => panic!("invalid object {}, need Boolean", v),
        }
    }

    fn test_integer_object(evaluated: &Object, expected: i64) {
        match evaluated {
            Object::Integer(v) => assert_eq!(*v, expected),
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
