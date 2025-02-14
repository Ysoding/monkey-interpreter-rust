use core::panic;
use std::collections::HashMap;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, BooleanExpression, CallExpression, Expression,
        ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpresion,
        IndexExpression, InfixExpresion, IntegerLiteral, LetStatement, PrefixExpresion, Program,
        ReturnStatement, Statement, StringLiteral,
    },
    lexer::{
        token::{Token, TokenType},
        Lexer,
    },
};

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

pub struct Parser<'a> {
    l: &'a mut Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.register_prefix(TokenType::Identifier, Parser::parse_identifier);
        p.register_prefix(TokenType::Int, Parser::parse_integer_literal);
        p.register_prefix(TokenType::Bang, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::Minus, Parser::parse_prefix_expression);
        p.register_prefix(TokenType::True, Parser::parse_boolean);
        p.register_prefix(TokenType::False, Parser::parse_boolean);
        p.register_prefix(TokenType::LParen, Parser::parse_grouped_expression);
        p.register_prefix(TokenType::If, Parser::parse_if_expression);
        p.register_prefix(TokenType::Function, Parser::parse_function_literal);
        p.register_prefix(TokenType::String, Parser::parse_string_literal);
        p.register_prefix(TokenType::LBracket, Parser::parse_array_literal);
        p.register_prefix(TokenType::LBrace, Parser::parse_hash_literal);

        p.register_infix(TokenType::Plus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Minus, Parser::parse_infix_expression);
        p.register_infix(TokenType::Slash, Parser::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Parser::parse_infix_expression);
        p.register_infix(TokenType::Eq, Parser::parse_infix_expression);
        p.register_infix(TokenType::Neq, Parser::parse_infix_expression);
        p.register_infix(TokenType::Lt, Parser::parse_infix_expression);
        p.register_infix(TokenType::Gt, Parser::parse_infix_expression);
        p.register_infix(TokenType::LParen, Parser::parse_call_expression);
        p.register_infix(TokenType::LBracket, Parser::parse_index_expression);

        p.next_token();
        p.next_token();
        p
    }

    fn parse_hash_literal(p: &mut Parser) -> Option<Expression> {
        let mut res = HashLiteral {
            token: p.cur_token.clone(),
            pairs: vec![],
        };

        while !p.peek_token_is(&TokenType::RBrace) {
            p.next_token();
            let key = p.parse_expression(Precedence::Lowest)?;

            if !p.expect_peek(TokenType::COLON) {
                return None;
            }
            p.next_token();
            let value = p.parse_expression(Precedence::Lowest)?;
            res.pairs.push((key, value));

            if !p.peek_token_is(&TokenType::RBrace) && !p.expect_peek(TokenType::Comma) {
                return None;
            }
        }

        if !p.expect_peek(TokenType::RBrace) {
            return None;
        }

        Some(Expression::Hash(res))
    }

    fn parse_array_literal(p: &mut Parser) -> Option<Expression> {
        Some(Expression::Array(ArrayLiteral {
            token: p.cur_token.clone(),
            elements: p.parse_expression_list(TokenType::RBracket),
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<Expression> {
        let mut eles = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return eles;
        }

        self.next_token();
        if let Some(v) = self.parse_expression(Precedence::Lowest) {
            eles.push(v);
        }

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            if let Some(v) = self.parse_expression(Precedence::Lowest) {
                eles.push(v);
            }
        }

        if !self.expect_peek(end) {
            return vec![];
        }

        eles
    }

    fn parse_string_literal(p: &mut Parser) -> Option<Expression> {
        Some(Expression::String(StringLiteral {
            token: p.cur_token.clone(),
            value: p.cur_token.literal.clone(),
        }))
    }

    fn parse_function_literal(p: &mut Parser) -> Option<Expression> {
        let token = p.cur_token.clone();

        if !p.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = p.parse_function_parameters();

        if !p.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = p.parse_block_statement();

        Some(Expression::Function(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut ret: Vec<Identifier> = Vec::new();

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return ret;
        }
        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };
        ret.push(ident);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Identifier {
                token: self.cur_token.clone(),
                name: self.cur_token.literal.clone(),
            };
            ret.push(ident);
        }

        if !self.expect_peek(TokenType::RParen) {
            panic!("expected `)`");
        }

        ret
    }

    fn parse_if_expression(p: &mut Parser) -> Option<Expression> {
        let token = p.cur_token.clone();

        if !p.expect_peek(TokenType::LParen) {
            return None;
        }
        p.next_token();

        let condition = Box::new(p.parse_expression(Precedence::Lowest).unwrap());

        if !p.expect_peek(TokenType::RParen) {
            return None;
        }

        if !p.expect_peek(TokenType::LBrace) {
            return None;
        }

        let mut ret = IfExpresion {
            token,
            condition,
            consequence: p.parse_block_statement(),
            alternative: None,
        };

        if p.peek_token_is(&TokenType::Else) {
            p.next_token();

            if !p.expect_peek(TokenType::LBrace) {
                return None;
            }
            ret.alternative = Some(p.parse_block_statement());
        }

        Some(Expression::If(ret))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut ret = BlockStatement {
            token: self.cur_token.clone(),
            statements: Vec::new(),
        };

        self.next_token();

        while !self.cur_token_is(&TokenType::RBrace) && !self.cur_token_is(&TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                ret.statements.push(stmt);
            }
            self.next_token();
        }
        ret
    }

    fn parse_grouped_expression(p: &mut Parser) -> Option<Expression> {
        p.next_token();
        let exp = p.parse_expression(Precedence::Lowest);
        if !p.expect_peek(TokenType::RParen) {
            return None;
        }
        exp
    }

    fn parse_boolean(p: &mut Parser) -> Option<Expression> {
        let ret = BooleanExpression {
            token: p.cur_token.clone(),
            value: p.cur_token_is(&TokenType::True),
        };

        Some(Expression::Boolean(ret))
    }

    fn parse_index_expression(p: &mut Parser, left: Expression) -> Option<Expression> {
        let token = p.cur_token.clone();

        p.next_token();
        let index = p.parse_expression(Precedence::Lowest)?;

        if !p.expect_peek(TokenType::RBracket) {
            return None;
        }

        Some(Expression::Index(IndexExpression {
            token,
            index: Box::new(index),
            left: Box::new(left),
        }))
    }

    fn parse_call_expression(p: &mut Parser, function: Expression) -> Option<Expression> {
        let token = p.cur_token.clone();
        let arguments = p.parse_expression_list(TokenType::RParen);
        Some(Expression::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }))
    }

    fn parse_infix_expression(p: &mut Parser, left: Expression) -> Option<Expression> {
        let mut ret = InfixExpresion {
            token: p.cur_token.clone(),
            left: Some(Box::new(left)),
            operator: p.cur_token.literal.clone(),
            right: None,
        };

        let precedence = p.cur_precedence();
        p.next_token();
        if let Some(right_expr) = p.parse_expression(precedence) {
            ret.right = Some(Box::new(right_expr));
        }

        Some(Expression::Infix(ret))
    }

    fn parse_prefix_expression(p: &mut Parser) -> Option<Expression> {
        let mut ret = PrefixExpresion {
            token: p.cur_token.clone(),
            operator: p.cur_token.literal.clone(),
            right: None,
        };
        p.next_token();

        if let Some(right_expr) = p.parse_expression(Precedence::Prefix) {
            ret.right = Some(Box::new(right_expr));
        }

        Some(Expression::Prefix(ret))
    }

    fn parse_integer_literal(p: &mut Parser) -> Option<Expression> {
        let v = p.cur_token.literal.parse().unwrap();
        let ret = IntegerLiteral {
            token: p.cur_token.clone(),
            value: v,
        };

        Some(Expression::Integer(ret))
    }

    fn parse_identifier(p: &mut Parser) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            token: p.cur_token.clone(),
            name: p.cur_token.literal.clone(),
        }))
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        let eof_tok = TokenType::Eof;
        while !self.cur_token_is(&eof_tok) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token()
        }

        program
    }

    fn cur_precedence(&mut self) -> Precedence {
        self.get_precedence(&self.cur_token.typ)
    }

    fn peek_precedence(&mut self) -> Precedence {
        self.get_precedence(&self.peek_token.typ)
    }

    fn get_precedence(&self, typ: &TokenType) -> Precedence {
        match typ {
            TokenType::Eq => Precedence::Equals,
            TokenType::Neq => Precedence::Equals,
            TokenType::Lt => Precedence::LessGreater,
            TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Asterisk => Precedence::Product,
            TokenType::Slash => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            TokenType::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.typ {
            TokenType::Let => Some(Statement::Let(self.parse_let_statement()?)),
            TokenType::Return => Some(Statement::Return(self.parse_ret_statement()?)),
            _ => Some(Statement::Expression(self.parse_expression_statement()?)),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let ret = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(ret)
    }

    fn no_prefix_parse_fn_error(&mut self, t: &TokenType) {
        self.errors
            .push(format!("no prefix parse function for {:?} found", t));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let cur_token_typ = self.cur_token.typ.clone();
        let prefix = match self.prefix_parse_fns.get(&cur_token_typ) {
            Some(prefix_fn) => prefix_fn,
            None => {
                self.no_prefix_parse_fn_error(&cur_token_typ);
                return None;
            }
        };
        let mut left_exp = prefix(self)?;

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            let typ = self.peek_token.typ.clone();

            let infix = match self.infix_parse_fns.get(&typ) {
                Some(infix_fn) => *infix_fn, // Dereference to copy the function pointer
                None => return Some(left_exp),
            };

            self.next_token();
            left_exp = infix(self, left_exp)?;
        }

        Some(left_exp)
    }

    fn parse_ret_statement(&mut self) -> Option<ReturnStatement> {
        let token = self.cur_token.clone();
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        let sem_tok = TokenType::Semicolon;
        while self.peek_token_is(&sem_tok) {
            self.next_token();
        }

        Some(ReturnStatement {
            token,
            return_value,
        })
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            name: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        let sem_tok = TokenType::Semicolon;
        while self.peek_token_is(&sem_tok) {
            self.next_token();
        }

        Some(LetStatement { token, name, value })
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.typ == *t
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.cur_token.typ == *t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(&t) {
            self.next_token();
            true
        } else {
            self.peek_error(&t);
            false
        }
    }

    fn peek_error(&mut self, t: &TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.typ
        );
        self.errors.push(msg);
    }

    pub fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    pub fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use core::panic;
    use std::{any::Any, fmt::Debug};

    #[test]
    fn test_hash_literal_empty() {
        let input = r#"{}"#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::Hash(hash_expr) => {
                        assert_eq!(0, hash_expr.pairs.len());
                    }
                    _ => panic!("unexpected ExpressionStatement"),
                }
            }
            _ => panic!("unexpected Statement"),
        }
    }

    #[test]
    fn test_hash_literal_expression_with_expression() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        let mut expected: HashMap<String, Box<dyn Fn(&Expression)>> = HashMap::new();

        expected.insert(
            "one".to_string(),
            Box::new(|expr: &Expression| {
                assert!(test_infix_expressions(expr, 0, "+", 1));
            }),
        );

        expected.insert(
            "two".to_string(),
            Box::new(|expr: &Expression| {
                assert!(test_infix_expressions(expr, 10, "-", 8));
            }),
        );

        expected.insert(
            "three".to_string(),
            Box::new(|expr: &Expression| {
                assert!(test_infix_expressions(expr, 15, "/", 5));
            }),
        );

        match &program.statements[0] {
            Statement::Expression(stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::Hash(hash_expr) => {
                        assert_eq!(3, hash_expr.pairs.len());
                        for (k, v) in &hash_expr.pairs {
                            match k {
                                Expression::String(sl) => {
                                    let test_fn = expected.get(&sl.value).unwrap();
                                    test_fn(v);
                                }
                                _ => panic!("unexpected key type"),
                            }
                        }
                    }
                    _ => panic!("unexpected ExpressionStatement"),
                }
            }
            _ => panic!("unexpected Statement"),
        }
    }

    #[test]
    fn test_hash_literal_expression() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        let mut expected = HashMap::new();
        expected.insert("one".to_string(), 1);
        expected.insert("two".to_string(), 2);
        expected.insert("three".to_string(), 3);

        match &program.statements[0] {
            Statement::Expression(stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::Hash(hash_expr) => {
                        assert_eq!(3, hash_expr.pairs.len());
                        for (k, v) in &hash_expr.pairs {
                            match k {
                                Expression::String(sl) => {
                                    let expected_value = *expected.get(&sl.value).unwrap();
                                    assert!(test_integer_literal(v, expected_value as i64));
                                }
                                _ => panic!("unexpected key type"),
                            }
                        }
                    }
                    _ => panic!("unexpected ExpressionStatement"),
                }
            }
            _ => panic!("unexpected Statement"),
        }
    }

    #[test]
    fn test_index_expression() {
        let input = r#"myArray[1 + 1]"#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::Index(index_expression) => {
                        assert!(test_identifier(&index_expression.left, "myArray"));
                        assert!(test_infix_expressions(&index_expression.index, 1, "+", 1))
                    }
                    _ => panic!("unexpected ExpressionStatement"),
                }
            }
            _ => panic!("unexpected Statement"),
        }
    }

    #[test]
    fn test_array_literal_expression() {
        let input = r#"[1, 2 * 2, 3 + 3]"#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        match program.statements[0] {
            Statement::Expression(ref stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::Array(array_literal) => {
                        assert_eq!(3, array_literal.elements.len());
                        test_integer_literal(&array_literal.elements[0], 1);
                        assert!(test_infix_expressions(
                            &array_literal.elements[1],
                            2,
                            "*",
                            2
                        ));
                        assert!(test_infix_expressions(
                            &array_literal.elements[2],
                            3,
                            "+",
                            3
                        ));
                    }

                    _ => panic!("unexpected Expression"),
                }
            }
            _ => panic!("unexpected statement"),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        match program.statements[0] {
            Statement::Expression(ref stmt) => {
                let expr = stmt.expression.as_ref().unwrap();
                match expr {
                    Expression::String(string_literal) => {
                        assert_eq!("hello world", string_literal.value);
                    }
                    _ => panic!("unexpected Expression"),
                }
            }
            _ => panic!("unexpected statement"),
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase {
            input: &'static str,
            expected: &'static str,
        }
        let tests = [
            TestCase {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            TestCase {
                input: "!-a",
                expected: "(!(-a))",
            },
            TestCase {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            TestCase {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            TestCase {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            TestCase {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            TestCase {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            TestCase {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            TestCase {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            TestCase {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            TestCase {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            TestCase {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            TestCase {
                input: "true",
                expected: "true",
            },
            TestCase {
                input: "false",
                expected: "false",
            },
            TestCase {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            TestCase {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
            TestCase {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            TestCase {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            TestCase {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            TestCase {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            TestCase {
                input: "!(true == true)",
                expected: "(!(true == true))",
            },
            TestCase {
                input: "a + add(b * c) + d",
                expected: "((a + add((b * c))) + d)",
            },
            TestCase {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            },
            TestCase {
                input: "add(a + b + c * d / f + g)",
                expected: "add((((a + b) + ((c * d) / f)) + g))",
            },
            TestCase {
                input: "a * [1, 2, 3, 4][b * c] * d",
                expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            },
            TestCase {
                input: "add(a * b[2], b[1], 2 * [1, 2][1])",
                expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            },
        ];

        for test_case in tests {
            let mut l = Lexer::new(test_case.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.as_string(), test_case.expected);
        }
    }

    #[test]
    fn test_parse_infix_expressios() {
        struct TestCase {
            input: &'static str,
            left_val: Box<dyn Any>,
            operator: &'static str,
            right_val: Box<dyn Any>,
        }
        let infix_tests = [
            TestCase {
                input: "5 + 5;",
                left_val: Box::new(5),
                operator: "+",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 - 5;",
                left_val: Box::new(5),
                operator: "-",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 * 5;",
                left_val: Box::new(5),
                operator: "*",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 / 5;",
                left_val: Box::new(5),
                operator: "/",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 > 5;",
                left_val: Box::new(5),
                operator: ">",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 < 5;",
                left_val: Box::new(5),
                operator: "<",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 == 5;",
                left_val: Box::new(5),
                operator: "==",
                right_val: Box::new(5),
            },
            TestCase {
                input: "5 != 5;",
                left_val: Box::new(5),
                operator: "!=",
                right_val: Box::new(5),
            },
            TestCase {
                input: "true == true",
                left_val: Box::new(true),
                operator: "==",
                right_val: Box::new(true),
            },
        ];

        for test_case in infix_tests {
            let mut l = Lexer::new(test_case.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program has not enough statements."
            );

            let stmt = &program.statements[0];
            if let Statement::Expression(stmt) = stmt {
                let expr = stmt
                    .expression
                    .as_ref()
                    .expect("Expression should not be None");

                if !test_infix_expressions(
                    expr,
                    test_case.left_val,
                    test_case.operator,
                    test_case.right_val,
                ) {
                    return;
                }
            } else {
                panic!(
                    "program.statements[0] is not ast.ExpressionStatement. got={}",
                    stmt.name()
                );
            }
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct TestCase {
            input: &'static str,
            expected_parapms: Vec<&'static str>,
        }
        let prefix_tests = [
            TestCase {
                input: "fn() {};",
                expected_parapms: Vec::new(),
            },
            TestCase {
                input: "fn(x) {};",
                expected_parapms: vec!["x"],
            },
            TestCase {
                input: "fn(x, y, z) {};",
                expected_parapms: vec!["x", "y", "z"],
            },
        ];

        for test_case in prefix_tests {
            let mut l = Lexer::new(test_case.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program has not enough statements."
            );

            let stmt = &program.statements[0];
            if let Statement::Expression(stmt) = stmt {
                let expr = stmt
                    .expression
                    .as_ref()
                    .expect("Expression should not be None");
                if let Expression::Function(fn_expr) = expr {
                    assert_eq!(
                        fn_expr.parameters.len(),
                        test_case.expected_parapms.len(),
                        "length parameters wrong.",
                    );

                    let ps: Vec<String> =
                        fn_expr.parameters.iter().map(|x| x.name.clone()).collect();
                    assert_eq!(ps, test_case.expected_parapms, "parameters not matched.");
                } else {
                    panic!("stmt is not ast.PrefixExpresion. got={}", expr);
                }
            } else {
                panic!(
                    "program.statements[0] is not ast.ExpressionStatement. got={}",
                    stmt.name()
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            let expr = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            match &expr {
                Expression::Call(call_expr) => {
                    if !test_identifier(&call_expr.function, "add") {
                        return;
                    }

                    assert_eq!(call_expr.arguments.len(), 3, "wrong length of arguments.");

                    test_integer_literal(&call_expr.arguments[0], 1);
                    test_infix_expressions(&call_expr.arguments[1], 2, "*", 3);
                    test_infix_expressions(&call_expr.arguments[2], 4, "+", 5);
                }

                _ => panic!("Expected FunctionExpression"),
            }
        } else {
            panic!("Expected ExpressionStatement")
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestCase {
            input: &'static str,
            operator: &'static str,
            val: Box<dyn Any>,
        }
        let prefix_tests = [
            TestCase {
                input: "!5;",
                operator: "!",
                val: Box::new(5),
            },
            TestCase {
                input: "-15",
                operator: "-",
                val: Box::new(-15),
            },
            TestCase {
                input: "!true",
                operator: "!",
                val: Box::new(true),
            },
            TestCase {
                input: "!false",
                operator: "!",
                val: Box::new(false),
            },
        ];

        for test_case in prefix_tests {
            let mut l = Lexer::new(test_case.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(),
                1,
                "program has not enough statements."
            );

            let stmt = &program.statements[0];
            if let Statement::Expression(stmt) = stmt {
                let expr = stmt
                    .expression
                    .as_ref()
                    .expect("Expression should not be None");
                if let Expression::Prefix(expr) = expr {
                    assert_eq!(
                        expr.operator, test_case.operator,
                        "exp.operator is not '{}' got={}",
                        test_case.operator, expr.operator
                    );
                    test_literal_expression(expr.right.as_ref().unwrap(), &test_case.val);
                } else {
                    panic!("stmt is not ast.PrefixExpresion. got={}", expr);
                }
            } else {
                panic!(
                    "program.statements[0] is not ast.ExpressionStatement. got={}",
                    stmt.name()
                );
            }
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            let expr = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            test_boolean_literal(expr, true);
        } else {
            panic!("Expected ExpressionStatement")
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        let stmt = match &program.statements[0] {
            Statement::Expression(stmt) => stmt,
            _ => panic!(
                "Expected ExpressionStatement, got {:?}",
                program.statements[0].as_string()
            ),
        };

        let if_expr = match stmt.expression.as_ref() {
            Some(Expression::If(if_expr)) => if_expr,
            _ => panic!("Expected IfExpression"),
        };

        assert!(
            test_infix_expressions(&if_expr.condition, "x", "<", "y"),
            "Failed condition test"
        );

        assert_eq!(
            if_expr.consequence.statements.len(),
            1,
            "consequence should have 1 statement."
        );

        let consequence_expr = match &if_expr.consequence.statements[0] {
            Statement::Expression(consequence) => consequence
                .expression
                .as_ref()
                .expect("Expected expression in consequence"),
            _ => panic!(
                "Expected ExpressionStatement in consequence, got {:?}",
                if_expr.consequence.statements[0].as_string()
            ),
        };

        assert!(
            test_identifier(consequence_expr, "x"),
            "Failed consequence identifier test"
        );

        let alt_expr = match &if_expr.alternative {
            Some(alt) => match &alt.statements[0] {
                Statement::Expression(alt) => alt
                    .expression
                    .as_ref()
                    .expect("Expected expression in consequence"),
                _ => panic!("Expcted expression in alternative"),
            },
            _ => panic!("Expected alternative"),
        };

        assert!(
            test_identifier(alt_expr, "y"),
            "Failed alternative identifier test"
        );
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x }";
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        let stmt = match &program.statements[0] {
            Statement::Expression(stmt) => stmt,
            _ => panic!(
                "Expected ExpressionStatement, got {:?}",
                program.statements[0].as_string()
            ),
        };

        let if_expr = match stmt.expression.as_ref() {
            Some(Expression::If(if_expr)) => if_expr,
            _ => panic!("Expected IfExpression"),
        };

        assert!(
            test_infix_expressions(&if_expr.condition, "x", "<", "y"),
            "Failed condition test"
        );

        assert_eq!(
            if_expr.consequence.statements.len(),
            1,
            "consequence should have 1 statement."
        );

        let consequence_expr = match &if_expr.consequence.statements[0] {
            Statement::Expression(consequence) => consequence
                .expression
                .as_ref()
                .expect("Expected expression in consequence"),
            _ => panic!(
                "Expected ExpressionStatement in consequence, got {:?}",
                if_expr.consequence.statements[0].as_string()
            ),
        };

        assert!(
            test_identifier(consequence_expr, "x"),
            "Failed consequence identifier test"
        );

        assert!(if_expr.alternative.is_none(), "Alternative should be None");
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            let expr = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            match &expr {
                Expression::Function(fn_expr) => {
                    assert_eq!(
                        fn_expr.parameters.len(),
                        2,
                        "function literal parameters wrong."
                    );

                    assert_eq!(
                        fn_expr.parameters[0].name, "x",
                        "first parameter not matched"
                    );
                    assert_eq!(
                        fn_expr.parameters[1].name, "y",
                        "second paramter not matched",
                    );

                    assert_eq!(
                        fn_expr.body.statements.len(),
                        1,
                        "function body statements has not 1 statements."
                    );

                    let body_stmt = match &fn_expr.body.statements[0] {
                        Statement::Expression(expr) => expr,
                        v => panic!(
                            "function body stmt is not ast.ExpressionStatement. got={}",
                            v.as_string()
                        ),
                    };
                    let body_expr = body_stmt
                        .expression
                        .as_ref()
                        .expect("Expression should not be None");
                    assert!(test_infix_expressions(body_expr, "x", "+", "y"));
                }

                _ => panic!("Expected FunctionExpression"),
            }
        } else {
            panic!("Expected ExpressionStatement")
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements."
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            let expr = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            test_integer_literal(expr, 5);
        } else {
            panic!("Expected ExpressionStatement")
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(),
            1,
            "program has incorrect number of statements."
        );

        if let Statement::Expression(stmt) = &program.statements[0] {
            let ident = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            test_identifier(ident, "foobar");
        } else {
            panic!("Expected statement to be ExpressionStatement");
        }
    }

    #[test]
    pub fn test_return_statement() {
        struct TestCase {
            input: &'static str,
            expected_value: &'static str,
        }
        let tests = [
            TestCase {
                input: "return 5;",
                expected_value: "5",
            },
            TestCase {
                input: "return true;",
                expected_value: "true",
            },
            TestCase {
                input: "return x;",
                expected_value: "x",
            },
        ];
        for tt in tests {
            let mut l = Lexer::new(tt.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert!(
                program.statements.len() == 1,
                "program.Statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            for st in &program.statements {
                if let Statement::Return(ret_stmt) = st {
                    assert_eq!(
                        ret_stmt.return_value.as_ref().unwrap().as_string(),
                        tt.expected_value,
                        "returnStmt.ReturnValue not matched",
                    );

                    assert_eq!(
                        ret_stmt.token_literal(),
                        "return",
                        "returnStmt.TokenLiteral not 'return', got {}",
                        ret_stmt.token_literal()
                    );
                } else {
                    panic!("s not *ast.LetStatement. got={}", st.name());
                }
            }
        }
    }

    #[test]
    pub fn test_left_statements() {
        struct TestCase {
            input: &'static str,
            expected_identifier: &'static str,
            expected_value: Box<dyn Any>,
        }
        let tests = [
            TestCase {
                input: "let x = 5;",
                expected_identifier: "x",
                expected_value: Box::new(5),
            },
            TestCase {
                input: "let y = true;",
                expected_identifier: "y",
                expected_value: Box::new(true),
            },
            TestCase {
                input: "let foobar = y;",
                expected_identifier: "foobar",
                expected_value: Box::new("y"),
            },
        ];

        for tt in tests {
            let mut l = Lexer::new(tt.input.to_string());
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert!(
                program.statements.len() == 1,
                "program.Statements does not contain 1 statements. got={}",
                program.statements.len()
            );

            let stmt = &program.statements[0];

            if !test_let_statement(stmt, tt.expected_identifier) {
                return;
            }

            match stmt {
                Statement::Let(let_st) => {
                    test_literal_expression(let_st.value.as_ref().unwrap(), &tt.expected_value);
                }
                _ => panic!("expected let statement"),
            }
        }
    }

    // helper function
    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("s.token_literal not 'let'. got={}", s.token_literal());
            return false;
        }

        if let Statement::Let(let_stmt) = s {
            if let_stmt.name.name != name {
                eprintln!(
                    "let_stmt.name.value not '{}'. got={}",
                    name, let_stmt.name.name
                );
                return false;
            }

            if let_stmt.name.token_literal() != name {
                eprintln!(
                    "let_stmt.name.token_literal() not '{}'. got={}",
                    name,
                    let_stmt.name.token_literal()
                );
                return false;
            }

            true
        } else {
            eprintln!("s not *ast.LetStatement. got={}", s.name());
            false
        }
    }

    fn test_identifier(expr: &Expression, value: &'static str) -> bool {
        if let Expression::Identifier(ident) = expr {
            if ident.token_literal() != value {
                eprintln!(
                    "ident.token_literal not {}. got={}",
                    value,
                    ident.token_literal()
                );
                return false;
            }

            if ident.name != value {
                eprintln!("ident.value not {}. got={}", value, ident.name);
                return false;
            }

            true
        } else {
            eprintln!("expr not ast.Identifier . got={}", expr);
            false
        }
    }

    fn test_boolean_literal(expr: &Expression, value: bool) -> bool {
        if let Expression::Boolean(boolean_expr) = expr {
            if boolean_expr.token_literal() != format!("{}", value) {
                eprintln!(
                    "boolean.token_literal not {}. got={}",
                    value,
                    boolean_expr.token_literal()
                );
                return false;
            }
            if boolean_expr.value != value {
                eprintln!("boolean.value not {}. got={}", value, boolean_expr.value);
                return false;
            }
            true
        } else {
            eprintln!("expr not ast.IntegerLiteral. got={}", expr);
            false
        }
    }

    fn test_integer_literal(expr: &Expression, value: i64) -> bool {
        if let Expression::Integer(integer) = expr {
            if integer.token_literal() != format!("{}", value) {
                eprintln!(
                    "integer.token_literal not {}. got={}",
                    value,
                    integer.token_literal()
                );
                return false;
            }
            if integer.value != value {
                eprintln!("integer.value not {}. got={}", value, integer.value);
                return false;
            }
            true
        } else {
            eprintln!("expr not ast.IntegerLiteral. got={}", expr);
            false
        }
    }

    fn test_literal_expression(expr: &Expression, expected: &dyn Any) -> bool {
        if let Some(&val) = expected.downcast_ref::<i32>() {
            test_integer_literal(expr, val as i64)
        } else if let Some(&val) = expected.downcast_ref::<i64>() {
            test_integer_literal(expr, val)
        } else if let Some(&val) = expected.downcast_ref::<&str>() {
            test_identifier(expr, val)
        } else if let Some(&val) = expected.downcast_ref::<bool>() {
            test_boolean_literal(expr, val)
        } else {
            eprintln!("type of expr not handled. got={}", expr.as_string());
            false
        }
    }

    fn test_infix_expressions<L, R>(
        expr: &Expression,
        left: L,
        operator: &'static str,
        right: R,
    ) -> bool
    where
        L: Any + Debug,
        R: Any + Debug,
    {
        if let Expression::Infix(infix) = expr {
            if !test_literal_expression(infix.left.as_ref().unwrap(), &left) {
                return false;
            }
            if infix.operator != operator {
                eprintln!("operator is not '{}'. got='{}'", operator, infix.operator);
                return false;
            }
            if !test_literal_expression(infix.right.as_ref().unwrap(), &right) {
                return false;
            }
            true
        } else {
            eprintln!("expr is not ast.InfixExpression. got={}", expr.as_string());
            false
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = &p.errors;
        if !errors.is_empty() {
            eprintln!("parser has {} errors", errors.len());
            for msg in errors {
                eprintln!("parser error: {}", msg);
            }
            panic!("Test failed due to parser errors");
        }
    }
}
