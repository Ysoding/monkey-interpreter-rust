use std::collections::HashMap;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program,
        ReturnStatement, Statement,
    },
    lexer::{
        token::{Token, TokenType},
        Lexer,
    },
};

type PrefixParseFn = fn(&mut Parser) -> Expression;
type InfixParseFn = fn(&mut Parser, Expression) -> Expression;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
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
        p.next_token();
        p.next_token();
        p
    }

    fn parse_integer_literal(p: &mut Parser) -> Expression {
        let v = p.cur_token.literal.parse().unwrap();
        let ret = IntegerLiteral {
            token: p.cur_token.clone(),
            value: v,
        };

        Expression::IntegerLiteral(ret)
    }

    fn parse_identifier(p: &mut Parser) -> Expression {
        Expression::Identifier(Identifier {
            token: p.cur_token.clone(),
            value: p.cur_token.literal.clone(),
        })
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Program {
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

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = match self.prefix_parse_fns.get(&self.cur_token.typ) {
            Some(prefix_fn) => prefix_fn,
            None => return None,
        };

        let left_exp = prefix(self);

        Some(left_exp)
    }

    fn parse_ret_statement(&mut self) -> Option<ReturnStatement> {
        let tok = self.cur_token.clone();
        self.next_token();

        let sem_tok = TokenType::Semicolon;
        while !self.cur_token_is(&sem_tok) {
            self.next_token();
        }

        Some(ReturnStatement {
            token: tok,
            return_value: None,
        })
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Identifier) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        let sem_tok = TokenType::Semicolon;
        while !self.cur_token_is(&sem_tok) {
            self.next_token();
        }

        Some(LetStatement {
            token,
            name,
            value: None,
        })
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
    use core::panic;

    use crate::ast::Node;

    use super::*;

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
            let ex = stmt
                .expression
                .as_ref()
                .expect("Expression should not be None");
            if let Expression::IntegerLiteral(literal) = ex {
                assert_eq!(literal.value, 5, "Literal value mismatch");
                assert_eq!(
                    literal.token_literal(),
                    "5",
                    "Literal token_literal mismatch"
                );
            } else {
                panic!("Expected IntegerLiteral")
            }
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
            if let Expression::Identifier(id) = ident {
                assert_eq!(id.value, "foobar", "Identifier value mismatch");
                assert_eq!(
                    id.token_literal(),
                    "foobar",
                    "Identifier token_literal mismatch"
                );
            } else {
                panic!("Expression is not an Identifier");
            }
        } else {
            panic!("Expected statement to be ExpressionStatement");
        }
    }

    #[test]
    pub fn test_return_statement() {
        let input = r#"
        return 5;
        return 10;
        return 993 322;
        "#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(
            program.statements.len() == 3,
            "program.Statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        for st in &program.statements {
            if let Statement::Return(ret_stmt) = st {
                if ret_stmt.token_literal() != "return" {
                    eprintln!(
                        "returnStmt.TokenLiteral not 'return', got {}",
                        ret_stmt.token_literal()
                    );
                }
            } else {
                panic!("s not *ast.LetStatement. got={}", st.name());
            }
        }
    }

    #[test]
    pub fn test_left_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let z = 838383;
        "#;

        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(
            program.statements.len() == 3,
            "program.Statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        let test_cases = ["x", "y", "foo"];

        for (i, &expected_ident) in test_cases.iter().enumerate() {
            let stmt = &program.statements[i];
            if !test_let_statement(stmt, expected_ident) {
                return;
            }
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("s.token_literal not 'let'. got={}", s.token_literal());
            return false;
        }

        if let Statement::Let(let_stmt) = s {
            if let_stmt.name.value != name {
                eprintln!(
                    "let_stmt.name.value not '{}'. got={}",
                    name, let_stmt.name.value
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
            return false;
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
