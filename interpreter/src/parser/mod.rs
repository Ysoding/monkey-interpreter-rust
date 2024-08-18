use crate::{
    ast::{Identifier, LetStatement, Program, ReturnStatement, Statement},
    lexer::{
        token::{Token, TokenType},
        Lexer,
    },
};

pub struct Parser<'a> {
    pub l: &'a mut Lexer,
    pub cur_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::default(),
            peek_token: Token::default(),
            errors: Vec::new(),
        };
        p.next_token();
        p.next_token();
        p
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

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.typ {
            TokenType::Let => Some(Box::new(self.parse_let_statement()?)),
            TokenType::Return => Some(Box::new(self.parse_ret_statement()?)),
            _ => None,
        }
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
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::ast::{Node, ReturnStatement};

    use super::*;

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
            let ret_stmt = match st.as_any().downcast_ref::<ReturnStatement>() {
                Some(ret_stmt) => ret_stmt,
                None => {
                    eprintln!("s not *ast.LetStatement. got={}", st.st_name());
                    continue;
                }
            };

            if ret_stmt.token_literal() != "return" {
                eprintln!(
                    "returnStmt.TokenLiteral not 'return', got {}",
                    ret_stmt.token_literal()
                );
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
            if !test_let_statement(stmt.as_ref(), expected_ident) {
                return;
            }
        }
    }

    fn test_let_statement(s: &dyn Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprintln!("s.token_literal not 'let'. got={}", s.token_literal());
            return false;
        }

        let let_stmt = match s.as_any().downcast_ref::<LetStatement>() {
            Some(let_stmt) => let_stmt,
            None => {
                eprintln!("s not *ast.LetStatement. got={}", s.st_name());
                return false;
            }
        };

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
