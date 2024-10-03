use std::fmt::Write;

use crate::lexer::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_string(&self) -> String;
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn name(&self) -> &str {
        match self {
            Statement::Let(_) => "LetStatement",
            Statement::Return(_) => "ReturnStatement",
            Statement::Expression(_) => "ExpressionStatement",
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
            Statement::Return(stmt) => stmt.token_literal(),
            Statement::Expression(stmt) => stmt.token_literal(),
        }
    }

    fn as_string(&self) -> String {
        match self {
            Statement::Let(stmt) => stmt.as_string(),
            Statement::Return(stmt) => stmt.as_string(),
            Statement::Expression(stmt) => stmt.as_string(),
        }
    }
}

pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpresion),
    Infix(InfixExpresion),
    Boolean(BooleanExpression),
}

impl Expression {
    pub fn name(&self) -> &str {
        match self {
            Expression::Identifier(_) => "IdentifierExpression",
            Expression::IntegerLiteral(_) => "IntegerLiteralExpression",
            Expression::Prefix(_) => "PrefixExpresion",
            Expression::Infix(_) => "InfixExpresion",
            Expression::Boolean(_) => "BooleanExpression",
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(expr) => expr.token_literal(),
            Expression::IntegerLiteral(expr) => expr.token_literal(),
            Expression::Prefix(expr) => expr.token_literal(),
            Expression::Infix(expr) => expr.token_literal(),
            Expression::Boolean(expr) => expr.token_literal(),
        }
    }

    fn as_string(&self) -> String {
        match self {
            Expression::Identifier(expr) => expr.as_string(),
            Expression::IntegerLiteral(expr) => expr.as_string(),
            Expression::Prefix(expr) => expr.as_string(),
            Expression::Infix(expr) => expr.as_string(),
            Expression::Boolean(expr) => expr.as_string(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.is_empty() {
            ""
        } else {
            self.statements[0].token_literal()
        }
    }

    fn as_string(&self) -> String {
        let mut out = String::new();
        for st in &self.statements {
            write!(&mut out, "{}", st.as_string()).unwrap();
        }
        out
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut out = String::new();

        write!(
            &mut out,
            "{} {} = ",
            self.token_literal(),
            self.name.as_string()
        )
        .unwrap();

        if let Some(val) = &self.value {
            write!(&mut out, "{}", val.as_string()).unwrap();
        }

        write!(&mut out, ";").unwrap();

        out
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.value.clone()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut out = String::new();

        write!(&mut out, "{} ", self.token_literal()).unwrap();

        if let Some(val) = &self.return_value {
            write!(&mut out, "{}", val.as_string()).unwrap();
        }

        write!(&mut out, ";").unwrap();
        out
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        if let Some(val) = &self.expression {
            val.as_string()
        } else {
            "".to_string()
        }
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct PrefixExpresion {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<Expression>>,
}

impl Node for PrefixExpresion {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut out = String::new();

        if let Some(right_expr) = self.right.as_ref() {
            write!(&mut out, "({}{})", self.operator, right_expr.as_string()).unwrap();
        } else {
            write!(&mut out, "({} <ERROR>)", self.operator).unwrap();
        }

        out
    }
}

pub struct InfixExpresion {
    pub token: Token,
    pub left: Option<Box<Expression>>,
    pub operator: String,
    pub right: Option<Box<Expression>>,
}

impl Node for InfixExpresion {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        let mut out = String::new();

        if let Some(left_expr) = self.left.as_ref() {
            write!(&mut out, "({} {} ", left_expr.as_string(), self.operator).unwrap();
        } else {
            write!(&mut out, "(<ERROR> {} ", self.operator).unwrap();
        }

        if let Some(right_expr) = self.right.as_ref() {
            write!(&mut out, "{})", right_expr.as_string()).unwrap();
        } else {
            write!(&mut out, "<ERROR>)").unwrap();
        }

        out
    }
}

pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn as_string(&self) -> String {
        self.token.literal.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::token::*;

    use super::*;

    #[test]
    fn test_string() {
        let p = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    typ: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        typ: TokenType::Identifier,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Some(Expression::Identifier(Identifier {
                    token: Token {
                        typ: TokenType::Identifier,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert!(
            p.as_string() == "let myVar = anotherVar;",
            "program.as_string() wrong. got={}",
            p.as_string()
        );
    }
}
