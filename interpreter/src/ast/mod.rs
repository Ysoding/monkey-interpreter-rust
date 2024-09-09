use std::any::Any;
use std::fmt::Write;

use crate::lexer::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn as_string(&self) -> String;
}

pub trait Statement: Node + Any {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
    fn st_name(&self) -> &str;
}

pub trait Expression: Node + Any {
    fn expression_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
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
    pub value: Option<Box<dyn Expression>>,
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

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn st_name(&self) -> &str {
        "LetStatement"
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
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
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn st_name(&self) -> &str {
        "ReturnStatement"
    }
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
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn st_name(&self) -> &str {
        "ExpressionStatement"
    }
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

#[cfg(test)]
mod tests {
    use crate::lexer::token::*;

    use super::*;

    #[test]
    fn test_string() {
        let p = Program {
            statements: vec![Box::new(LetStatement {
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
                value: Some(Box::new(Identifier {
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
