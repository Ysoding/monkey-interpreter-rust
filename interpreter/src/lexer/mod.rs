pub mod token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: Option<char>,     // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_white_space();

        let mut tok: token::Token;
        match self.ch {
            Some('+') => tok = self.new_token(token::TokenType::Plus, '+'),
            Some('-') => tok = self.new_token(token::TokenType::Minus, '-'),
            Some('/') => tok = self.new_token(token::TokenType::Slash, '/'),
            Some('*') => tok = self.new_token(token::TokenType::Asterisk, '*'),
            Some('<') => tok = self.new_token(token::TokenType::Lt, '<'),
            Some('>') => tok = self.new_token(token::TokenType::Gt, '>'),
            Some(',') => tok = self.new_token(token::TokenType::Comma, ','),
            Some(';') => tok = self.new_token(token::TokenType::Semicolon, ';'),
            Some('(') => tok = self.new_token(token::TokenType::LParen, '('),
            Some(')') => tok = self.new_token(token::TokenType::RParen, ')'),
            Some('{') => tok = self.new_token(token::TokenType::LBrace, '{'),
            Some('}') => tok = self.new_token(token::TokenType::RBrace, '}'),
            Some('=') => {
                tok = if let Some('=') = self.peek_char() {
                    self.read_char();
                    token::Token::new(token::TokenType::Eq, "==".to_string())
                } else {
                    self.new_token(token::TokenType::Assign, '=')
                };
            }
            Some('!') => {
                tok = if let Some('=') = self.peek_char() {
                    self.read_char();
                    token::Token::new(token::TokenType::Neq, "!=".to_string())
                } else {
                    self.new_token(token::TokenType::Bang, '!')
                };
            }
            Some(ch) => {
                if self.is_letter(ch) {
                    let literal = self.read_identifier();
                    return token::Token {
                        typ: token::lookup_ident(&literal),
                        literal,
                    };
                }
                if self.is_digit(ch) {
                    return token::Token {
                        typ: token::TokenType::Int,
                        literal: self.read_number(),
                    };
                }

                tok = self.new_token(token::TokenType::Illegal, ch);
            }
            None => {
                tok = token::Token {
                    typ: token::TokenType::Eof,
                    literal: String::new(),
                };
            }
        };
        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn skip_white_space(&mut self) {
        while self.ch == Some(' ')
            || self.ch == Some('\t')
            || self.ch == Some('\n')
            || self.ch == Some('\r')
        {
            self.read_char();
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn new_token(&self, typ: token::TokenType, ch: char) -> token::Token {
        token::Token {
            typ,
            literal: ch.to_string(),
        }
    }

    fn read_number(&mut self) -> String {
        let pos = self.position;
        while let Some(ch) = self.ch {
            if self.is_digit(ch) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[pos..self.position].iter().collect()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while let Some(ch) = self.ch {
            if self.is_letter(ch) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[pos..self.position].iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::TokenType;

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;	
        };
        let result = add(five, ten);
        !-/*0;
        2 < 10 > 7;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "#;

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            //
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "0"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "2"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "7"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            //
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::Neq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Eof, ""),
        ];

        let mut l = Lexer::new(input.to_string());

        for (i, (expected_type, expected_literal)) in tests.into_iter().enumerate() {
            let tok = l.next_token();

            assert_eq!(
                tok.typ, expected_type,
                "tests[{}] - token type wrong. expected={:?}, got={:?}",
                i, expected_type, tok.typ
            );

            assert_eq!(
                tok.literal, expected_literal,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, expected_literal, tok.literal
            );
        }
    }
}
