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

        let tok = match self.ch {
            Some('+') => self.new_token(token::TokenType::Plus, '+'),
            Some('-') => self.new_token(token::TokenType::Minus, '-'),
            Some('/') => self.new_token(token::TokenType::Slash, '/'),
            Some('*') => self.new_token(token::TokenType::Asterisk, '*'),
            Some('<') => self.new_token(token::TokenType::Lt, '<'),
            Some('>') => self.new_token(token::TokenType::Gt, '>'),
            Some(',') => self.new_token(token::TokenType::Comma, ','),
            Some(';') => self.new_token(token::TokenType::Semicolon, ';'),
            Some('(') => self.new_token(token::TokenType::LParen, '('),
            Some(')') => self.new_token(token::TokenType::RParen, ')'),
            Some('{') => self.new_token(token::TokenType::LBrace, '{'),
            Some('}') => self.new_token(token::TokenType::RBrace, '}'),
            Some('[') => self.new_token(token::TokenType::LBracket, '['),
            Some(']') => self.new_token(token::TokenType::RBracket, ']'),
            Some('"') => {
                let literal = self.read_string();
                let typ = token::TokenType::String;
                token::Token { typ, literal }
            }
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    token::Token::new(token::TokenType::Eq, "==".to_string())
                } else {
                    self.new_token(token::TokenType::Assign, '=')
                }
            }
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    token::Token::new(token::TokenType::Neq, "!=".to_string())
                } else {
                    self.new_token(token::TokenType::Bang, '!')
                }
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

                self.new_token(token::TokenType::Illegal, ch)
            }
            None => token::Token {
                typ: token::TokenType::Eof,
                literal: String::new(),
            },
        };

        self.read_char();
        tok
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;
        loop {
            self.read_char();
            match self.ch {
                Some(v) => {
                    if v == '"' {
                        break;
                    }
                }
                None => break,
            }
        }
        self.input[pos..self.position].iter().collect()
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
        "foobar";
        "foo bar";
        [1, 2];
        "#;

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Identifier, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Identifier, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Identifier, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Identifier, "x"),
            (TokenType::Comma, ","),
            (TokenType::Identifier, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            //
            (TokenType::Identifier, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Identifier, "y"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::Let, "let"),
            (TokenType::Identifier, "result"),
            (TokenType::Assign, "="),
            (TokenType::Identifier, "add"),
            (TokenType::LParen, "("),
            (TokenType::Identifier, "five"),
            (TokenType::Comma, ","),
            (TokenType::Identifier, "ten"),
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
            (TokenType::String, "foobar"),
            (TokenType::Semicolon, ";"),
            (TokenType::String, "foo bar"),
            (TokenType::Semicolon, ";"),
            //
            (TokenType::LBracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::RBracket, "]"),
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
