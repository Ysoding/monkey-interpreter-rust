use std::io::{BufRead, Write};

use crate::lexer::{token::TokenType, Lexer};

pub fn start(mut input: impl BufRead, mut output: impl Write) {
    loop {
        output.write_all(b">> ").unwrap();
        output.flush().unwrap();

        let mut buffer = String::new();

        input.read_line(&mut buffer).unwrap();

        let mut l = Lexer::new(buffer.trim().to_string());

        let mut tok = l.next_token();
        while tok.typ != TokenType::Eof {
            writeln!(output, "{:?}", tok).unwrap();
            tok = l.next_token();
        }

        buffer.clear();
    }
}
