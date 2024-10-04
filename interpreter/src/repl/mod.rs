use std::io::{BufRead, Write};

use crate::{ast::Node, lexer::Lexer, parser::Parser};

pub fn start(mut input: impl BufRead, mut output: impl Write) {
    let mut buffer = String::new();
    loop {
        output.write_all(b">> ").unwrap();
        output.flush().unwrap();

        input.read_line(&mut buffer).unwrap();

        let mut l = Lexer::new(buffer.trim().to_string());

        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        if !p.errors.is_empty() {
            print_parse_errors(&mut output, &p.errors);
            continue;
        }
        writeln!(output, "{}", program.as_string()).unwrap();

        buffer.clear();
    }
}

fn print_parse_errors(mut output: impl Write, errors: &Vec<String>) {
    for err in errors {
        writeln!(output, "\t{}", err).unwrap();
    }
}
