use std::io::{BufRead, Write};

pub fn start(mut input: impl BufRead, mut output: impl Write) {
    loop {
        output.write_all(b">> ").unwrap();
        output.flush().unwrap();

        let mut buffer = String::new();

        input.read_line(&mut buffer).unwrap();

        let input = buffer.trim();
        if input.is_empty() || input == "quit" || input == "q" {
            break;
        }

        output.write_all(buffer.as_bytes()).unwrap();
        buffer.clear();
    }
}
