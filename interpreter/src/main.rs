use std::io;

use interpreter::repl;

fn main() {
    let username = users::get_current_username().expect("Failed to get current username");

    println!(
        "Hello {}! This is the Monkey programming language!",
        username.to_string_lossy()
    );
    println!("Feel free to type in commands");
    repl::start(io::stdin().lock(), io::stdout());
}
