use crate::lexer::Lexer;
use std::io::{self};

mod ast;
mod lexer;
mod parser;

fn main() {
    println!(
        "This is a placeholder for the main function. The main functionality will be implemented later. This is just to ensure the program compiles and for testing purposes."
    );
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read stdin");
    let mut lexer = Lexer::new(buffer);
    let tokens = lexer.tokenize();
    dbg!(&tokens);
}
// Placeholder main function to ensure the program compiles.
