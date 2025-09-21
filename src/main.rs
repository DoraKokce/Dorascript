use crate::{ast::Token, lexer::Lexer, parser::Parser};
use std::io::{self};

mod ast;
mod errors;
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
    if tokens.is_err() {
        for err in tokens.err().unwrap() {
            err.print();
        }
        return;
    }
    let mut parser: Parser = Parser::new(tokens.unwrap());
    let ast = parser.parse();
    println!("{:?}", ast);
}
