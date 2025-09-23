use crate::{ast::BindingPower, lexer::Lexer, parser::Parser};
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
    if ast.is_err() {
        for err in ast.err().unwrap() {
            err.print();
        }
        return;
    }
    println!("{}", ast.unwrap().format("", true));
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    fn get_tokens(input: &str) -> Vec<String> {
        Lexer::new(input.to_string())
            .tokenize()
            .unwrap_or_default()
            .iter()
            .map(|t| t.t.value())
            .collect()
    }

    #[test]
    fn lexer_tokenize_literals() {
        assert_eq!(
            get_tokens("100 10.5 0x15 0b10110 \"\\0x68\\0x69\\0x21\" is_true"),
            vec!["100", "10.5", "21", "22", "hi!", "is_true", "EOF"]
        );
    }

    #[test]
    fn lexer_tokenize_operators() {
        assert_eq!(
            get_tokens("+ - * / % & && | || = == != <= >= < > . , += -= *= /= .. ++ --"),
            vec![
                "+", "-", "*", "/", "%", "&", "&&", "|", "||", "=", "==", "!=", "<=", ">=", "<",
                ">", ".", ",", "+=", "-=", "*=", "/=", "..", "++", "--", "EOF"
            ]
        );
    }

    #[test]
    fn lexer_tokenize_keywords() {
        assert_eq!(
            get_tokens("fn let if else return while for"),
            vec!["fn", "let", "if", "else", "return", "while", "for", "EOF"]
        );
    }

    #[test]
    fn lexer_tokenize_parentheses() {
        assert_eq!(
            get_tokens("() {} []"),
            vec!["(", ")", "{", "}", "[", "]", "EOF"]
        );
    }
}
