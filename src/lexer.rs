/*
    Lexer for a simple programming language.
    Supports identifiers, numbers (decimal, hex, binary), string literals, operators, and parentheses
    Handles comments and whitespace.
    Provides error handling for invalid tokens and unterminated string literals.
    Not fully developed; serves as a foundation for further enhancements.
*/

use crate::ast::{ParantheseType, Token};

pub struct Lexer {
    index: usize,
    input: String,
    keywords: Vec<&'static str>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            index: 0,
            input,
            keywords: vec!["fn", "let", "if", "else", "return", "while", "for"],
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens: Vec<Token> = vec![];

        while let Some(char) = self.peek() {
            match char {
                '/' => {
                    let next = self.input.chars().nth(self.index + 1);
                    if next == Some('/') {
                        self.advance_until(|c| c == '\n');
                    }
                }
                ' ' | '\n' | '\t' => {
                    self.advance();
                }
                '(' | '[' | '{' => {
                    tokens.push(Token::OpenParen(match char {
                        '(' => ParantheseType::Round,
                        '[' => ParantheseType::Square,
                        '{' => ParantheseType::Curly,
                        _ => unreachable!(),
                    }));
                    self.advance();
                }
                ')' | ']' | '}' => {
                    tokens.push(Token::CloseParen(match char {
                        ')' => ParantheseType::Round,
                        ']' => ParantheseType::Square,
                        '}' => ParantheseType::Curly,
                        _ => unreachable!(),
                    }));
                    self.advance();
                }
                '"' => {
                    tokens.push(self.tokenize_string_literal()?);
                }
                _ => {
                    if let Some(op) = self.tokenize_operator() {
                        tokens.push(op);
                        continue;
                    } else if char.is_digit(10) {
                        tokens.push(self.tokenize_number()?);
                        continue;
                    } else if char.is_alphabetic() || char == '_' {
                        tokens.push(self.tokenize_identifier());
                        continue;
                    }
                    return Err(format!("Unexpected character: {}", char));
                }
            }
        }

        tokens.push(Token::EOF);
        Ok(tokens)
    }

    fn tokenize_number(&mut self) -> Result<Token, String> {
        let mut number_str = String::new();

        if let Some('0') = self.peek() {
            number_str.push('0');
            self.advance();
            if let Some(next) = self.peek() {
                if next == 'x' || next == 'X' {
                    number_str.push(next);
                    self.advance();
                    while let Some(char) = self.peek() {
                        if char.is_digit(16) {
                            number_str.push(char);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    match u64::from_str_radix(&number_str[2..], 16) {
                        Ok(num) => return Ok(Token::Number(num as f64)),
                        Err(_) => return Err(format!("Invalid hex number: {}", number_str)),
                    }
                } else if next == 'b' || next == 'B' {
                    number_str.push(next);
                    self.advance();
                    while let Some(char) = self.peek() {
                        if char == '0' || char == '1' {
                            number_str.push(char);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    match u64::from_str_radix(&number_str[2..], 2) {
                        Ok(num) => return Ok(Token::Number(num as f64)),
                        Err(_) => return Err(format!("Invalid binary number: {}", number_str)),
                    }
                }
            }
        }

        while let Some(char) = self.peek() {
            if char.is_digit(10) || char == '.' {
                number_str.push(char);
                self.advance();
            } else {
                break;
            }
        }
        match number_str.parse::<f64>() {
            Ok(num) => Ok(Token::Number(num)),
            Err(_) => Err(format!("Invalid number: {}", number_str)),
        }
    }

    fn tokenize_identifier(&mut self) -> Token {
        let mut ident_str = String::new();
        while let Some(char) = self.peek() {
            if char.is_alphanumeric() || char == '_' {
                ident_str.push(char);
                self.advance();
            } else {
                break;
            }
        }

        if self.keywords.contains(&ident_str.as_str()) {
            Token::Keyword(ident_str)
        } else {
            Token::Identifier(ident_str)
        }
    }

    fn tokenize_operator(&mut self) -> Option<Token> {
        let first = match self.peek() {
            Some(c) => c,
            None => return None,
        };

        if let Some(second) = self.input.chars().nth(self.index + 1) {
            let two = format!("{}{}", first, second);
            let ops = [
                '+', '-', '*', '/', '%', '=', '<', '>', '&', '|', ';', '.', ',',
            ];
            let multi_ops = [
                "==", "!=", "<=", ">=", "+=", "-=", "*=", "/=", "++", "--", "&&", "||",
            ];
            if multi_ops.contains(&two.as_str()) {
                self.advance();
                self.advance();
                return Some(Token::Operator(two));
            } else if ops.contains(&first) {
                self.advance();
                return Some(Token::Operator(first.to_string()));
            } else {
                return None;
            }
        }

        None
    }

    fn tokenize_string_literal(&mut self) -> Result<Token, String> {
        let mut string_lit = String::new();
        self.advance();
        while let Some(char) = self.peek() {
            if char == '"' {
                self.advance();
                return Ok(Token::StringLiteral(string_lit));
            } else if char == '\\' {
                self.advance();
                if let Some(escaped) = self.peek() {
                    match escaped {
                        'n' => string_lit.push('\n'),
                        't' => string_lit.push('\t'),
                        'r' => string_lit.push('\r'),
                        '"' => string_lit.push('"'),
                        '\\' => string_lit.push('\\'),
                        _ => {
                            let num = self.tokenize_number();
                            let Some(Token::Number(n)) = num.ok() else {
                                return Err("Invalid escape sequence".to_string());
                            };
                            string_lit.push(
                                char::from_u32(n.floor() as u32)
                                    .ok_or("Invalid Unicode code point")?,
                            );
                            continue;
                        }
                    }
                    self.advance();
                }
            } else {
                string_lit.push(char);
                self.advance();
            }
        }
        Err("Unterminated string literal".to_string())
    }

    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.index)
    }

    fn advance(&mut self) -> Option<char> {
        let old = self.input.chars().nth(self.index);
        self.index += 1;
        old
    }

    fn advance_until<F>(&mut self, condition: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(char) = self.peek() {
            if condition(char) {
                break;
            }
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ParantheseType, Token};

    #[test]
    fn tokenize_string_literal() {
        let input = r#""Hello, World!\nThis is a test string with a number: 42 and a hex: \0x2A""#
            .to_string();
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().expect("tokenize failed");

        let expected = vec![
            Token::StringLiteral(
                "Hello, World!\nThis is a test string with a number: 42 and a hex: \x2A"
                    .to_string(),
            ),
            Token::EOF,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_simple() {
        let input = "fn main() { let x = 5; }".to_string();
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().expect("tokenize failed");

        let expected = vec![
            Token::Keyword("fn".to_string()),
            Token::Identifier("main".to_string()),
            Token::OpenParen(ParantheseType::Round),
            Token::CloseParen(ParantheseType::Round),
            Token::OpenParen(ParantheseType::Curly),
            Token::Keyword("let".to_string()),
            Token::Identifier("x".to_string()),
            Token::Operator("=".to_string()),
            Token::Number(5.0),
            Token::Operator(";".to_string()),
            Token::CloseParen(ParantheseType::Curly),
            Token::EOF,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn tokenize_operator() {
        let input = "+ - * / % = < > & | ; == <= >= != && ||".to_string();
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().expect("tokenize failed");

        let expected = vec![
            Token::Operator("+".to_string()),
            Token::Operator("-".to_string()),
            Token::Operator("*".to_string()),
            Token::Operator("/".to_string()),
            Token::Operator("%".to_string()),
            Token::Operator("=".to_string()),
            Token::Operator("<".to_string()),
            Token::Operator(">".to_string()),
            Token::Operator("&".to_string()),
            Token::Operator("|".to_string()),
            Token::Operator(";".to_string()),
            Token::Operator("==".to_string()),
            Token::Operator("<=".to_string()),
            Token::Operator(">=".to_string()),
            Token::Operator("!=".to_string()),
            Token::Operator("&&".to_string()),
            Token::Operator("||".to_string()),
            Token::EOF,
        ];

        assert_eq!(tokens, expected);
    }
}
