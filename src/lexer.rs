/*
    Lexer for a simple programming language.
    Supports identifiers, numbers (decimal, hex, binary), string literals, operators, and parentheses
    Handles comments and whitespace.
    Provides error handling for invalid tokens and unterminated string literals.
    Not fully developed; serves as a foundation for further enhancements.
*/

use crate::{
    ast::{ParantheseType, Position, Span, Token, TokenType},
    errors::Error,
};

pub struct Lexer {
    index: usize,
    input: String,
    keywords: Vec<&'static str>,
    current_pos: Position,
    start_pos: Position,
    errors: Vec<Error>,
    file_name: String,
}

impl Lexer {
    pub fn new(input: String, file_name: String) -> Self {
        Lexer {
            index: 0,
            input,
            keywords: vec!["fn", "let", "if", "else", "return", "while", "for"],
            current_pos: Position { row: 1, column: 0 },
            start_pos: Position { row: 1, column: 0 },
            errors: vec![],
            file_name,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut tokens: Vec<Token> = vec![];

        while let Some(char) = self.peek() {
            self.update_start_pos();
            match char {
                '/' => {
                    let next = self.peak_next();
                    if next == Some('/') {
                        self.advance_until(|c| c == '\n');
                    } else {
                        let op = self.tokenize_operator();
                        if let Some(op) = op {
                            tokens.push(op);
                        }
                    }
                }
                ' ' | '\n' | '\t' => {
                    self.advance();
                }
                '(' | '[' | '{' => {
                    self.advance();
                    tokens.push(Token::new(
                        TokenType::OpenParen(match char {
                            '(' => ParantheseType::Round,
                            '[' => ParantheseType::Square,
                            '{' => ParantheseType::Curly,
                            _ => unreachable!(),
                        }),
                        self.current_span(),
                    ));
                }
                ')' | ']' | '}' => {
                    self.advance();
                    tokens.push(Token::new(
                        TokenType::CloseParen(match char {
                            ')' => ParantheseType::Round,
                            ']' => ParantheseType::Square,
                            '}' => ParantheseType::Curly,
                            _ => unreachable!(),
                        }),
                        self.current_span(),
                    ));
                }
                '"' => {
                    let token = self.tokenize_string_literal();
                    if token == None {
                        continue;
                    }
                    tokens.push(token.unwrap());
                }
                ';' => {
                    self.advance();
                    tokens.push(Token::new(TokenType::Semicolon, self.current_span()));
                }
                _ => {
                    if let Some(op) = self.tokenize_operator() {
                        tokens.push(op);
                        continue;
                    } else if char.is_digit(10) {
                        tokens.push(self.tokenize_number().unwrap());
                        continue;
                    } else if char.is_alphabetic() || char == '_' {
                        tokens.push(self.tokenize_identifier());
                        continue;
                    }
                    self.errors.push(Error::new(
                        format!("unexpected character: '{}'", char),
                        self.current_span(),
                        self.file_name.clone(),
                        1,
                    ));
                    self.advance();
                }
            }
        }

        if self.errors.is_empty() {
            tokens.push(Token::new(TokenType::EOF, self.current_span()));
            Ok(tokens)
        } else {
            Err(self.errors.clone())
        }
    }

    fn tokenize_number(&mut self) -> Option<Token> {
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
                        Ok(num) => {
                            return Some(Token::new(
                                TokenType::Number(num as f64),
                                self.current_span(),
                            ));
                        }
                        Err(_) => {
                            self.errors.push(Error::new(
                                format!("invalid hex number: {}", number_str),
                                self.current_span(),
                                self.file_name.clone(),
                                2,
                            ));
                        }
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
                        Ok(num) => {
                            return Some(Token::new(
                                TokenType::Number(num as f64),
                                self.current_span(),
                            ));
                        }
                        Err(_) => {
                            self.errors.push(Error::new(
                                format!("invalid binary number: {}", number_str),
                                self.current_span(),
                                self.file_name.clone(),
                                3,
                            ));
                        }
                    }
                }
            }
        }

        while let Some(char) = self.peek() {
            if char.is_digit(10) || (char == '.' && self.peak_next()?.is_digit(10)) {
                number_str.push(char);
                self.advance();
            } else {
                break;
            }
        }
        match number_str.parse::<f64>() {
            Ok(num) => Some(Token::new(TokenType::Number(num), self.current_span())),
            Err(_) => {
                self.errors.push(Error::new(
                    format!("invalid number: {}", number_str),
                    self.current_span(),
                    self.file_name.clone(),
                    4,
                ));
                None
            }
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
            Token::new(TokenType::Keyword(ident_str), self.current_span())
        } else {
            Token::new(TokenType::Identifier(ident_str), self.current_span())
        }
    }

    fn tokenize_operator(&mut self) -> Option<Token> {
        let first = match self.peek() {
            Some(c) => c,
            None => return None,
        };

        if let Some(second) = self.peak_next() {
            let two = format!("{}{}", first, second);
            let ops = ['+', '-', '*', '/', '%', '=', '<', '>', '&', '|', '.', ','];
            let multi_ops = [
                "==", "!=", "<=", ">=", "+=", "-=", "*=", "/=", "++", "--", "&&", "||", "..",
            ];
            if multi_ops.contains(&two.as_str()) {
                self.advance();
                self.advance();
                return Some(Token::new(TokenType::Operator(two), self.current_span()));
            } else if ops.contains(&first) {
                self.advance();
                return Some(Token::new(
                    TokenType::Operator(first.to_string()),
                    self.current_span(),
                ));
            } else {
                return None;
            }
        }

        None
    }

    fn tokenize_string_literal(&mut self) -> Option<Token> {
        let mut string_lit = String::new();

        self.advance();
        while let Some(char) = self.peek() {
            if char == '"' {
                self.advance();
                return Some(Token::new(
                    TokenType::StringLiteral(string_lit),
                    self.current_span(),
                ));
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
                            if let TokenType::Number(n) = num?.t {
                                string_lit.push(char::from_u32(n.floor() as u32).unwrap_or_else(
                                    || {
                                        self.errors.push(Error::new(
                                            format!("invalid escape sequence: \\{}", escaped),
                                            self.current_span(),
                                            self.file_name.clone(),
                                            5,
                                        ));
                                        '\u{FFFD}'
                                    },
                                ));
                            }

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
        self.errors.push(Error::new(
            "unterminated string literal".to_string(),
            self.current_span(),
            self.file_name.clone(),
            7,
        ));
        None
    }

    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.index)
    }

    fn peak_next(&self) -> Option<char> {
        self.input.chars().nth(self.index + 1)
    }

    fn advance(&mut self) -> Option<char> {
        let old = self.input.chars().nth(self.index);
        self.index += 1;
        if old == Some('\n') {
            self.current_pos.row += 1;
            self.current_pos.column = 1;
        } else {
            self.current_pos.column += 1;
        }
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

    fn current_span(&self) -> Span {
        Span::new(self.start_pos, self.current_pos)
    }

    fn update_start_pos(&mut self) {
        self.start_pos = self.current_pos.clone()
    }
}
