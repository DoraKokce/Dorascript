/*
    Parser is in development. If it has bugs please report to me
*/

use crate::ast::{BindingPower, Expr, Position, Stmt, Token, TokenType};
use crate::errors::ParseError;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<Stmt, Vec<ParseError>> {
        let mut stmts: Vec<Box<Stmt>> = vec![];
        while self.current().clone().unwrap().t != TokenType::EOF {
            stmts.push(Box::new(
                self.parse_stmt()
                    .ok_or_else(|| {
                        self.errors.push(ParseError::new(
                            "Failed to parse statement".to_string(),
                            self.current()
                                .map_or(Position { line: 0, column: 0 }, |t| t.position.clone()),
                        ));
                        self.errors.clone()
                    })?
                    .clone(),
            ));
        }
        if self.errors.is_empty() {
            Ok(Stmt::BlockStmt(stmts))
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let first = self.current().unwrap().clone();
        match first.t {
            _ => {
                let expr = self.parse_expression(BindingPower::Default)?.clone();
                let _ = self.expect(TokenType::Semicolon);
                return Some(Stmt::ExprStmt(expr));
            }
        }
    }

    fn parse_expression(&mut self, limit: BindingPower) -> Option<Expr> {
        let first = self.current()?.clone();
        let mut left = self.nud(&first)?;
        while self.get_precedence(self.current()?.clone().t) > limit {
            let next = self.current()?.clone();
            self.advance();
            left = self.led(left, next)?.clone();
        }
        Some(left)
    }

    fn nud(&mut self, token: &Token) -> Option<Expr> {
        let expr = match &token.t {
            TokenType::Number(n) => Some(Expr::Number(*n)),
            TokenType::Identifier(name) => Some(Expr::Identifier(name.clone())),
            TokenType::StringLiteral(s) => Some(Expr::StringLiteral(s.clone())),
            _ => {
                self.errors.push(ParseError::new(
                    "Unexpected token".to_string(),
                    token.position.clone(),
                ));
                self.advance();
                None
            }
        };
        self.advance();
        expr
    }

    fn led(&mut self, mut left: Expr, token: Token) -> Option<Expr> {
        match token.t.clone() {
            TokenType::Operator(op) => Some(Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(self.parse_expression(self.get_precedence(token.clone().t))?),
            }),
            _ => {
                self.errors.push(ParseError::new(
                    format!("No led handler for token: '{}'", token.t.value()),
                    token.position,
                ));
                None
            }
        }
    }

    fn get_precedence(&self, token: TokenType) -> BindingPower {
        match token.clone() {
            TokenType::Operator(op) => match op.as_str() {
                "+" | "-" => BindingPower::Additive,
                "*" | "/" => BindingPower::Multiplicative,
                _ => BindingPower::Default,
            },
            TokenType::EOF => BindingPower::Eof,
            _ => BindingPower::Default,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn expect(&mut self, expected: TokenType) -> Result<(), String> {
        if let Some(current) = self.current() {
            if current.t == expected {
                self.advance();
                Ok(())
            } else {
                Err(format!(
                    "Expected token {:?} but found {:?}",
                    expected, current
                ))
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn test_parse() {
        let mut lexer = Lexer::new("2+2;".to_string());
        let mut parser = Parser::new(lexer.tokenize().unwrap());

        let mut ast = parser.parse().unwrap();
    }
}
