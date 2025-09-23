/*
    Parser is in development. If it has bugs please report to me
*/

use std::vec;

use crate::ast::{BindingPower, Expr, Stmt, Token, TokenType};
use crate::errors::{LexError, ParseError};
use crate::lexer::Lexer;

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

    pub fn from_str(input: String) -> Result<Self, Vec<LexError>> {
        let tokens = Lexer::new(input).tokenize();
        if tokens.is_err() {
            Err(tokens.unwrap_err())
        } else {
            Ok(Parser::new(tokens.unwrap_or_default()))
        }
    }

    pub fn parse(&mut self) -> Result<Stmt, Vec<ParseError>> {
        let mut stmts: Vec<Box<Stmt>> = vec![];
        while self.position < self.tokens.len() - 1 {
            match self.parse_stmt() {
                Some(stmt) => stmts.push(Box::new(stmt)),
                None => continue,
            }
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
                let expr = self.parse_expression(BindingPower::Default);
                self.expect(TokenType::Semicolon);
                if expr == None {
                    return None;
                }
                return Some(Stmt::ExprStmt(expr.unwrap()));
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
        match &token.t {
            TokenType::Number(n) => {
                self.advance();
                Some(Expr::Number(*n))
            }
            TokenType::Identifier(name) => {
                self.advance();
                Some(Expr::Identifier(name.clone()))
            }
            TokenType::StringLiteral(s) => {
                self.advance();
                Some(Expr::StringLiteral(s.clone()))
            }
            TokenType::Operator(op) if op == "-" => {
                self.advance();
                let expr = self.parse_expression(BindingPower::Unary)?;
                Some(Expr::PrefixExpr {
                    op: token.clone(),
                    right: Box::new(expr),
                })
            }
            _ => {
                self.errors.push(ParseError::new(
                    format!("Unexpected token {:?}", token.t),
                    token.position.clone(),
                ));
                self.advance();
                None
            }
        }
    }

    fn led(&mut self, left: Expr, token: Token) -> Option<Expr> {
        let bp = self.get_precedence(token.clone().t);
        match token.t.clone() {
            TokenType::Operator(op) => {
                if op == "=".to_owned() {
                    Some(Expr::AssignmentExpr {
                        assigne: Box::new(left),
                        value: Box::new(self.parse_expression(bp)?),
                    })
                } else {
                    Some(Expr::BinaryOp {
                        left: Box::new(left),
                        op: token.clone(),
                        right: Box::new(self.parse_expression(bp)?),
                    })
                }
            }
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
                "=" => BindingPower::Assignment,
                _ => BindingPower::Default,
            },
            TokenType::Number(_) | TokenType::Identifier(_) | TokenType::StringLiteral(_) => {
                BindingPower::Primary
            }
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

    fn expect(&mut self, expected: TokenType) -> Option<Token> {
        if let Some(current) = self.current() {
            if current.t == expected {
                let token = current.clone();
                self.advance();
                Some(token)
            } else {
                self.errors.push(ParseError::new(
                    format!("Expected token {:?} but found {:?}", expected, current),
                    current.position,
                ));
                None
            }
        } else {
            self.errors.push(ParseError::new(
                "Unexpected end of input".to_string(),
                self.current().unwrap_or(&Token::eof()).position,
            ));
            None
        }
    }
}
