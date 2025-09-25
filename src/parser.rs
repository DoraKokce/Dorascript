/*
    Parser is in development. If it has bugs please report to me
*/

use std::vec;

use crate::ast::{BindingPower, Expr, ParantheseType, Position, Span, Stmt, Token, TokenType};
use crate::errors::Error;
use crate::lexer::Lexer;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    errors: Vec<Error>,
    file_name: String,
    current_pos: Position,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file_name: String) -> Self {
        Parser {
            tokens,
            index: 0,
            errors: vec![],
            file_name,
            current_pos: Position::zero(),
        }
    }

    pub fn from_str(input: String, file_name: String) -> Result<Self, Vec<Error>> {
        let tokens = Lexer::new(input, file_name.clone()).tokenize();
        if tokens.is_err() {
            Err(tokens.unwrap_err())
        } else {
            Ok(Parser::new(tokens.unwrap_or_default(), file_name.clone()))
        }
    }

    pub fn parse(&mut self) -> Result<Stmt, Vec<Error>> {
        let mut stmts: Vec<Box<Stmt>> = vec![];
        let start = self.current_pos.clone();
        while self.index < self.tokens.len() - 1 {
            match self.parse_stmt() {
                Some(stmt) => stmts.push(Box::new(stmt)),
                None => continue,
            }
        }
        if self.errors.is_empty() {
            Ok(Stmt::BlockStmt(stmts, self.current_span(start)))
        } else {
            Err(self.errors.clone())
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        let first = self.current().unwrap().clone();
        let start = self.current_pos.clone();
        match first.t {
            _ => {
                let expr = self.parse_expression(BindingPower::Default);
                self.expect(TokenType::Semicolon);
                if expr == None {
                    return None;
                }
                return Some(Stmt::ExprStmt(expr.unwrap(), self.current_span(start)));
            }
        }
    }

    fn parse_expression(&mut self, limit: BindingPower) -> Option<Expr> {
        let first = self.current()?.clone();
        let mut left = self.nud(&first)?;
        while self.get_precedence(self.current()?.clone().t) > limit {
            let next = self.current()?.clone();
            left = self.led(left, next)?.clone();
        }
        Some(left)
    }

    fn nud(&mut self, token: &Token) -> Option<Expr> {
        let start = self.current_pos.clone();
        match &token.t {
            TokenType::Number(n) => {
                self.advance();
                Some(Expr::Number(*n, self.current_span(start)))
            }
            TokenType::Identifier(name) => {
                self.advance();
                Some(Expr::Identifier(name.clone(), self.current_span(start)))
            }
            TokenType::StringLiteral(s) => {
                self.advance();
                Some(Expr::StringLiteral(s.clone(), self.current_span(start)))
            }
            TokenType::Operator(op) if op == "-" => {
                self.advance();
                let expr = self.parse_expression(BindingPower::Unary)?;
                Some(Expr::PrefixExpr {
                    op: token.clone(),
                    right: Box::new(expr),
                    span: self.current_span(start),
                })
            }
            TokenType::OpenParen(t) if t == &ParantheseType::Square => {
                let start = self.current_pos.clone();
                self.advance();
                let mut exprs: Vec<Box<Expr>> = vec![];
                while self.current()?.t.value() != "]" {
                    exprs.push(Box::new(self.parse_expression(BindingPower::Default)?));
                    if self.current()?.t.value() == "]" {
                        break;
                    }
                    self.expect(TokenType::Operator(",".to_string()));
                }
                self.expect(TokenType::CloseParen(ParantheseType::Square));
                Some(Expr::ArrayLiteral(exprs, self.current_span(start)))
            }
            _ => {
                self.errors.push(Error::new(
                    format!("unexpected token '{}'", token.t.value()),
                    token.span.clone(),
                    self.file_name.clone(),
                    8,
                ));
                self.advance();
                None
            }
        }
    }

    fn led(&mut self, left: Expr, token: Token) -> Option<Expr> {
        let bp = self.get_precedence(token.clone().t);
        let start = self.current_pos.clone();
        match token.t.clone() {
            TokenType::Operator(op) => {
                self.advance();
                if ["=", "+=", "-=", "*=", "/="].contains(&op.as_str()) {
                    Some(Expr::AssignmentExpr {
                        assigne: Box::new(left),
                        value: Box::new(self.parse_expression(bp)?),
                        span: self.current_span(start),
                    })
                } else if op == "." {
                    Some(Expr::MemberExpr {
                        member: Box::new(left),
                        property: Box::new(self.parse_expression(BindingPower::Default)?),
                        span: self.current_span(start),
                    })
                } else if op == ".." {
                    Some(Expr::RangeExpr {
                        lower: Box::new(left),
                        upper: Box::new(self.parse_expression(BindingPower::Default)?),
                        span: self.current_span(start),
                    })
                } else {
                    Some(Expr::BinaryOp {
                        left: Box::new(left),
                        op: token.clone(),
                        right: Box::new(self.parse_expression(bp)?),
                        span: self.current_span(start),
                    })
                }
            }
            TokenType::OpenParen(t) => {
                self.advance();
                if t == ParantheseType::Square {
                    let expr = self.parse_expression(BindingPower::Default)?;
                    self.expect(TokenType::CloseParen(ParantheseType::Square));
                    Some(Expr::ComputedExpr {
                        member: Box::new(left),
                        property: Box::new(expr),
                        span: self.current_span(start),
                    })
                } else if t == ParantheseType::Round {
                    let mut args: Vec<Box<Expr>> = vec![];
                    while self.current()?.t.value() != ")" {
                        args.push(Box::new(self.parse_expression(BindingPower::Default)?));
                        if self.current()?.t.value() == ")" {
                            break;
                        }
                        self.expect(TokenType::Operator(",".to_string()));
                    }
                    self.expect(TokenType::CloseParen(ParantheseType::Round));
                    Some(Expr::CallExpr {
                        method: Box::new(left),
                        args,
                        span: self.current_span(start),
                    })
                } else {
                    None
                }
            }
            _ => {
                self.errors.push(Error::new(
                    format!("no led handler for token: '{}'", token.t.value()),
                    token.span,
                    self.file_name.clone(),
                    9,
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
                "." => BindingPower::Member,
                ".." | "&&" | "||" => BindingPower::Logical,
                _ => BindingPower::Default,
            },
            TokenType::OpenParen(ParantheseType::Square) => BindingPower::Member,
            TokenType::OpenParen(ParantheseType::Round) => BindingPower::Call,
            TokenType::Number(_) | TokenType::Identifier(_) | TokenType::StringLiteral(_) => {
                BindingPower::Primary
            }
            TokenType::EOF => BindingPower::Eof,
            _ => BindingPower::Default,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn advance(&mut self) {
        self.current_pos = self.current().unwrap_or(&Token::eof()).span.end;
        if self.index < self.tokens.len() {
            self.index += 1;
        }
    }

    fn expect(&mut self, expected: TokenType) -> Option<Token> {
        if let Some(current) = self.current() {
            if current.t.same_type(&expected) {
                let token = current.clone();
                self.advance();
                Some(token)
            } else {
                self.errors.push(Error::new(
                    format!(
                        "expected token '{}' but found '{}'",
                        expected.value(),
                        current.t.value()
                    ),
                    current.span,
                    self.file_name.clone(),
                    10,
                ));
                None
            }
        } else {
            self.errors.push(Error::new(
                "unexpected end of input".to_string(),
                self.current().unwrap_or(&Token::eof()).span,
                self.file_name.clone(),
                11,
            ));
            None
        }
    }

    fn current_span(&self, start: Position) -> Span {
        Span::new(start, self.current_pos)
    }
}
