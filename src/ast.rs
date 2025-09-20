/*
    Abstract Syntax Tree (AST) definitions.
    Includes token types, expressions and statements.
*/

/* Token Types */
#[derive(Debug, Clone, PartialEq)]
pub enum ParantheseType {
    Round,
    Curly,
    Square,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF(Position),
    Operator(String, Position),
    Identifier(String, Position),
    Number(f64, Position),
    Keyword(String, Position),
    StringLiteral(String, Position),
    OpenParen(ParantheseType, Position),
    CloseParen(ParantheseType, Position),
}

/* Expressions */
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Identifier(String),
    StringLiteral(String),
    BinaryOp {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Assignment {
        identifier: String,
        value: Box<Expr>,
    },
    ParenExpr {
        paren_type: ParantheseType,
        expr: Box<Expr>,
    },
}

/* Statements */

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    BlockStmt(Vec<Stmt>),
}
