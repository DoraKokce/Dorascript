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
pub struct Token {
    pub t: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(t: TokenType, position: Position) -> Self {
        Token { t, position }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    Operator(String),
    Identifier(String),
    Number(f64),
    Keyword(String),
    StringLiteral(String),
    OpenParen(ParantheseType),
    CloseParen(ParantheseType),
    Semicolon,
}

impl TokenType {
    pub fn value(&self) -> String {
        match self {
            TokenType::EOF => "EOF".to_string(),
            TokenType::Operator(op) => op.clone(),
            TokenType::Identifier(id) => id.clone(),
            TokenType::Number(num) => num.to_string(),
            TokenType::Keyword(kw) => kw.clone(),
            TokenType::StringLiteral(s) => s.clone(),
            TokenType::OpenParen(pt) => match pt {
                ParantheseType::Round => "(".to_string(),
                ParantheseType::Curly => "{".to_string(),
                ParantheseType::Square => "[".to_string(),
            },
            TokenType::CloseParen(pt) => match pt {
                ParantheseType::Round => ")".to_string(),
                ParantheseType::Curly => "}".to_string(),
                ParantheseType::Square => "]".to_string(),
            },
            TokenType::Semicolon => ";".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum BindingPower {
    Eof = -1,
    Default,
    Additive,
    Multiplicative,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Identifier(String),
    StringLiteral(String),
    BinaryOp {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    BlockStmt(Vec<Box<Stmt>>),
    ExprStmt(Expr),
}
