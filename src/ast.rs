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

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl Position {
    pub fn zero() -> Self {
        Position { row: 0, column: 0 }
    }
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

    pub fn eof() -> Self {
        Token {
            t: TokenType::EOF,
            position: Position::zero(),
        }
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum BindingPower {
    Eof = 0,
    Default,
    Assignment,
    Additive,
    Multiplicative,
    Unary,
    Primary,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Identifier(String),
    StringLiteral(String),
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    AssignmentExpr {
        assigne: Box<Expr>,
        value: Box<Expr>,
    },
    PrefixExpr {
        op: Token,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn format(&self, prefix: &str, last: bool) -> String {
        let mut result = String::new();
        let connector = if last { "└── " } else { "├── " };
        let padding = if last { "    " } else { "│   " };

        match self {
            Expr::Number(n) => result += &format!("{}{}Number({})\n", prefix, connector, n),
            Expr::Identifier(name) => {
                result += &format!("{}{}Identifier({})\n", prefix, connector, name)
            }
            Expr::StringLiteral(s) => result += &format!("{}{}String({})\n", prefix, connector, s),
            Expr::PrefixExpr { op, right } => {
                result += &format!("{}{}PrefixExpr({:?})\n", prefix, connector, op);
                result += &right.format(&(prefix.to_string() + padding), true);
            }
            Expr::BinaryOp { left, op, right } => {
                result += &format!("{}{}BinaryOp({:?})\n", prefix, connector, op);
                result += &left.format(&(prefix.to_string() + padding), false);
                result += &right.format(&(prefix.to_string() + padding), true);
            }
            Expr::AssignmentExpr { assigne, value } => {
                result += &format!("{}{}AssignmentExpr\n", prefix, connector);
                result += &assigne.format(&(prefix.to_string() + padding), false);
                result += &value.format(&(prefix.to_string() + padding), true);
            }
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    BlockStmt(Vec<Box<Stmt>>),
    ExprStmt(Expr),
}

impl Stmt {
    pub fn format(&self, prefix: &str, last: bool) -> String {
        let mut result = String::new();
        let connector = if last { "└── " } else { "├── " };
        let padding = if last { "    " } else { "│   " };

        match self {
            Stmt::ExprStmt(expr) => {
                result += &format!("{}{}ExprStmt\n", prefix, connector);
                result += &expr.format(&(prefix.to_string() + padding), true);
            }
            Stmt::BlockStmt(stmts) => {
                result += &format!("{}{}BlockStmt\n", prefix, connector);
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == stmts.len() - 1;
                    result += &stmt.format(&(prefix.to_string() + padding), is_last);
                }
            }
        }

        result
    }
}
