/*
    Abstract Syntax Tree (AST) definitions.
    Includes token types, expressions and statements.
*/

use colored::Colorize;

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl Position {
    pub fn zero() -> Self {
        Position { row: 1, column: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    pub fn zero() -> Self {
        Span {
            start: Position::zero(),
            end: Position::zero(),
        }
    }

    pub fn format(&self, prefix: &str, last: bool) -> String {
        let mut result = String::new();
        let connector = if last { "└── " } else { "├── " };

        result += &format!(
            "{}{}From {}:{} to {}:{}\n",
            prefix, connector, self.start.row, self.start.column, self.end.row, self.end.column
        )
        .dimmed();
        result
    }
}

/* Token Types */
#[derive(Debug, Clone, PartialEq)]
pub enum ParantheseType {
    Round,
    Curly,
    Square,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub t: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(t: TokenType, span: Span) -> Self {
        Token { t, span }
    }

    pub fn eof() -> Self {
        Token {
            t: TokenType::EOF,
            span: Span::zero(),
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
    pub fn same_type(&self, other: &Self) -> bool {
        use TokenType::*;
        match (self, other) {
            (Identifier(_), Identifier(_)) => true,
            (Number(_), Number(_)) => true,
            (StringLiteral(_), StringLiteral(_)) => true,
            (Operator(a), Operator(b)) => a == b, // keep exact match for operators
            (OpenParen(a), OpenParen(b)) => a == b,
            (CloseParen(a), CloseParen(b)) => a == b,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
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
    Logical,
    Additive,
    Multiplicative,
    Unary,
    Call,
    Member,
    Primary,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64, Span),
    Identifier(String, Span),
    StringLiteral(String, Span),
    ArrayLiteral(Vec<Box<Expr>>, Span),
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
        span: Span,
    },
    AssignmentExpr {
        assigne: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    PrefixExpr {
        op: Token,
        right: Box<Expr>,
        span: Span,
    },
    MemberExpr {
        member: Box<Expr>,
        property: Box<Expr>,
        span: Span,
    },
    ComputedExpr {
        member: Box<Expr>,
        property: Box<Expr>,
        span: Span,
    },
    RangeExpr {
        lower: Box<Expr>,
        upper: Box<Expr>,
        span: Span,
    },
    CallExpr {
        method: Box<Expr>,
        args: Vec<Box<Expr>>,
        span: Span,
    },
}

impl Expr {
    pub fn format(&self, prefix: &str, last: bool) -> String {
        let mut result = String::new();
        let connector = if last { "└── " } else { "├── " };
        let padding = if last { "    " } else { "│   " };

        match self {
            Expr::Number(n, span) => {
                result += &format!("{}{}Number: {}\n", prefix, connector, n);
                result += &span.format(&(prefix.to_string() + padding), true);
            }
            Expr::Identifier(name, span) => {
                result += &format!("{}{}Identifier: {}\n", prefix, connector, name);
                result += &span.format(&(prefix.to_string() + padding), true);
            }
            Expr::StringLiteral(s, span) => {
                result += &format!("{}{}String: {}\n", prefix, connector, s);
                result += &span.format(&(prefix.to_string() + padding), true);
            }
            Expr::ArrayLiteral(exprs, span) => {
                result += &format!("{}{}ArrayLiteral\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                for (i, expr) in exprs.iter().enumerate() {
                    let is_last = i == exprs.len() - 1;
                    result += &expr.format(&(prefix.to_string() + padding), is_last);
                }
            }
            Expr::BinaryOp {
                left,
                op,
                right,
                span,
            } => {
                result += &format!("{}{}BinaryOp({})\n", prefix, connector, op.t.value());
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &left.format(&(prefix.to_string() + padding), false);
                result += &right.format(&(prefix.to_string() + padding), true);
            }
            Expr::AssignmentExpr {
                assigne,
                value,
                span,
            } => {
                result += &format!("{}{}AssignmentExpr\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &assigne.format(&(prefix.to_string() + padding), false);
                result += &value.format(&(prefix.to_string() + padding), true);
            }
            Expr::PrefixExpr { op, right, span } => {
                result += &format!("{}{}PrefixExpr({})\n", prefix, connector, op.t.value());
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &right.format(&(prefix.to_string() + padding), true);
            }
            Expr::MemberExpr {
                member,
                property,
                span,
            } => {
                result += &format!("{}{}MemberExpr\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &member.format(&(prefix.to_string() + padding), false);
                result += &property.format(&(prefix.to_string() + padding), true);
            }
            Expr::ComputedExpr {
                member,
                property,
                span,
            } => {
                result += &format!("{}{}ComputedExpr\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &member.format(&(prefix.to_string() + padding), false);
                result += &property.format(&(prefix.to_string() + padding), true);
            }
            Expr::RangeExpr { lower, upper, span } => {
                result += &format!("{}{}RangeExpr\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &lower.format(&(prefix.to_string() + padding), false);
                result += &upper.format(&(prefix.to_string() + padding), true);
            }
            Expr::CallExpr { method, args, span } => {
                result += &format!("{}{}CallExpr\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &method.format(&(prefix.to_string() + padding), false);
                if !args.is_empty() {
                    result += &format!("{}└── Args\n", prefix.to_string() + padding);
                    for (i, arg) in args.iter().enumerate() {
                        let is_last = i == args.len() - 1;
                        result += &arg.format(&(prefix.to_string() + padding + padding), is_last);
                    }
                }
            }
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    BlockStmt(Vec<Box<Stmt>>, Span),
    ExprStmt(Expr, Span),
}

impl Stmt {
    pub fn format(&self, prefix: &str, last: bool) -> String {
        let mut result = String::new();
        let connector = if last { "└── " } else { "├── " };
        let padding = if last { "    " } else { "│   " };

        match self {
            Stmt::ExprStmt(expr, span) => {
                result += &format!("{}{}ExprStmt\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                result += &expr.format(&(prefix.to_string() + padding), true);
            }
            Stmt::BlockStmt(stmts, span) => {
                result += &format!("{}{}BlockStmt\n", prefix, connector);
                result += &span.format(&(prefix.to_string() + padding), false);
                for (i, stmt) in stmts.iter().enumerate() {
                    let is_last = i == stmts.len() - 1;
                    result += &stmt.format(&(prefix.to_string() + padding), is_last);
                }
            }
        }

        result
    }
}
