#[derive(Debug, Clone, PartialEq)]
pub enum ParantheseType {
    Round,
    Curly,
    Square,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF,
    Operator(String),
    Identifier(String),
    Number(f64),
    Keyword(String),
    StringLiteral(String),
    OpenParen(ParantheseType),
    CloseParen(ParantheseType),
}
