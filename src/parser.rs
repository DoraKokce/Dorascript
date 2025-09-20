use crate::ast::Token;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    operator_binding_power: fn(&str) -> Option<(u8, u8)>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            operator_binding_power: |op| match op {
                "+" | "-" => Some((10, 11)),
                "*" | "/" => Some((20, 21)),
                "=" => Some((5, 4)),
                _ => None,
            },
        }
    }

    pub fn parse(&mut self) {}
}
