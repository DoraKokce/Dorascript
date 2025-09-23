use crate::ast::Position;

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub position: Position,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub position: Position,
}

impl LexError {
    pub fn new(message: String, position: Position) -> Self {
        LexError { message, position }
    }

    pub fn print(&self) {
        eprintln!(
            "Lexer error at line {}, column {}:\n\t{}",
            self.position.row, self.position.column, self.message
        );
    }
}

impl ParseError {
    pub fn new(message: String, position: Position) -> Self {
        ParseError { message, position }
    }

    pub fn print(&self) {
        eprintln!(
            "Parser error at line {}, column {}:\n\t{}",
            self.position.row, self.position.column, self.message
        );
    }
}
