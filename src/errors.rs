use crate::ast::Position;
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub position: Position,
    pub file_name: String,
    pub err_code: u16,
}

impl Error {
    pub fn new(message: String, position: Position, file_name: String, err_code: u16) -> Self {
        Error {
            message,
            position,
            file_name,
            err_code,
        }
    }
    pub fn print(&self) {
        eprintln!(
            "{} {}\n\t{}:{}:{}",
            format!("[E{:04}]:", self.err_code).red().bold(),
            self.message.bold(),
            self.file_name,
            self.position.row,
            self.position.column
        );
    }
}
