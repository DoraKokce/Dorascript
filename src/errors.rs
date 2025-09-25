use crate::ast::Span;
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub span: Span,
    pub file_name: String,
    pub err_code: u16,
}

impl Error {
    pub fn new(message: String, span: Span, file_name: String, err_code: u16) -> Self {
        Error {
            message,
            span,
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
            self.span.start.row,
            self.span.start.column
        );
    }
}
