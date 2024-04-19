use std::{fmt, error};

use super::token::{Token, TokenType};

#[derive(Debug)]
pub enum Error {
    ParseError { token: Token, message: String },
    ScannerError { line: usize, message: String },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseError { token, message } => {
                if token.type_token == TokenType::EOF {
                    write!(f, "[line {}] Parsing Error {}: {}", token.line, " at end".to_string(), message.to_string())
                } else {
                    write!(f, "[line {}] Parsing Error {}: {}", token.line, format!(" at '{}'", token.lexeme), message.to_string())
                }
            },
            Error::ScannerError { line, message } => write!(f, "[line {}] Lexing Error: {}", line, message),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::ParseError { message, .. } => &message,
            Error::ScannerError { message, .. } => &message,
        }
    }
}
