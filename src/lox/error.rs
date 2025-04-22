use std::{error, fmt};

use super::token::{Token, TokenType};

#[derive(Debug)]
pub enum Error {
    Parse { token: Token, message: String },
    Report { token: Token, message: String },
    Scanner { line: usize, message: String },
    Eval { token: Token, message: String },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Parse { token, message } => {
                if token.type_token == TokenType::Eof {
                    write!(f, "[line {}] Parsing Error at end: {}", token.line, message)
                } else {
                    write!(
                        f,
                        "[line {}] Parsing Error at '{}': {}",
                        token.line, token.lexeme, message
                    )
                }
            }
            Error::Report { token, message } => {
                if token.type_token == TokenType::Eof {
                    write!(f, "[line {}] Error at end: {}", token.line, message)
                } else {
                    write!(
                        f,
                        "[line {}] Error at '{}': {}",
                        token.line, token.lexeme, message
                    )
                }
            }
            Error::Scanner { line, message } => {
                write!(f, "[line {}] Lexing Error: {}", line, message)
            }
            Error::Eval { token, message } => {
                write!(f, "[line {}] Eval Error: {}", token.line, message)
            }
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::Parse { message, .. } => message,
            Error::Report { message, .. } => message,
            Error::Scanner { message, .. } => message,
            Error::Eval { message, .. } => message,
        }
    }
}
