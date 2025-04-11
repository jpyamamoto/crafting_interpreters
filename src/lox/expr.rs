use std::convert::TryFrom;
use std::fmt::Display;

use super::error::Error;
use super::token::{Token, TokenType};

pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),

    // Analog to Literal, since we're not using a separate type to represent the
    // values held by literals.
    Atomic(Literal),
    Unary(UnaryOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Comma,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    Plus,
    Slash,
    Star,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
    True,
    False,
    Nil,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, op, right) => write!(
                f,
                "({} {} {})",
                op,
                format!("{}", left),
                format!("{}", right)
            ),
            Expr::Grouping(expr) => write!(f, "(group {})", format!("{}", expr)),
            Expr::Atomic(literal) => write!(f, "{}", literal),
            Expr::Unary(op, expr) => write!(f, "({} {})", op, format!("{}", expr)),
            Expr::Ternary(guard, t, e) => write!(f, "({} ? {} : {})", guard, t, e),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Comma => write!(f, ","),
            BinaryOp::BangEqual => write!(f, "!="),
            BinaryOp::EqualEqual => write!(f, "=="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Slash => write!(f, "/"),
            BinaryOp::Star => write!(f, "*"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Identifier(identifier) => write!(f, "{}", identifier),
            Literal::String(string) => write!(f, "{}", string),
            Literal::Number(number) => write!(f, "{}", number),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl TryFrom<&Token> for Literal {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match &token.type_token {
            TokenType::Identifier(v) => Ok(Literal::Identifier(v.clone())),
            TokenType::String(v) => Ok(Literal::String(v.clone())),
            TokenType::Number(v) => Ok(Literal::Number(*v)),
            TokenType::False => Ok(Literal::False),
            TokenType::True => Ok(Literal::True),
            TokenType::Nil => Ok(Literal::Nil),
            _ => Err(Error::ParseError {
                token: token.clone(),
                message: "Not a literal".to_string(),
            }),
        }
    }
}

impl TryFrom<&Token> for UnaryOp {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.type_token {
            TokenType::Minus => Ok(UnaryOp::Minus),
            TokenType::Bang => Ok(UnaryOp::Bang),
            _ => Err(Error::ParseError {
                token: token.clone(),
                message: "Not a unary operator".to_string(),
            }),
        }
    }
}

impl TryFrom<&Token> for BinaryOp {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.type_token {
            TokenType::Comma => Ok(BinaryOp::Comma),
            TokenType::Minus => Ok(BinaryOp::Minus),
            TokenType::Plus => Ok(BinaryOp::Plus),
            TokenType::Slash => Ok(BinaryOp::Slash),
            TokenType::Star => Ok(BinaryOp::Star),
            TokenType::BangEqual => Ok(BinaryOp::BangEqual),
            TokenType::EqualEqual => Ok(BinaryOp::EqualEqual),
            TokenType::Greater => Ok(BinaryOp::Greater),
            TokenType::GreaterEqual => Ok(BinaryOp::GreaterEqual),
            TokenType::Less => Ok(BinaryOp::Less),
            TokenType::LessEqual => Ok(BinaryOp::LessEqual),
            _ => Err(Error::ParseError {
                token: token.clone(),
                message: "Not a binary operator".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lox::expr::{BinaryOp, Expr, Literal, UnaryOp};

    #[test]
    fn display_expr() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                UnaryOp::Minus,
                Box::new(Expr::Atomic(Literal::Number(123.0))),
            )),
            BinaryOp::Star,
            Box::new(Expr::Grouping(Box::new(Expr::Atomic(Literal::Number(
                45.67,
            ))))),
        );

        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }
}
