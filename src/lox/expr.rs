use std::convert::TryFrom;
use std::fmt::Display;

use super::environment::Environment;
use super::error::Error;
use super::stmt::Stmt;
use super::token::{Token, TokenType};

#[derive(Clone)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, Token),
    Grouping(Box<Expr>),

    // Analog to Literal, since we're not using a separate type to represent the
    // values held by literals.
    Atomic(Literal),
    Logical(Box<Expr>, LogicalOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    Variable(Token),
    Assign(Token, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Comma,
    BangEqual,
    EqualEqual,
    Greater(Token),
    GreaterEqual(Token),
    Less(Token),
    LessEqual(Token),
    Minus(Token),
    Plus(Token),
    Slash(Token),
    Star(Token),
}

#[derive(Debug, Clone, Copy)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Bang,
    Minus(Token),
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub arity: usize,
    pub name: String,
    pub call: fn(&mut Environment, Vec<Literal>) -> Result<Literal, Error>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Token>,
    pub name: Token,
    pub body: Vec<Stmt>,
    pub closure: Environment,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
    True,
    False,
    Nil,
    NativeFunction(NativeFunction),
    Function(Function),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Atomic(literal) => write!(f, "{}", literal),
            Expr::Unary(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Ternary(guard, t, e) => write!(f, "({} ? {} : {})", guard, t, e),
            Expr::Variable(token) => write!(f, "{}", token.lexeme),
            Expr::Assign(token, expr) => write!(f, "{} = {}", token.lexeme, expr),
            Expr::Logical(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Expr::Call(callee, args, _) => {
                let mut args_str: String = String::new();

                for arg in args {
                    let arg_str = format!("{}, ", arg);
                    args_str.push_str(&arg_str);
                }

                if !args_str.is_empty() {
                    args_str.pop();
                    args_str.pop();
                }

                write!(f, "{}({})", callee, args_str)
            }
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Comma => write!(f, ","),
            BinaryOp::BangEqual => write!(f, "!="),
            BinaryOp::EqualEqual => write!(f, "=="),
            BinaryOp::Greater(_) => write!(f, ">"),
            BinaryOp::GreaterEqual(_) => write!(f, ">="),
            BinaryOp::Less(_) => write!(f, "<"),
            BinaryOp::LessEqual(_) => write!(f, "<="),
            BinaryOp::Minus(_) => write!(f, "-"),
            BinaryOp::Plus(_) => write!(f, "+"),
            BinaryOp::Slash(_) => write!(f, "/"),
            BinaryOp::Star(_) => write!(f, "*"),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus(_) => write!(f, "-"),
        }
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::And => write!(f, "and"),
            LogicalOp::Or => write!(f, "or"),
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
            Literal::NativeFunction(native_func) => write!(f, "{}", native_func.name),
            Literal::Function(func) => write!(f, "<fn {}>", func.name.lexeme),
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l), Self::Identifier(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::True, Self::True) => true,
            (Self::False, Self::False) => true,
            (Self::Nil, Self::Nil) => true,
            (Self::NativeFunction(l), Self::NativeFunction(r)) => l.name == r.name,
            _ => false,
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
            _ => Err(Error::Parse {
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
            TokenType::Minus => Ok(UnaryOp::Minus(token.clone())),
            TokenType::Bang => Ok(UnaryOp::Bang),
            _ => Err(Error::Parse {
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
            TokenType::Minus => Ok(BinaryOp::Minus(token.clone())),
            TokenType::Plus => Ok(BinaryOp::Plus(token.clone())),
            TokenType::Slash => Ok(BinaryOp::Slash(token.clone())),
            TokenType::Star => Ok(BinaryOp::Star(token.clone())),
            TokenType::BangEqual => Ok(BinaryOp::BangEqual),
            TokenType::EqualEqual => Ok(BinaryOp::EqualEqual),
            TokenType::Greater => Ok(BinaryOp::Greater(token.clone())),
            TokenType::GreaterEqual => Ok(BinaryOp::GreaterEqual(token.clone())),
            TokenType::Less => Ok(BinaryOp::Less(token.clone())),
            TokenType::LessEqual => Ok(BinaryOp::LessEqual(token.clone())),
            _ => Err(Error::Parse {
                token: token.clone(),
                message: "Not a binary operator".to_string(),
            }),
        }
    }
}

impl TryFrom<&Token> for LogicalOp {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.type_token {
            TokenType::And => Ok(LogicalOp::And),
            TokenType::Or => Ok(LogicalOp::Or),
            _ => Err(Error::Parse {
                token: token.clone(),
                message: "Not a logical operator".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lox::{
        expr::{BinaryOp, Expr, Literal, UnaryOp},
        token::{Token, TokenType},
    };

    #[test]
    fn display_expr() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                UnaryOp::Minus(Token {
                    type_token: TokenType::Minus,
                    lexeme: "-".to_string(),
                    line: 1,
                }),
                Box::new(Expr::Atomic(Literal::Number(123.0))),
            )),
            BinaryOp::Star(Token {
                type_token: TokenType::Star,
                lexeme: "*".to_string(),
                line: 1,
            }),
            Box::new(Expr::Grouping(Box::new(Expr::Atomic(Literal::Number(
                45.67,
            ))))),
        );

        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }
}
