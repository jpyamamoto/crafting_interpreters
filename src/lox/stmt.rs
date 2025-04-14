use super::{expr::Expr, token::Token};
use std::fmt::{Debug, Display};

pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Var(Token, Option<Box<Expr>>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{};", expr),
            Stmt::Print(expr) => write!(f, "print {};", expr),
            Stmt::Var(token, None) => write!(f, "{};", token.lexeme),
            Stmt::Var(token, Some(expr)) => write!(f, "var {} = {};", token.lexeme, expr),
            Stmt::Block(statements) => {
                let mut block_str: String = "{".to_owned();

                for stmt in statements {
                    let stmt_str = format!("{}", stmt);
                    block_str.push_str(&stmt_str);
                }

                block_str.push('}');
                write!(f, "{}", block_str)
            }
            Stmt::If(condition, then_branch, Some(else_branch)) => {
                write!(f, "if ({}) {} else {}", condition, then_branch, else_branch)
            }
            Stmt::If(condition, then_branch, None) => {
                write!(f, "if ({}) {}", condition, then_branch)
            }
            Stmt::While(condition, body) => write!(f, "while ({}) {}", condition, body),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
