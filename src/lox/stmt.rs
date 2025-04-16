use super::{expr::Expr, token::Token};
use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Stmt {
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Return(Token, Box<Expr>),
    Var(Token, Option<Box<Expr>>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Function(Token, Vec<Token>, Vec<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{};", expr),
            Stmt::Print(expr) => write!(f, "print {};", expr),
            Stmt::Return(token, expr) => write!(f, "{} {};", token.lexeme, expr),
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
            Stmt::Function(name, params, body) => {
                let mut params_str: String = String::new();

                for param in params {
                    let param_str = format!("{}, ", param.lexeme);
                    params_str.push_str(&param_str);
                }

                if !params_str.is_empty() {
                    params_str.pop();
                    params_str.pop();
                }

                let mut block_str: String = "{".to_string();

                for stmt in body {
                    let stmt_str = format!("{}", stmt);
                    block_str.push_str(&stmt_str);
                }

                block_str.push('}');
                write!(f, "{}({}){}", name.lexeme, params_str, block_str)
            }
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
