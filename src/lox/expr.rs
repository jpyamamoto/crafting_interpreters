use std::fmt::Display;

use super::token::Token;

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),

    // Analog to Literal, since we're not using a separate type to represent the
    // values held by literals.
    Atomic(Token),
    Unary(Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, op, right) => write!(f, "({} {} {})", op.lexeme, format!("{}", left), format!("{}", right)),
            Expr::Grouping(expr) => write!(f, "(group {})", format!("{}", expr)),
            Expr::Atomic(token) => write!(f, "{}", token.lexeme),
            Expr::Unary(op, expr) => write!(f, "({} {})", op.lexeme, format!("{}", expr)),
            Expr::Ternary(guard, t, e) => write!(f, "({} ? {} : {})", guard, t, e),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::Expr;
    use crate::token::{Token, TokenType};

    #[test]
    fn display_expr() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                Token { type_token: TokenType::Minus, lexeme: "-".to_string(), line: 1 },
                Box::new(Expr::Atomic(
                    Token { type_token: TokenType::Number(123.0), lexeme: "123".to_string(), line: 1 })))),

            Token { type_token: TokenType::Star, lexeme: "*".to_string(), line: 1 },

            Box::new(Expr::Grouping(
                Box::new(Expr::Atomic(
                    Token { type_token: TokenType::Number(45.67), lexeme: "45.67".to_string(), line: 1 })))));

        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }
}
