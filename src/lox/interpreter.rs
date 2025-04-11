use super::error::Error;
use super::expr::{BinaryOp, Expr, Literal, UnaryOp};

pub fn interpret(expr: &Expr) -> Result<Literal, Error> {
    evaluate(expr)
}

fn evaluate(expr: &Expr) -> Result<Literal, Error> {
    match expr {
        Expr::Atomic(literal) => evaluate_literal(literal),
        Expr::Unary(op, expr) => evaluate_unary(op, expr),
        Expr::Binary(expr1, op, expr2) => evaluate_binary(op, expr1, expr2),
        Expr::Ternary(expr1, expr2, expr3) => evaluate_ternary(expr1, expr2, expr3),
        Expr::Grouping(expr) => evaluate(expr),
    }
}

fn evaluate_literal(literal: &Literal) -> Result<Literal, Error> {
    Ok(literal.to_owned())
}

fn evaluate_unary(op: &UnaryOp, expr: &Expr) -> Result<Literal, Error> {
    let right: Literal = evaluate(expr)?;

    match op {
        UnaryOp::Bang => {
            if is_truthy(right) {
                Ok(Literal::False)
            } else {
                Ok(Literal::True)
            }
        }
        UnaryOp::Minus => {
            if let Literal::Number(n) = right {
                Ok(Literal::Number(-n))
            } else {
                Err(Error::Eval {
                    message: "Invalid negation: not a number".into(),
                })
            }
        }
    }
}

fn evaluate_binary(op: &BinaryOp, expr1: &Expr, expr2: &Expr) -> Result<Literal, Error> {
    let left: Literal = evaluate(expr1)?;
    let right: Literal = evaluate(expr2)?;

    match op {
        BinaryOp::Minus => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid subtraction: operands must be numbers".into(),
            })?;

            Ok(Literal::Number(left_num - right_num))
        }
        BinaryOp::Slash => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid division: operands must be numbers".into(),
            })?;

            if right_num == 0.0 {
                Err(Error::Eval {
                    message: "Invalid division: division by zero".into(),
                })
            } else {
                Ok(Literal::Number(left_num / right_num))
            }
        }
        BinaryOp::Star => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid multiplication: operands must be numbers".into(),
            })?;

            Ok(Literal::Number(left_num * right_num))
        }
        BinaryOp::Plus => match left {
            Literal::Number(left_num) => {
                if let Literal::Number(right_num) = right {
                    Ok(Literal::Number(left_num + right_num))
                } else {
                    Err(Error::Eval {
                        message: "Invalid addition: operands must be numbers".into(),
                    })
                }
            }
            Literal::String(left_str) => {
                if let Literal::String(right_str) = right {
                    Ok(Literal::String(left_str + &right_str))
                } else {
                    Err(Error::Eval {
                        message: "Invalid concatenation: operands must be strings".into(),
                    })
                }
            }
            _ => Err(Error::Eval {
                message: "Invalid addition: operands must be numbers".into(),
            }),
        },
        BinaryOp::Greater => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid > comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num > right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::GreaterEqual => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid >= comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num >= right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::Less => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid < comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num < right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::LessEqual => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                message: "Invalid <= comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num <= right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::BangEqual => Ok(if !is_equal(&left, &right) {
            Literal::True
        } else {
            Literal::False
        }),
        BinaryOp::EqualEqual => Ok(if is_equal(&left, &right) {
            Literal::True
        } else {
            Literal::False
        }),
        BinaryOp::Comma => Ok(right),
    }
}

fn evaluate_ternary(expr1: &Expr, expr2: &Expr, expr3: &Expr) -> Result<Literal, Error> {
    let guard: Literal = evaluate(expr1)?;

    if is_truthy(guard) {
        evaluate(expr2)
    } else {
        evaluate(expr3)
    }
}

fn is_truthy(expr: Literal) -> bool {
    !matches!(expr, Literal::Nil | Literal::False)
}

fn retrieve_nums(expr1: Literal, expr2: Literal) -> Option<(f64, f64)> {
    if let (Literal::Number(expr1_num), Literal::Number(expr2_num)) = (expr1, expr2) {
        Some((expr1_num, expr2_num))
    } else {
        None
    }
}

fn is_equal(left: &Literal, right: &Literal) -> bool {
    // This function implements the same behaviour of Java (the language of the original Lox
    // implementation) when dealing with floating-point numbers.
    if let (Literal::Number(left_num), Literal::Number(right_num)) = (left, right) {
        if left_num.is_nan() && right_num.is_nan() {
            true
        } else {
            left_num.to_bits() == right_num.to_bits()
        }
    } else {
        left == right
    }
}
