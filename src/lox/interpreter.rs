use super::environment::Environment;
use super::error::Error;
use super::expr::{BinaryOp, Expr, Literal, LogicalOp, UnaryOp};
use super::stmt::Stmt;
use super::token::Token;

pub fn interpret(statements: Vec<Stmt>) -> Result<(), Error> {
    let mut env = Environment::new();

    for statement in statements {
        execute(&statement, &mut env)?;
    }

    Ok(())
}

fn execute(statement: &Stmt, env: &mut Environment) -> Result<(), Error> {
    match statement {
        Stmt::Expr(expr) => {
            evaluate(expr, env)?;
        }
        Stmt::Print(expr) => {
            let value = evaluate(expr, env)?;
            println!("{}", value);
        }
        Stmt::Var(token, None) => {
            env.define(token, &Literal::Nil);
        }
        Stmt::Var(token, Some(expr)) => {
            let value = evaluate(expr, env)?;
            env.define(token, &value);
        }
        Stmt::Block(statements) => execute_block(statements, env)?,
        Stmt::If(condition, then_branch, else_branch) => {
            execute_if(condition, then_branch, else_branch, env)?
        }
        Stmt::While(condition, body) => execute_while(condition, body, env)?,
    }

    Ok(())
}

fn evaluate(expr: &Expr, env: &mut Environment) -> Result<Literal, Error> {
    match expr {
        Expr::Atomic(literal) => evaluate_literal(literal),
        Expr::Unary(op, expr) => evaluate_unary(op, expr, env),
        Expr::Binary(expr1, op, expr2) => evaluate_binary(op, expr1, expr2, env),
        Expr::Ternary(expr1, expr2, expr3) => evaluate_ternary(expr1, expr2, expr3, env),
        Expr::Grouping(expr) => evaluate(expr, env),
        Expr::Variable(token) => env.get(token),
        Expr::Assign(token, expr) => evaluate_assign(token, expr, env),
        Expr::Logical(expr1, op, expr2) => evaluate_logical(op, expr1, expr2, env),
    }
}

fn evaluate_literal(literal: &Literal) -> Result<Literal, Error> {
    Ok(literal.to_owned())
}

fn evaluate_unary(op: &UnaryOp, expr: &Expr, env: &mut Environment) -> Result<Literal, Error> {
    let right: Literal = evaluate(expr, env)?;

    match op {
        UnaryOp::Bang => {
            if is_truthy(&right) {
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

fn evaluate_binary(
    op: &BinaryOp,
    expr1: &Expr,
    expr2: &Expr,
    env: &mut Environment,
) -> Result<Literal, Error> {
    let left: Literal = evaluate(expr1, env)?;
    let right: Literal = evaluate(expr2, env)?;

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

fn evaluate_ternary(
    expr1: &Expr,
    expr2: &Expr,
    expr3: &Expr,
    env: &mut Environment,
) -> Result<Literal, Error> {
    let guard: Literal = evaluate(expr1, env)?;

    if is_truthy(&guard) {
        evaluate(expr2, env)
    } else {
        evaluate(expr3, env)
    }
}

fn evaluate_assign(token: &Token, expr: &Expr, env: &mut Environment) -> Result<Literal, Error> {
    let value = evaluate(expr, env)?;
    env.assign(token, &value)?;
    Ok(value)
}

fn evaluate_logical(
    op: &LogicalOp,
    expr1: &Expr,
    expr2: &Expr,
    env: &mut Environment,
) -> Result<Literal, Error> {
    let left = evaluate(expr1, env)?;

    match op {
        LogicalOp::Or => {
            if is_truthy(&left) {
                Ok(left)
            } else {
                evaluate(expr2, env)
            }
        }
        LogicalOp::And => {
            if !is_truthy(&left) {
                Ok(left)
            } else {
                evaluate(expr2, env)
            }
        }
    }
}

fn execute_if(
    condition: &Expr,
    then_branch: &Stmt,
    else_branch: &Option<Box<Stmt>>,
    env: &mut Environment,
) -> Result<(), Error> {
    let guard = evaluate(condition, env)?;

    if is_truthy(&guard) {
        execute(then_branch, env)
    } else {
        match else_branch {
            Some(else_stmt) => execute(else_stmt, env),
            None => Ok(()),
        }
    }
}

fn execute_block(statements: &Vec<Stmt>, env: &mut Environment) -> Result<(), Error> {
    let mut block_env = Environment::new_child(env);

    for stmt in statements {
        execute(stmt, &mut block_env)?;
    }

    Ok(())
}

fn execute_while(condition: &Expr, body: &Stmt, env: &mut Environment) -> Result<(), Error> {
    while is_truthy(&evaluate(condition, env)?) {
        execute(body, env)?;
    }

    Ok(())
}

fn is_truthy(expr: &Literal) -> bool {
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
