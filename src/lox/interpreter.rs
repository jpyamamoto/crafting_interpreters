use std::time::{SystemTime, UNIX_EPOCH};

use super::environment::Environment;
use super::error::Error;
use super::expr::{BinaryOp, Expr, Function, Literal, LogicalOp, NativeFunction, UnaryOp};
use super::stmt::Stmt;
use super::token::Token;

type ExecResult = Result<Option<Literal>, Error>;
type EvalResult = Result<Literal, Error>;

pub fn interpret(statements: Vec<Stmt>) -> ExecResult {
    let mut env = Environment::new();

    env.define(
        "clock".to_string(),
        &Literal::NativeFunction(NativeFunction {
            arity: 0,
            name: "clock".to_string(),
            call: |_env, _args| {
                let start = SystemTime::now();
                let seconds = start
                    .duration_since(UNIX_EPOCH)
                    .map(|t| t.as_secs_f64())
                    .unwrap_or_else(|t| -t.duration().as_secs_f64());
                Ok(Literal::Number(seconds))
            },
        }),
    );

    for statement in statements {
        execute(&statement, &mut env)?;
    }

    Ok(None)
}

fn execute(statement: &Stmt, env: &mut Environment) -> ExecResult {
    match statement {
        Stmt::Expr(expr) => {
            evaluate(expr, env)?;
            Ok(None)
        }
        Stmt::Print(expr) => {
            let value = evaluate(expr, env)?;
            println!("{}", value);
            Ok(None)
        }
        Stmt::Var(token, None) => {
            env.define(token.lexeme.clone(), &Literal::Nil);
            Ok(None)
        }
        Stmt::Var(token, Some(expr)) => {
            let value = evaluate(expr, env)?;
            env.define(token.lexeme.clone(), &value);
            Ok(None)
        }
        Stmt::Block(statements) => Ok(execute_block(statements, env)?),
        Stmt::If(condition, then_branch, else_branch) => {
            Ok(execute_if(condition, then_branch, else_branch, env)?)
        }
        Stmt::While(condition, body) => Ok(execute_while(condition, body, env)?),
        Stmt::Function(name, params, body) => {
            Ok(execute_func(name, params.to_owned(), body.to_owned(), env)?)
        }
        Stmt::Return(_, expr) => execute_return(expr, env).map(Option::from),
    }
}

fn evaluate(expr: &Expr, env: &mut Environment) -> EvalResult {
    match expr {
        Expr::Atomic(literal) => evaluate_literal(literal),
        Expr::Unary(op, expr) => evaluate_unary(op, expr, env),
        Expr::Binary(expr1, op, expr2) => evaluate_binary(op, expr1, expr2, env),
        Expr::Ternary(expr1, expr2, expr3) => evaluate_ternary(expr1, expr2, expr3, env),
        Expr::Grouping(expr) => evaluate(expr, env),
        Expr::Variable(token) => env.get(token),
        Expr::Assign(token, expr) => evaluate_assign(token, expr, env),
        Expr::Logical(expr1, op, expr2) => evaluate_logical(op, expr1, expr2, env),
        Expr::Call(callee, arguments, paren) => evaluate_call(callee, arguments, paren, env),
    }
}

fn evaluate_literal(literal: &Literal) -> EvalResult {
    Ok(literal.to_owned())
}

fn evaluate_unary(op: &UnaryOp, expr: &Expr, env: &mut Environment) -> EvalResult {
    let right: Literal = evaluate(expr, env)?;

    match op {
        UnaryOp::Bang => {
            if is_truthy(&right) {
                Ok(Literal::False)
            } else {
                Ok(Literal::True)
            }
        }
        UnaryOp::Minus(token) => {
            if let Literal::Number(n) = right {
                Ok(Literal::Number(-n))
            } else {
                Err(Error::Eval {
                    token: token.clone(),
                    message: "Invalid negation: not a number".into(),
                })
            }
        }
    }
}

fn evaluate_binary(op: &BinaryOp, expr1: &Expr, expr2: &Expr, env: &mut Environment) -> EvalResult {
    let left: Literal = evaluate(expr1, env)?;
    let right: Literal = evaluate(expr2, env)?;

    match op {
        BinaryOp::Minus(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid subtraction: operands must be numbers".into(),
            })?;

            Ok(Literal::Number(left_num - right_num))
        }
        BinaryOp::Slash(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid division: operands must be numbers".into(),
            })?;

            if right_num == 0.0 {
                Err(Error::Eval {
                    token: token.clone(),
                    message: "Invalid division: division by zero".into(),
                })
            } else {
                Ok(Literal::Number(left_num / right_num))
            }
        }
        BinaryOp::Star(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid multiplication: operands must be numbers".into(),
            })?;

            Ok(Literal::Number(left_num * right_num))
        }
        BinaryOp::Plus(token) => match left {
            Literal::Number(left_num) => {
                if let Literal::Number(right_num) = right {
                    Ok(Literal::Number(left_num + right_num))
                } else {
                    Err(Error::Eval {
                        token: token.clone(),
                        message: "Invalid addition: operands must be numbers".into(),
                    })
                }
            }
            Literal::String(left_str) => {
                if let Literal::String(right_str) = right {
                    Ok(Literal::String(left_str + &right_str))
                } else {
                    Err(Error::Eval {
                        token: token.clone(),
                        message: "Invalid concatenation: operands must be strings".into(),
                    })
                }
            }
            _ => Err(Error::Eval {
                token: token.clone(),
                message: "Invalid addition: operands must be numbers".into(),
            }),
        },
        BinaryOp::Greater(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid > comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num > right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::GreaterEqual(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid >= comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num >= right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::Less(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
                message: "Invalid < comparison: operands must be numbers".into(),
            })?;

            Ok(if left_num < right_num {
                Literal::True
            } else {
                Literal::False
            })
        }
        BinaryOp::LessEqual(token) => {
            let (left_num, right_num) = retrieve_nums(left, right).ok_or(Error::Eval {
                token: token.clone(),
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

fn evaluate_ternary(expr1: &Expr, expr2: &Expr, expr3: &Expr, env: &mut Environment) -> EvalResult {
    let guard: Literal = evaluate(expr1, env)?;

    if is_truthy(&guard) {
        evaluate(expr2, env)
    } else {
        evaluate(expr3, env)
    }
}

fn evaluate_assign(token: &Token, expr: &Expr, env: &mut Environment) -> EvalResult {
    let value = evaluate(expr, env)?;
    env.assign(token, &value)?;
    Ok(value)
}

fn evaluate_logical(
    op: &LogicalOp,
    expr1: &Expr,
    expr2: &Expr,
    env: &mut Environment,
) -> EvalResult {
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

fn evaluate_call(
    callee_expr: &Expr,
    arguments_expr: &Vec<Expr>,
    end_token: &Token,
    env: &mut Environment,
) -> EvalResult {
    let callee = evaluate(callee_expr, env)?;

    let mut arguments: Vec<Literal> = vec![];

    for arg_expr in arguments_expr {
        let arg = evaluate(arg_expr, env)?;
        arguments.push(arg);
    }

    match callee {
        Literal::NativeFunction(callable) => {
            if callable.arity != arguments.len() {
                Err(Error::Eval {
                    token: end_token.clone(),
                    message: format!(
                        "Expected {} arguments but got {}.",
                        callable.arity,
                        arguments.len()
                    ),
                })
            } else {
                (callable.call)(env, arguments)
            }
        }
        Literal::Function(callable) => {
            if callable.params.len() != arguments.len() {
                return Err(Error::Eval {
                    token: end_token.clone(),
                    message: format!(
                        "Expected {} arguments but got {}.",
                        callable.params.len(),
                        arguments.len()
                    ),
                });
            }

            // let globals = env.get_globals();
            let parent = callable.closure;
            let mut env = Environment::new_child(&parent);

            for (param, arg) in callable.params.iter().zip(arguments) {
                env.define(param.lexeme.clone(), &arg);
            }

            let result = execute_block(&callable.body, &mut env)?;

            Ok(result.unwrap_or(Literal::Nil))
        }
        _ => Err(Error::Eval {
            token: end_token.clone(),
            message: "Can only call functions and classes.".to_string(),
        }),
    }
}

fn execute_if(
    condition: &Expr,
    then_branch: &Stmt,
    else_branch: &Option<Box<Stmt>>,
    env: &mut Environment,
) -> ExecResult {
    let guard = evaluate(condition, env)?;

    if is_truthy(&guard) {
        execute(then_branch, env)
    } else {
        match else_branch {
            Some(else_stmt) => execute(else_stmt, env),
            None => Ok(None),
        }
    }
}

fn execute_block(statements: &Vec<Stmt>, env: &mut Environment) -> ExecResult {
    let mut block_env = Environment::new_child(env);

    for stmt in statements {
        let result = execute(stmt, &mut block_env)?;

        if let Some(return_lit) = result {
            return Ok(Some(return_lit));
        }
    }

    Ok(None)
}

fn execute_while(condition: &Expr, body: &Stmt, env: &mut Environment) -> ExecResult {
    while is_truthy(&evaluate(condition, env)?) {
        let result = execute(body, env)?;

        if let Some(return_lit) = result {
            return Ok(Some(return_lit));
        }
    }

    Ok(None)
}

fn execute_func(
    name: &Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
    env: &mut Environment,
) -> ExecResult {
    let func = Function {
        params,
        name: name.to_owned(),
        body,
        closure: env.to_owned(),
    };

    env.define(name.lexeme.clone(), &Literal::Function(func));
    Ok(None)
}

fn execute_return(expr: &Expr, env: &mut Environment) -> ExecResult {
    evaluate(expr, env).map(Option::from)
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
