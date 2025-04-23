use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use ordered_float::OrderedFloat;

use super::environment::Environment;
use super::error::Error;
use super::expr::{
    BinaryOp, Class, Expr, Function, Instance, Literal, LogicalOp, NativeFunction, UnaryOp,
};
use super::resolver::Locals;
use super::stmt::{FuncContainer, Stmt};
use super::token::Token;

type ExecResult = Result<Option<Literal>, Error>;
type EvalResult = Result<Literal, Error>;

struct State {
    env: Environment,
    locals: Locals,
}

pub fn interpret(statements: Vec<Stmt>, locals: Locals) -> ExecResult {
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
                Ok(Literal::Number(seconds.into()))
            },
        }),
    );

    let mut state = State { env, locals };

    for statement in statements {
        execute(&statement, &mut state)?;
    }

    Ok(None)
}

fn execute(statement: &Stmt, state: &mut State) -> ExecResult {
    match statement {
        Stmt::Expr(expr) => {
            evaluate(expr, state)?;
            Ok(None)
        }
        Stmt::Print(expr) => {
            let value = evaluate(expr, state)?;
            println!("{}", value);
            Ok(None)
        }
        Stmt::Var(token, None) => {
            state.env.define(token.lexeme.clone(), &Literal::Nil);
            Ok(None)
        }
        Stmt::Var(token, Some(expr)) => {
            let value = evaluate(expr, state)?;
            state.env.define(token.lexeme.clone(), &value);
            Ok(None)
        }
        Stmt::Block(statements) => {
            let block_env = Environment::new_child(&state.env);
            let mut block_state = State {
                env: block_env,
                locals: state.locals.clone(),
            };

            Ok(execute_block(statements, &mut block_state)?)
        }
        Stmt::If(condition, then_branch, else_branch) => {
            Ok(execute_if(condition, then_branch, else_branch, state)?)
        }
        Stmt::While(condition, body) => Ok(execute_while(condition, body, state)?),
        Stmt::Function(FuncContainer { name, params, body }) => Ok(execute_func(
            name,
            params.to_owned(),
            body.to_owned(),
            state,
        )?),
        Stmt::Return(_, expr) => execute_return(expr, state).map(Option::from),
        Stmt::Class(name, methods) => {
            state.env.define(name.lexeme.clone(), &Literal::Nil);

            let mut methods_def: HashMap<String, Function> = HashMap::new();

            for method in methods {
                let func = Function {
                    params: method.params.to_owned(),
                    name: method.name.to_owned(),
                    body: method.body.to_owned(),
                    closure: state.env.to_owned(),
                };

                methods_def.insert(method.name.lexeme.clone(), func);
            }

            let class: Class = Class {
                name: name.lexeme.clone(),
                methods: methods_def,
            };
            state.env.assign(name, &Literal::Class(class))?;

            Ok(None)
        }
    }
}

fn evaluate(expr: &Expr, state: &mut State) -> EvalResult {
    match expr {
        Expr::Atomic(literal) => evaluate_literal(literal),
        Expr::Unary(op, expr) => evaluate_unary(op, expr, state),
        Expr::Binary(expr1, op, expr2) => evaluate_binary(op, expr1, expr2, state),
        Expr::Ternary(expr1, expr2, expr3) => evaluate_ternary(expr1, expr2, expr3, state),
        Expr::Get(expr, name) => evaluate_get(expr, name, state),
        Expr::Set(object, name, value) => evaluate_set(object, name, value, state),
        Expr::Grouping(expr) => evaluate(expr, state),
        Expr::Variable(token) => lookup_variable(token, state),
        Expr::Assign(token, expr) => evaluate_assign(token, expr, state),
        Expr::Logical(expr1, op, expr2) => evaluate_logical(op, expr1, expr2, state),
        Expr::Call(callee, arguments, paren) => evaluate_call(callee, arguments, paren, state),
        Expr::This(keyword) => evaluate_this(keyword, state),
    }
}

fn evaluate_literal(literal: &Literal) -> EvalResult {
    Ok(literal.to_owned())
}

fn evaluate_unary(op: &UnaryOp, expr: &Expr, state: &mut State) -> EvalResult {
    let right: Literal = evaluate(expr, state)?;

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

fn evaluate_binary(op: &BinaryOp, expr1: &Expr, expr2: &Expr, state: &mut State) -> EvalResult {
    let left: Literal = evaluate(expr1, state)?;
    let right: Literal = evaluate(expr2, state)?;

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

fn evaluate_ternary(expr1: &Expr, expr2: &Expr, expr3: &Expr, state: &mut State) -> EvalResult {
    let guard: Literal = evaluate(expr1, state)?;

    if is_truthy(&guard) {
        evaluate(expr2, state)
    } else {
        evaluate(expr3, state)
    }
}

fn evaluate_get(expr: &Expr, name: &Token, state: &mut State) -> EvalResult {
    let result = evaluate(expr, state)?;

    if let Literal::Instance(instance) = result {
        instance.get(name)
    } else {
        Err(Error::Eval {
            token: name.clone(),
            message: "Only instances have properties.".to_string(),
        })
    }
}

fn evaluate_set(object: &Expr, name: &Token, value: &Expr, state: &mut State) -> EvalResult {
    let mut eval_object = evaluate(object, state)?;

    if let Literal::Instance(instance) = &mut eval_object {
        let eval_value = evaluate(value, state)?;
        instance.set(name, eval_value.clone());
        Ok(eval_value)
    } else {
        Err(Error::Eval {
            token: name.clone(),
            message: "Only instances have fields.".to_string(),
        })
    }
}

fn evaluate_assign(token: &Token, expr: &Expr, state: &mut State) -> EvalResult {
    let value = evaluate(expr, state)?;

    if let Some(distance) = state.locals.get(token) {
        state.env.assign_at(*distance, token, &value)?;
    } else {
        state.env.get_globals().assign(token, &value)?;
    }

    Ok(value)
}

fn evaluate_logical(op: &LogicalOp, expr1: &Expr, expr2: &Expr, state: &mut State) -> EvalResult {
    let left = evaluate(expr1, state)?;

    match op {
        LogicalOp::Or => {
            if is_truthy(&left) {
                Ok(left)
            } else {
                evaluate(expr2, state)
            }
        }
        LogicalOp::And => {
            if !is_truthy(&left) {
                Ok(left)
            } else {
                evaluate(expr2, state)
            }
        }
    }
}

fn evaluate_call(
    callee_expr: &Expr,
    arguments_expr: &Vec<Expr>,
    end_token: &Token,
    state: &mut State,
) -> EvalResult {
    let callee = evaluate(callee_expr, state)?;

    let mut arguments: Vec<Literal> = vec![];

    for arg_expr in arguments_expr {
        let arg = evaluate(arg_expr, state)?;
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
                (callable.call)(&mut state.env, arguments)
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

            let parent = callable.closure;
            let mut env = Environment::new_child(&parent);

            for (param, arg) in callable.params.iter().zip(arguments) {
                env.define(param.lexeme.clone(), &arg);
            }

            let mut new_state = State {
                env,
                locals: state.locals.clone(),
            };

            let result = execute_block(&callable.body, &mut new_state)?;

            Ok(result.unwrap_or(Literal::Nil))
        }
        Literal::Class(class) => {
            let instance = Instance::new(class);
            Ok(Literal::Instance(instance))
        }
        _ => Err(Error::Eval {
            token: end_token.clone(),
            message: "Can only call functions and classes.".to_string(),
        }),
    }
}

fn evaluate_this(keyword: &Token, state: &mut State) -> EvalResult {
    lookup_variable(keyword, state)
}

fn execute_if(
    condition: &Expr,
    then_branch: &Stmt,
    else_branch: &Option<Box<Stmt>>,
    state: &mut State,
) -> ExecResult {
    let guard = evaluate(condition, state)?;

    if is_truthy(&guard) {
        execute(then_branch, state)
    } else {
        match else_branch {
            Some(else_stmt) => execute(else_stmt, state),
            None => Ok(None),
        }
    }
}

fn execute_block(statements: &Vec<Stmt>, state: &mut State) -> ExecResult {
    for stmt in statements {
        let result = execute(stmt, state)?;

        if let Some(return_lit) = result {
            return Ok(Some(return_lit));
        }
    }

    Ok(None)
}

fn execute_while(condition: &Expr, body: &Stmt, state: &mut State) -> ExecResult {
    while is_truthy(&evaluate(condition, state)?) {
        let result = execute(body, state)?;

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
    state: &mut State,
) -> ExecResult {
    let func = Function {
        params,
        name: name.to_owned(),
        body,
        closure: state.env.to_owned(),
    };

    state
        .env
        .define(name.lexeme.clone(), &Literal::Function(func));
    Ok(None)
}

fn execute_return(expr: &Expr, state: &mut State) -> ExecResult {
    evaluate(expr, state).map(Option::from)
}

fn lookup_variable(name: &Token, state: &mut State) -> EvalResult {
    let distance = state.locals.get(name);

    match distance {
        Some(d) => state.env.get_at(*d, name),
        None => state.env.get_globals().get(name),
    }
}

fn is_truthy(expr: &Literal) -> bool {
    !matches!(expr, Literal::Nil | Literal::False)
}

fn retrieve_nums(expr1: Literal, expr2: Literal) -> Option<(OrderedFloat<f64>, OrderedFloat<f64>)> {
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
