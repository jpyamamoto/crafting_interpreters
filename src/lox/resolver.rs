use std::{borrow::BorrowMut, collections::HashMap};

use super::{
    error::Error,
    expr::Expr,
    stmt::{FuncContainer, Stmt},
    token::Token,
};

type Scope = HashMap<String, bool>;
pub type Locals = HashMap<Token, usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
}

struct State {
    scopes: Vec<Scope>,
    locals: Locals,
    current_function: FunctionType,
    current_class: ClassType,
}

pub fn resolve(statements: &Vec<Stmt>) -> Result<Locals, Error> {
    let mut state = State {
        scopes: vec![],
        locals: HashMap::new(),
        current_function: FunctionType::None,
        current_class: ClassType::None,
    };

    resolve_many(statements, &mut state).map(|_| state.locals)
}

fn resolve_many(statements: &Vec<Stmt>, state: &mut State) -> Result<(), Error> {
    for stmt in statements {
        resolve_stmt(stmt, state)?;
    }

    Ok(())
}

fn resolve_stmt(statement: &Stmt, state: &mut State) -> Result<(), Error> {
    match statement {
        Stmt::Expr(expr) => resolve_expr(expr, state),
        Stmt::Print(expr) => resolve_expr(expr, state),
        Stmt::Var(token, initializer) => {
            declare(token, state)?;

            if let Some(init_expr) = initializer {
                resolve_expr(init_expr, state)?;
            }

            define(token, state);
            Ok(())
        }
        Stmt::Block(statements) => {
            begin_scope(state);
            resolve_many(statements, state)?;
            end_scope(state);
            Ok(())
        }
        Stmt::If(condition, then_branch, else_branch) => {
            resolve_expr(condition, state)?;
            resolve_stmt(then_branch, state)?;

            if let Some(else_expr) = else_branch {
                resolve_stmt(else_expr, state)?;
            }

            Ok(())
        }
        Stmt::While(condition, body) => {
            resolve_expr(condition, state)?;
            resolve_stmt(body, state)
        }
        Stmt::Function(FuncContainer { name, params, body }) => {
            declare(name, state)?;
            define(name, state);

            resolve_function(params, body, state, FunctionType::Function)?;
            Ok(())
        }
        Stmt::Return(keyword, expr) => {
            if state.current_function == FunctionType::None {
                return Err(Error::Report {
                    token: keyword.clone(),
                    message: "Can't return from top-level code.".to_string(),
                });
            };

            if let Some(some_expr) = expr {
                if state.current_function == FunctionType::Initializer {
                    return Err(Error::Report {
                        token: keyword.clone(),
                        message: "Can't return a value from an initializer.".to_string(),
                    });
                }

                resolve_expr(some_expr, state)?;
            }

            Ok(())
        }
        Stmt::Class(name, methods) => {
            let enclosing_class = state.current_class;
            state.current_class = ClassType::Class;

            declare(name, state)?;
            define(name, state);

            begin_scope(state);

            if let Some(scope) = state.scopes.last_mut() {
                scope.insert("this".to_string(), true);
            }

            for method in methods {
                let declaration = if method.name.lexeme == "init" {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                };
                resolve_function(&method.params, &method.body, state, declaration)?;
            }

            end_scope(state);

            state.current_class = enclosing_class;

            Ok(())
        }
    }
}

fn resolve_expr(expr: &Expr, state: &mut State) -> Result<(), Error> {
    match expr {
        Expr::Atomic(_) => Ok(()),
        Expr::Unary(_, expr) => resolve_expr(expr, state),
        Expr::Binary(expr1, _, expr2) => {
            resolve_expr(expr1, state)?;
            resolve_expr(expr2, state)
        }
        Expr::Ternary(expr1, expr2, expr3) => {
            resolve_expr(expr1, state)?;
            resolve_expr(expr2, state)?;
            resolve_expr(expr3, state)
        }
        Expr::Grouping(expr) => resolve_expr(expr, state),
        Expr::Get(expr, _) => resolve_expr(expr, state),
        Expr::Set(object, _, value) => {
            resolve_expr(value, state)?;
            resolve_expr(object, state)?;
            Ok(())
        }
        Expr::Variable(token) => {
            if let Some(scope) = state.scopes.last() {
                if let Some(false) = scope.get(&token.lexeme) {
                    let err = Error::Report {
                        token: token.clone(),
                        message: "Can't read local variable in its own initializer.".to_string(),
                    };

                    return Err(err);
                }
            }

            resolve_local(token, state)?;
            Ok(())
        }
        Expr::Assign(token, expr) => {
            resolve_expr(expr, state)?;
            resolve_local(token, state)?;
            Ok(())
        }
        Expr::Logical(expr1, _, expr2) => {
            resolve_expr(expr1, state)?;
            resolve_expr(expr2, state)
        }
        Expr::Call(callee, arguments, _) => {
            resolve_expr(callee, state)?;

            for arg in arguments {
                resolve_expr(arg, state)?;
            }

            Ok(())
        }
        Expr::This(token) => {
            if state.current_class == ClassType::None {
                Err(Error::Report {
                    token: token.clone(),
                    message: "Can't use 'this' outside of a class.".to_string(),
                })
            } else {
                resolve_local(token, state)
            }
        }
    }
}

fn begin_scope(state: &mut State) {
    state.scopes.push(HashMap::new());
}

fn end_scope(state: &mut State) {
    state.scopes.pop();
}

fn declare(name: &Token, state: &mut State) -> Result<(), Error> {
    if let Some(scope) = state.scopes.last_mut() {
        if scope.contains_key(&name.lexeme) {
            return Err(Error::Report {
                token: name.clone(),
                message: "Already a variable with this name in this scope.".to_string(),
            });
        }

        scope.insert(name.lexeme.clone(), false);
    }

    Ok(())
}

fn define(name: &Token, state: &mut State) {
    if let Some(scope) = state.scopes.last_mut() {
        scope.insert(name.lexeme.clone(), true);
    }
}

fn resolve_local(name: &Token, state: &mut State) -> Result<(), Error> {
    let scopes_size = state.scopes.len();

    for i in (0..scopes_size).rev() {
        if state.scopes[i].contains_key(&name.lexeme) {
            state
                .locals
                .borrow_mut()
                .insert(name.clone(), scopes_size - 1 - i);
        }
    }

    Ok(())
}

fn resolve_function(
    params: &Vec<Token>,
    body: &Vec<Stmt>,
    state: &mut State,
    function_type: FunctionType,
) -> Result<(), Error> {
    let enclosing_function = state.current_function;
    state.current_function = function_type;

    begin_scope(state);

    for param in params {
        declare(param, state)?;
        define(param, state);
    }

    resolve_many(body, state)?;

    end_scope(state);
    state.current_function = enclosing_function;

    Ok(())
}
