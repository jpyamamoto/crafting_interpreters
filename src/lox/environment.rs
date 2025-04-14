use super::{error::Error, expr::Literal, token::Token};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone)]
struct Env {
    values: HashMap<String, Literal>,
    enclosing: Option<Rc<RefCell<Env>>>,
}

pub struct Environment(Rc<RefCell<Env>>);

impl Environment {
    pub fn new() -> Environment {
        let env = Env {
            values: HashMap::new(),
            enclosing: None,
        };

        Environment(Rc::new(RefCell::new(env)))
    }

    pub fn new_child(Environment(parent): &Environment) -> Environment {
        let parent_ref: Rc<RefCell<Env>> = Rc::clone(parent);

        let result_env = Env {
            values: HashMap::new(),
            enclosing: Some(parent_ref),
        };

        Environment(Rc::new(RefCell::new(result_env)))
    }

    pub fn define(&mut self, name: &Token, value: &Literal) {
        (*self.0)
            .borrow_mut()
            .values
            .insert(name.lexeme.clone(), value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Literal, Error> {
        self.0
            .borrow()
            .values
            .get(&name.lexeme)
            .map(|v| v.to_owned())
            .or_else(|| match &self.0.borrow().enclosing {
                None => None,
                // Some(parent) => parent.borrow().get(name).ok(),
                Some(parent) => Environment(parent.to_owned()).get(name).ok(),
            })
            .ok_or(Error::Eval {
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    pub fn assign(&mut self, name: &Token, value: &Literal) -> Result<(), Error> {
        if self.0.borrow().values.contains_key(&name.lexeme) {
            (*self.0)
                .borrow_mut()
                .values
                .insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }

        if let Some(parent) = &self.0.borrow().enclosing {
            Environment(parent.to_owned()).assign(name, value)
        } else {
            Err(Error::Eval {
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
        }
    }
}
