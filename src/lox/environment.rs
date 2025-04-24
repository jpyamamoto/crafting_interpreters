use super::{error::Error, literal::Lit, token::Token};
use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Env {
    values: RefCell<HashMap<String, Lit>>,
    enclosing: Option<Environment>,
}

#[derive(Debug, Clone)]
pub struct Environment(Rc<Env>);

impl Environment {
    pub fn new() -> Self {
        Env {
            values: RefCell::new(HashMap::new()),
            enclosing: None,
        }
        .into()
    }

    pub fn with_parent(parent: Environment) -> Self {
        Env {
            values: RefCell::new(HashMap::new()),
            enclosing: Some(parent),
        }
        .into()
    }

    pub fn get_globals(self: &Environment) -> Self {
        let mut current = self.clone();

        while let Some(parent) = &current.0.enclosing {
            current = parent.clone();
        }

        current
    }

    pub fn define(&self, name: String, value: Lit) {
        self.0.values.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<Lit, Error> {
        self.0
            .values
            .borrow()
            .get(&name.lexeme)
            .map(|v| v.to_owned())
            .or_else(|| match self.0.enclosing.borrow() {
                None => None,
                Some(parent) => parent.get(name).ok(),
            })
            .ok_or(Error::Eval {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    pub fn assign(&self, name: &Token, value: Lit) -> Result<(), Error> {
        if self.0.values.borrow().contains_key(&name.lexeme) {
            self.0
                .values
                .borrow_mut()
                .insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }

        if let Some(parent) = self.0.enclosing.borrow() {
            parent.assign(name, value)
        } else {
            Err(Error::Eval {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
        }
    }

    pub fn ancestor(&self, distance: usize) -> Rc<Env> {
        let mut current = self.clone();
        let mut n = distance;

        while n > 0 {
            if let Some(parent) = &current.0.enclosing {
                current = parent.clone();
                n -= 1;
            } else {
                return current.0;
            }
        }

        current.0
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<Lit, Error> {
        self.ancestor(distance)
            .values
            .borrow()
            .get(&name.lexeme)
            .map(|l| l.to_owned())
            .ok_or(Error::Eval {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    pub fn assign_at(&mut self, distance: usize, name: &Token, value: Lit) -> Result<(), Error> {
        self.ancestor(distance)
            .values
            .borrow_mut()
            .insert(name.lexeme.clone(), value.clone());
        Ok(())
    }
}

impl From<Env> for Environment {
    fn from(value: Env) -> Self {
        Environment(Rc::new(value))
    }
}
