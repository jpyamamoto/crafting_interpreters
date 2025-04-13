use super::{error::Error, expr::Literal, token::Token};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Literal>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_child(self) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(Box::new(self)),
        }
    }

    pub fn define(&mut self, name: &Token, value: &Literal) {
        self.values.insert(name.lexeme.clone(), value.clone());
    }

    pub fn get(&self, name: &Token) -> Result<Literal, Error> {
        self.values
            .get(&name.lexeme)
            .map(|v| v.to_owned())
            .or_else(|| match &self.enclosing {
                None => None,
                Some(parent) => parent.get(name).ok(),
            })
            .ok_or(Error::Eval {
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    pub fn assign(&mut self, name: &Token, value: &Literal) -> Result<(), Error> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            return Ok(());
        }

        if let Some(parent) = &mut self.enclosing {
            parent.assign(name, value)
        } else {
            Err(Error::Eval {
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
        }
    }
}
