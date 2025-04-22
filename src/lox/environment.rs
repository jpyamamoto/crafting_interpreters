use super::{error::Error, expr::Literal, token::Token};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

struct Env {
    values: HashMap<String, Literal>,
    enclosing: Option<Rc<RefCell<Env>>>,
}

#[derive(Clone, Debug)]
pub struct Environment(Rc<RefCell<Env>>);

impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<env>")
    }
}

impl Environment {
    pub fn new() -> Environment {
        let env = Env {
            values: HashMap::new(),
            enclosing: None,
        };

        Environment(Rc::new(RefCell::new(env)))
    }

    pub fn get_globals(&self) -> Environment {
        let content: &Rc<_> = &self.0;

        if let Some(parent) = &content.borrow().enclosing {
            let parent_ref = Rc::clone(parent);
            Environment(parent_ref).get_globals()
        } else {
            Environment(content.to_owned())
        }
    }

    pub fn new_child(Environment(parent): &Environment) -> Environment {
        let parent_ref: Rc<RefCell<Env>> = Rc::clone(parent);

        let result_env = Env {
            values: HashMap::new(),
            enclosing: Some(parent_ref),
        };

        Environment(Rc::new(RefCell::new(result_env)))
    }

    pub fn define(&mut self, name: String, value: &Literal) {
        (*self.0).borrow_mut().values.insert(name, value.clone());
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
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<Literal, Error> {
        self.ancestor(distance)
            .borrow()
            .values
            .get(&name.lexeme)
            .map(|l| l.to_owned())
            .ok_or(Error::Eval {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Env>> {
        let mut current = self.0.clone();

        for _ in 0..distance {
            let maybe_enclosing = current.borrow().enclosing.as_ref().map(Rc::clone);

            if let Some(env) = maybe_enclosing {
                current = env;
            } else {
                break;
            }
        }

        current.clone()
    }

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: &Token,
        value: &Literal,
    ) -> Result<(), Error> {
        self.ancestor(distance)
            .borrow_mut()
            .values
            .insert(name.lexeme.clone(), value.clone());
        Ok(())
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
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            })
        }
    }
}
