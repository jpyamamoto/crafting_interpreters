use super::environment::Environment;
use super::error::Error;
use super::stmt::Stmt;
use super::token::{InternalValue, Token, TokenType};

use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Lit(pub Rc<RefCell<Literal>>);

#[derive(Debug, Clone)]
pub struct Function(Rc<RefCell<FunctionDesc>>);

#[derive(Debug, Clone)]
pub struct Class(Rc<RefCell<ClassDesc>>);

#[derive(Debug, Clone)]
pub struct Instance(Rc<RefCell<InstanceDesc>>);

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(OrderedFloat<f64>),
    True,
    False,
    Nil,
    NativeFunction(NativeFunction),
    Function(Function),
    Class(Class),
    Instance(Instance),
}

#[derive(Debug)]
pub struct NativeFunction {
    name: String,
    pub arity: usize,
    pub call: fn(&mut Environment, Vec<Lit>) -> Result<Literal, Error>,
}

#[derive(Debug)]
pub struct FunctionDesc {
    name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub closure: Environment,
}

#[derive(Debug)]
pub struct ClassDesc {
    name: String,
    pub methods: RefCell<HashMap<String, Function>>,
}

#[derive(Debug)]
pub struct InstanceDesc {
    pub class: Class,
    pub fields: HashMap<String, Lit>,
}

impl Lit {
    pub fn with<R>(&self, f: impl FnOnce(&Literal) -> R) -> R {
        f(&self.0.borrow())
    }

    pub fn with_mut<R>(&self, f: impl FnOnce(&mut Literal) -> R) -> R {
        f(&mut self.0.borrow_mut())
    }
}

impl Function {
    pub fn new(params: Vec<Token>, name: Token, body: Vec<Stmt>, closure: Environment) -> Self {
        FunctionDesc {
            name,
            params,
            body,
            closure,
        }
        .into()
    }

    pub fn with<R>(&self, f: impl FnOnce(&FunctionDesc) -> R) -> R {
        f(&self.0.borrow())
    }
}

impl Class {
    pub fn new(name: String, methods: RefCell<HashMap<String, Function>>) -> Self {
        ClassDesc { name, methods }.into()
    }

    pub fn find_method(&self, name: String) -> Option<Function> {
        self.0
            .borrow()
            .methods
            .borrow()
            .get(&name)
            .map(Function::to_owned)
    }
    pub fn with<R>(&self, f: impl FnOnce(&ClassDesc) -> R) -> R {
        f(&self.0.borrow())
    }
}

impl Instance {
    pub fn new(class: Class) -> Instance {
        InstanceDesc {
            class,
            fields: HashMap::new(),
        }
        .into()
    }

    pub fn with<R>(&self, f: impl FnOnce(&InstanceDesc) -> R) -> R {
        f(&self.0.borrow())
    }

    pub fn with_mut<R>(&self, f: impl FnOnce(&mut InstanceDesc) -> R) -> R {
        f(&mut self.0.borrow_mut())
    }

    pub fn get(&self, name: &Token) -> Result<Lit, Error> {
        if let Some(field) = self.0.borrow().fields.get(&name.lexeme) {
            return Ok(field.clone());
        }

        let method = self.with(|i| i.class.find_method(name.lexeme.clone()));

        if let Some(method) = method {
            let func = method.with(|f| f.bind(self.clone()));
            Ok(Literal::Function(func.into()).into())
        } else {
            Err(Error::Eval {
                token: name.clone(),
                message: format!("Undefined property '{}'.", name.lexeme),
            })
        }
    }

    pub fn set(&self, name: &Token, value: Lit) {
        self.with_mut(|instance| {
            instance.fields.insert(name.lexeme.clone(), value);
        })
    }
}

impl NativeFunction {
    pub fn new(
        name: String,
        arity: usize,
        call: fn(&mut Environment, Vec<Lit>) -> Result<Literal, Error>,
    ) -> Self {
        NativeFunction { name, arity, call }
    }
}

impl FunctionDesc {
    pub fn bind(&self, instance: Instance) -> FunctionDesc {
        let environment = Environment::with_parent(self.closure.clone());
        environment.define("this".to_string(), Literal::Instance(instance).into());

        FunctionDesc {
            params: self.params.clone(),
            name: self.name.clone(),
            body: self.body.clone(),
            closure: environment,
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(|lit| write!(f, "{}", lit))
    }
}

impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        self.with(|self_lit| other.with(|other_lit| self_lit == other_lit))
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Identifier(identifier) => write!(f, "{}", identifier),
            Literal::String(string) => write!(f, "{}", string),
            Literal::Number(number) => write!(f, "{}", number),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
            Literal::NativeFunction(native_func) => write!(f, "{}", native_func.name),
            Literal::Function(func) => write!(f, "<fn {}>", func.with(|f| f.name.lexeme.clone())),
            Literal::Class(class) => write!(f, "<class {}>", class.with(|c| c.name.clone())),
            Literal::Instance(instance) => {
                write!(
                    f,
                    "<instance {}>",
                    instance.0.borrow().class.with(|c| c.name.clone())
                )
            }
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l), Self::Identifier(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::True, Self::True) => true,
            (Self::False, Self::False) => true,
            (Self::Nil, Self::Nil) => true,
            (Self::NativeFunction(l), Self::NativeFunction(r)) => l.name == r.name,
            _ => false,
        }
    }
}

impl TryFrom<&Token> for Literal {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match &token.type_token {
            TokenType::Identifier => {
                if let Some(InternalValue::Text(value)) = &token.value {
                    Ok(Literal::Identifier(value.clone()))
                } else {
                    Err(Error::Parse {
                        token: token.clone(),
                        message: "Can't retrieve variable name".to_string(),
                    })
                }
            }
            TokenType::String => {
                if let Some(InternalValue::Text(value)) = &token.value {
                    Ok(Literal::String(value.clone()))
                } else {
                    Err(Error::Parse {
                        token: token.clone(),
                        message: "Can't retrieve string content".to_string(),
                    })
                }
            }
            TokenType::Number => {
                if let Some(InternalValue::Number(value)) = token.value {
                    Ok(Literal::Number(value))
                } else {
                    Err(Error::Parse {
                        token: token.clone(),
                        message: "Can't retrieve number value".to_string(),
                    })
                }
            }
            TokenType::False => Ok(Literal::False),
            TokenType::True => Ok(Literal::True),
            TokenType::Nil => Ok(Literal::Nil),
            _ => Err(Error::Parse {
                token: token.clone(),
                message: "Not a literal".to_string(),
            }),
        }
    }
}

impl From<Literal> for Lit {
    fn from(value: Literal) -> Self {
        Lit(Rc::new(RefCell::new(value)))
    }
}

impl From<FunctionDesc> for Function {
    fn from(value: FunctionDesc) -> Self {
        Function(Rc::new(RefCell::new(value)))
    }
}

impl From<ClassDesc> for Class {
    fn from(value: ClassDesc) -> Self {
        Class(Rc::new(RefCell::new(value)))
    }
}

impl From<InstanceDesc> for Instance {
    fn from(value: InstanceDesc) -> Self {
        Instance(Rc::new(RefCell::new(value)))
    }
}
