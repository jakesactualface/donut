use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parse::ast::{Expression, Statement};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Rc<RefCell<Environment>>,
    },
}

#[derive(PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosure(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        if let Some(object) = self.store.get(name) {
            return Some(Rc::new(object.clone()));
        }
        if let Some(outer) = &self.outer {
            return outer.borrow().get(name).clone();
        }
        return None;
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        return value;
    }
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Environment {{ store<keys>: {:?} }}", self.store.keys())
    }
}
