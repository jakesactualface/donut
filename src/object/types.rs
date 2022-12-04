use std::collections::HashMap;

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
        env: Box<Environment>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}
// TODO: Take a look at this: http://way-cooler.org/blog/2016/08/14/designing-a-bi-mutable-directional-tree-safely-in-rust.html

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosure(outer: Environment) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        if let Some(object) = self.store.get(name) {
            return Some(object);
        }
        if let Some(outer) = &self.outer {
            return outer.get(name);
        }
        return None;
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        return value;
    }
}
