use core::fmt;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use crate::parse::ast::{Expression, Node, Statement, ToNode};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Rc<RefCell<Environment>>,
    },
    Builtin(String),
    Quote(Expression),
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&format!("{:?}", self).into_bytes());
    }
}

impl ToNode for Object {
    fn to_node(self: Self) -> Node {
        match self {
            Object::Integer(i) => Node::Expression(Expression::Integer { value: i }),
            o => todo!("Not implemented for object: {o:?}"),
        }
    }
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use pretty_assertions::assert_eq;

    use super::Object;

    #[test]
    fn object_hash() {
        let key1 = Object::String(String::from("Hello World!"));
        let key2 = Object::String(String::from("Hello World!"));
        let value = Object::Integer(1);
        let mut hash_map: HashMap<Object, Object> = HashMap::new();

        hash_map.insert(key1.clone(), value.clone());
        hash_map.insert(key2.clone(), value.clone());

        assert_eq!(1, hash_map.len());
        assert_eq!(Some(&value), hash_map.get(&key1));
        assert_eq!(Some(&value), hash_map.get(&key2));
    }
}
