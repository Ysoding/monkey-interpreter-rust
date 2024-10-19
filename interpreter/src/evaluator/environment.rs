use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(v) => Some(v.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
