use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{builtins::BuiltinFunctions, object::Object};

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut store = HashMap::new();
        Self::add_builtins(&mut store);
        Self { store, outer: None }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        let mut store = HashMap::new();
        Self::add_builtins(&mut store);
        Self {
            store,
            outer: Some(outer),
        }
    }

    fn add_builtins(store: &mut HashMap<String, Object>) {
        let builtin_functions = BuiltinFunctions::new();
        let bfns = builtin_functions.get_builtins();
        for (name, obj) in bfns {
            store.insert(name, obj);
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
