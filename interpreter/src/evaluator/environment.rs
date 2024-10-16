use std::collections::HashMap;

use super::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }

    pub fn get(&mut self, name: &str) -> Option<Object> {
        self.store.get(name).cloned()
    }
}
