use super::object::{BuiltinFunction, Object};
pub struct BuiltinFunctions;

impl Default for BuiltinFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinFunctions {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get_builtins(&self) -> Vec<(String, Object)> {
        vec![add_builtin("len", 1, fn_blen)]
    }
}

fn add_builtin(name: &str, param_num: usize, func: BuiltinFunction) -> (String, Object) {
    let name = name.to_owned();
    (name.clone(), Object::Builtin(name, param_num, func))
}

fn fn_blen(params: Vec<Object>) -> Object {
    match params.first() {
        Some(Object::String(s)) => Object::Integer(s.len() as i64),
        Some(o) => Object::Error(format!(
            "argument to `len` not supported, got {}",
            o.get_type_name()
        )),
        None => Object::Error("argument length wrong".to_string()),
    }
}
