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
        vec![
            add_builtin("len", 1, blen),
            add_builtin("first", 1, bfirst),
            add_builtin("last", 1, blast),
            add_builtin("rest", 1, brest),
            add_builtin("push", 2, bpush),
            add_builtin("puts", 1, bputs),
        ]
    }
}

fn add_builtin(name: &str, param_num: usize, func: BuiltinFunction) -> (String, Object) {
    let name = name.to_owned();
    (name.clone(), Object::Builtin(name, param_num, func))
}

fn bputs(params: Vec<Object>) -> Object {
    match params.first() {
        Some(Object::String(s)) => {
            println!("{}", s);
            Object::Null
        }
        Some(o) => {
            println!("{}", o);
            Object::Null
        }
        _ => Object::Error("argument length wrong".to_string()),
    }
}

fn blen(params: Vec<Object>) -> Object {
    match params.first() {
        Some(Object::String(s)) => Object::Integer(s.len() as i64),
        Some(Object::Array(v)) => Object::Integer(v.len() as i64),
        Some(o) => Object::Error(format!(
            "argument to `len` not supported, got {}",
            o.get_type_name()
        )),
        None => Object::Error("argument length wrong".to_string()),
    }
}

fn bfirst(params: Vec<Object>) -> Object {
    match params.into_iter().next() {
        Some(Object::Array(v)) => match v.into_iter().next() {
            Some(x) => x,
            None => Object::Null,
        },
        Some(o) => Object::Error(format!(
            "argument to `first` must be Array, got {}",
            o.get_type_name()
        )),
        _ => Object::Error("argument length wrong".to_string()),
    }
}

fn blast(params: Vec<Object>) -> Object {
    match params.into_iter().next() {
        Some(Object::Array(mut v)) => match v.pop() {
            Some(a) => a,
            None => Object::Null,
        },
        Some(o) => Object::Error(format!(
            "argument to `last` must be Array, got {}",
            o.get_type_name()
        )),
        _ => Object::Error("argument length wrong".to_string()),
    }
}

fn brest(params: Vec<Object>) -> Object {
    match params.into_iter().next() {
        Some(Object::Array(mut v)) => match v.len() {
            0 => Object::Null,
            _ => {
                v.remove(0);
                Object::Array(v)
            }
        },
        Some(o) => Object::Error(format!(
            "argument to `rest` must be Array, got {}",
            o.get_type_name()
        )),
        _ => Object::Error("argument length wrong".to_string()),
    }
}

fn bpush(params: Vec<Object>) -> Object {
    let mut params = params.into_iter();
    match (params.next(), params.next()) {
        (Some(Object::Array(mut v)), Some(o)) => {
            v.push(o);
            Object::Array(v)
        }
        (Some(v), Some(_)) => Object::Error(format!(
            "first argument to `push` must be Array, got {}",
            v.get_type_name()
        )),
        _ => Object::Error("argument length wrong".to_string()),
    }
}
