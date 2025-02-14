use core::fmt;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use crate::ast::{BlockStatement, Expression, Identifier, Node};

use super::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    Builtin(String, usize, BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Quote(Expression),
    Null,
}

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

impl Object {
    pub fn is_integer(&self) -> bool {
        matches!(*self, Object::Integer(_))
    }
    pub fn is_boolean(&self) -> bool {
        matches!(*self, Object::Boolean(_))
    }
    pub fn is_string(&self) -> bool {
        matches!(*self, Object::Return(_))
    }
    pub fn is_returned(&self) -> bool {
        matches!(*self, Object::Return(_))
    }
    pub fn is_error(&self) -> bool {
        matches!(*self, Object::Error(_))
    }
    pub fn is_function(&self) -> bool {
        matches!(*self, Object::Function(_, _, _))
    }
    pub fn is_null(&self) -> bool {
        matches!(*self, Object::Null)
    }

    pub fn get_type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::Return(_) => "Return",
            Object::Error(_) => "Error",
            Object::Null => "Null",
            Object::Function(_, _, _) => "Function",
            Object::String(_) => "String",
            Object::Builtin(_, _, _) => "Builtin",
            Object::Array(_) => "Array",
            Object::Hash(_) => "Hash",
            Object::Quote(_) => "Quote",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Integer(ref v) => write!(f, "{}", v),
            Object::Boolean(ref v) => {
                if *v {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Object::Return(ref v) => write!(f, "{}", *v),
            Object::Null => write!(f, "null"),
            Object::Error(ref v) => write!(f, "ERROR: {}", v),
            Object::Function(_, _, _) => write!(f, "[function]"),
            Object::String(ref v) => write!(f, "{}", v),
            Object::Builtin(ref name, _, _) => write!(f, "[buitlin function: {}]", name),
            Object::Array(ref v) => {
                let mut out = String::new();
                out.push('[');
                for (i, o) in v.iter().enumerate() {
                    out.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        out.push_str(", ");
                    }
                }
                out.push(']');
                write!(f, "{}", out)
            }
            Object::Hash(ref v) => {
                let mut out = String::new();

                out.push('{');
                for (i, (key, val)) in v.iter().enumerate() {
                    out.push_str(format!("{}: {}", key, val).as_str());
                    if i < v.len() - 1 {
                        out.push_str(", ");
                    }
                }
                out.push('}');

                write!(f, "{}", out)
            }
            Object::Quote(ref object) => {
                write!(f, "QUOTE({})", object.as_string())
            }
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match *self {
            Object::Integer(ref i) => i.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
