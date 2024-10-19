use core::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::ast::{BlockStatement, Identifier};

use super::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
    Null,
}

impl Object {
    pub fn is_returned(&self) -> bool {
        matches!(*self, Object::Return(_))
    }
    pub fn is_error(&self) -> bool {
        matches!(*self, Object::Error(_))
    }

    pub fn get_type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::Return(_) => "Return",
            Object::Error(_) => "Error",
            Object::Null => "Null",
            Object::Function(_, _, _) => "Function",
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
        }
    }
}
