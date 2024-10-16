use core::fmt;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Null,
}

impl Object {
    pub fn is_returned(&self) -> bool {
        matches!(*self, Object::Return(_))
    }
    pub fn is_error(&self) -> bool {
        matches!(*self, Object::Error(_))
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
        }
    }
}
