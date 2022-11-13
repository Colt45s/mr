use std::fmt::Display;

#[derive(Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::Return(v) => write!(f, "{}", v),
            Object::Error(v) => write!(f, "{}", v),
        }
    }
}

impl Object {
    pub fn to_type(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            Object::Null => "NULL".to_string(),
            Object::Return(_) => "RETURN".to_string(),
            Object::Error(_) => "ERROR".to_string(),
        }
    }
}
