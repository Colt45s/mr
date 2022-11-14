use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

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

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: &str, val: &Object) {
        self.store.insert(name.to_string(), val.clone());
    }
}
