use std::fmt;

use ast;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

impl Value {
    pub fn get_type_string(&self) -> String {
        match *self {
            Value::Integer(_) => "Integer".to_string(),
            Value::Float(_) => "Float".to_string(),
            Value::Bool(_) => "Bool".to_string(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Integer(i) => i != 0,
            Value::Float(f) => f != 0.0,
            Value::Bool(b) => b,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Float(x) => write!(f, "Value::Float({})", x),
            Value::Integer(x) => write!(f, "Value::Integer({})", x),
            Value::Bool(x) => write!(f, "Value::Bool({})", x),
        }
    }
}

impl From<ast::Literal> for Value {
    fn from(from: ast::Literal) -> Self {
        match from {
            ast::Literal::Integer(x) => Value::Integer(x),
            ast::Literal::Float(x) => Value::Float(x),
            ast::Literal::Bool(x) => Value::Bool(x),
        }
    }
}
