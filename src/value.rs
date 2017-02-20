use std::ops::*;
use std::fmt;

use ast;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

impl Value {
    fn get_type_string(&self) -> String {
        match *self {
            Value::Integer(_) => "Integer".to_string(),
            Value::Float(_) => "Float".to_string(),
            Value::Bool(_) => "Bool".to_string(),
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

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
            (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 + y),
            (Value::Float(x), Value::Integer(y)) => Value::Float(x + y as f64),
            (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
            (a, b) => panic!(
                "error: operation + is not defined for types {} and {}",
                a.get_type_string(),
                b.get_type_string()
            ),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
            (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 - y),
            (Value::Float(x), Value::Integer(y)) => Value::Float(x - y as f64),
            (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
            (a, b) => panic!(
                "error: operation - is not defined for types {} and {}",
                a.get_type_string(),
                b.get_type_string()
            ),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
            (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 * y),
            (Value::Float(x), Value::Integer(y)) => Value::Float(x * y as f64),
            (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
            (a, b) => panic!(
                "error: operation * is not defined for types {} and {}",
                a.get_type_string(),
                b.get_type_string()
            ),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(x), Value::Integer(y)) => {
                if x % y == 0 {
                    Value::Integer(x / y)
                } else {
                    Value::Float(x as f64 / y as f64)
                }
            },
            (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 / y),
            (Value::Float(x), Value::Integer(y)) => Value::Float(x / y as f64),
            (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
            (a, b) => panic!(
                "error: operation / is not defined for types {} and {}",
                a.get_type_string(),
                b.get_type_string()
            ),
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
