use std::ops::*;
use std::fmt;

use ast;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Float(x) => write!(f, "Value::Float({})", x),
            Value::Integer(x) => write!(f, "Value::Integer({})", x),
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
            // _ => panic!("error: operation + is undefined on given types")
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
            // _ => panic!("error: operation - is undefined on given types")
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
            // _ => panic!("error: operation * is undefined on given types")
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
            // _ => panic!("error: operation / is undefined on given types")
        }
    }
}

impl From<ast::Literal> for Value {
    fn from(from: ast::Literal) -> Self {
        match from {
            ast::Literal::Integer(x) => Value::Integer(x),
            ast::Literal::Float(x) => Value::Float(x),
        }
    }
}
