use std::fmt;
use std::ops;
use std::cmp;

use ast;
use checker::Type;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Number(Number),
    Bool(bool),
}

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Number::Integer(x) => write!(f, "Integer({})", x),
            Number::Float(x) => write!(f, "Float({})", x),
        }
    }
}

impl ops::Add for Number {
    type Output = Number;

    fn add(self, other: Number) -> Number {
        match (self, other) {
            (Number::Integer(x), Number::Integer(y)) => Number::Integer(x + y),
            (Number::Float(x), Number::Float(y)) => Number::Float(x + y),
            (Number::Float(x), Number::Integer(y)) => Number::Float(x + (y as f64)),
            (Number::Integer(x), Number::Float(y)) => Number::Float((x as f64) + y),
        }
    }
}

impl ops::Sub for Number {
    type Output = Number;

    fn sub(self, other: Number) -> Number {
        match (self, other) {
            (Number::Integer(x), Number::Integer(y)) => Number::Integer(x - y),
            (Number::Float(x), Number::Float(y)) => Number::Float(x - y),
            (Number::Float(x), Number::Integer(y)) => Number::Float(x - (y as f64)),
            (Number::Integer(x), Number::Float(y)) => Number::Float((x as f64) - y),
        }
    }
}

impl ops::Mul for Number {
    type Output = Number;

    fn mul(self, other: Number) -> Number {
        match (self, other) {
            (Number::Integer(x), Number::Integer(y)) => Number::Integer(x * y),
            (Number::Float(x), Number::Float(y)) => Number::Float(x * y),
            (Number::Float(x), Number::Integer(y)) => Number::Float(x * (y as f64)),
            (Number::Integer(x), Number::Float(y)) => Number::Float((x as f64) * y),
        }
    }
}

impl ops::Div for Number {
    type Output = Number;

    fn div(self, other: Number) -> Number {
        match (self, other) {
            (Number::Integer(x), Number::Integer(y)) => Number::Float((x as f64) / (y as f64)),
            (Number::Float(x), Number::Float(y)) => Number::Float(x / y),
            (Number::Float(x), Number::Integer(y)) => Number::Float(x / (y as f64)),
            (Number::Integer(x), Number::Float(y)) => Number::Float((x as f64) / y),
        }
    }
}

impl ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Number {
        match self {
            Number::Integer(x) => Number::Integer(-x),
            Number::Float(x) => Number::Float(-x),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        match (*self, *other) {
            (Number::Integer(x), Number::Integer(y)) => x == y,
            (Number::Float(x), Number::Float(y)) => x == y,
            (Number::Float(x), Number::Integer(y)) => (x == x.trunc()) && (x as i64) == y,
            (Number::Integer(x), Number::Float(y)) => (y == y.trunc()) && (y as i64) == x,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Number) -> Option<cmp::Ordering> {
        match (*self, *other) {
            (Number::Integer(x), Number::Integer(y)) => x.partial_cmp(&y),
            (Number::Float(x), Number::Float(y)) => x.partial_cmp(&y),
            (Number::Float(x), Number::Integer(y)) => x.partial_cmp(&(y as f64)),
            (Number::Integer(x), Number::Float(y)) => (x as f64).partial_cmp(&y),
        }
    }
}

impl Number {
    pub fn floor_div(&self, other: &Number) -> Number {
        match (*self, *other) {
            (Number::Integer(x), Number::Integer(y)) => Number::Integer(x / y),
            (Number::Float(x), Number::Float(y)) => Number::Float((x / y).floor()),
            (Number::Float(x), Number::Integer(y)) => Number::Float((x / (y as f64)).floor()),
            (Number::Integer(x), Number::Float(y)) => Number::Float(((x as f64) / y).floor()),
        }
    }
}

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Bool,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Number(n) => {
                match n {
                    Number::Integer(i) => i != 0,
                    Number::Float(f) => f != 0.0,
                }
            }
            Value::Bool(b) => b,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "Value::Number({})", n),
            Value::Bool(x) => write!(f, "Value::Bool({})", x),
        }
    }
}

impl From<ast::Literal> for Value {
    fn from(from: ast::Literal) -> Self {
        match from {
            ast::Literal::Integer(x) => Value::Number(Number::Integer(x)),
            ast::Literal::Float(x) => Value::Number(Number::Float(x)),
            ast::Literal::Bool(x) => Value::Bool(x),
        }
    }
}
