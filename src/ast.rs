use std::ops::*;
use std::fmt;

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

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug, Clone)]
pub enum LhsExpr {
    Identifier(String),
}

#[derive(Debug, Clone)]
pub enum Variable {
    Identifier(BindingType, String),
}

#[derive(Debug, Clone)]
pub enum BindingType {
    Mutable,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    Identifier(String),
    BinaryExpression(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(LhsExpr, Expr),
    VariableDeclaration(Variable, Expr),
    Expression(Expr),
    Block(Vec<Statement>),
}
