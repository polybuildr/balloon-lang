use value::*;
use ast::{BinaryOp, UnaryOp};
use interpreter::InterpreterError;

pub fn unary_minus(a: Value) -> Result<Value, InterpreterError> {
    match a {
        Value::Number(x) => Ok(Value::Number(-x)),
        x => Err(InterpreterError::UnaryTypeError(UnaryOp::Minus, x)),
    }
}

pub fn add(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::Add, a, b)),
    }
}

pub fn subtract(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::Sub, a, b)),
    }
}

pub fn multiply(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::Mul, a, b)),
    }
}

pub fn divide(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::Div, a, b)),
    }
}

pub fn floor_divide(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.floor_div(&b))),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::FloorDiv, a, b)),
    }
}

pub fn less_than(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::LessThan, a, b)),
    }
}

pub fn less_than_or_equal(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a <= b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::LessThanOrEqual, a, b)),
    }
}
pub fn greater_than(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::GreaterThan, a, b)),
    }
}

pub fn greater_than_or_equal(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a >= b)),
        (a, b) => Err(InterpreterError::BinaryTypeError(BinaryOp::GreaterThanOrEqual, a, b)),
    }
}

pub fn strict_equals(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a == b)),
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
        (_, _) => Ok(Value::Bool(false)),
    }
}
