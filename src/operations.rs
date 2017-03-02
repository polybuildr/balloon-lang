use value::*;
use ast::BinaryOp;
use interpreter::InterpreterError;

pub fn add(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::Add, a, b)),
    }
}

pub fn subtract(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::Sub, a, b)),
    }
}

pub fn multiply(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::Mul, a, b)),
    }
}

pub fn divide(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::Div, a, b)),
    }
}

pub fn floor_divide(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.floor_div(&b))),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::FloorDiv, a, b)),
    }
}

pub fn less_than(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::LessThan, a, b)),
    }
}

pub fn less_than_or_equal(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a <= b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::LessThanOrEqual, a, b)),
    }
}
pub fn greater_than(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::GreaterThan, a, b)),
    }
}

pub fn greater_than_or_equal(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a >= b)),
        (a, b) => Err(InterpreterError::TypeError(BinaryOp::GreaterThanOrEqual, a, b)),
    }
}

pub fn strict_equals(a: Value, b: Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a == b)),
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
        (_, _) => Ok(Value::Bool(false)),
    }
}
