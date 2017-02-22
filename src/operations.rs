use value::*;

pub fn add(a: Value, b: Value) -> Value {
    match (a, b) {
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

pub fn subtract(a: Value, b: Value) -> Value {
    match (a, b) {
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

pub fn multiply(a: Value, b: Value) -> Value {
    match (a, b) {
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

pub fn divide(a: Value, b: Value) -> Value {
    match (a,b) {
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

pub fn less_than(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x < y),
        (Value::Integer(x), Value::Float(y)) => Value::Bool((x as f64) < y),
        (Value::Float(x), Value::Integer(y)) => Value::Bool(x < y as f64),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x < y),
        (a, b) => panic!(
            "error: operation < is not defined for types {} and {}",
            a.get_type_string(),
            b.get_type_string()
        ),
    }
}

pub fn less_than_or_equal(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x <= y),
        (Value::Integer(x), Value::Float(y)) => Value::Bool(x as f64 <= y),
        (Value::Float(x), Value::Integer(y)) => Value::Bool(x <= y as f64),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x <= y),
        (a, b) => panic!(
            "error: operation <= is not defined for types {} and {}",
            a.get_type_string(),
            b.get_type_string()
        ),
    }
}
pub fn greater_than(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x > y),
        (Value::Integer(x), Value::Float(y)) => Value::Bool(x as f64 > y),
        (Value::Float(x), Value::Integer(y)) => Value::Bool(x > y as f64),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x > y),
        (a, b) => panic!(
            "error: operation < is not defined for types {} and {}",
            a.get_type_string(),
            b.get_type_string()
        ),
    }
}

pub fn greater_than_or_equal(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x >= y),
        (Value::Integer(x), Value::Float(y)) => Value::Bool(x as f64 >= y),
        (Value::Float(x), Value::Integer(y)) => Value::Bool(x >= y as f64),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x >= y),
        (a, b) => panic!(
            "error: operation < is not defined for types {} and {}",
            a.get_type_string(),
            b.get_type_string()
        ),
    }
}

