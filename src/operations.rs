use value::*;

pub fn add(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
        (a, b) => {
            panic!("error: operation + is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn subtract(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
        (a, b) => {
            panic!("error: operation - is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn multiply(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
        (a, b) => {
            panic!("error: operation * is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn divide(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
        (a, b) => {
            panic!("error: operation / is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn floor_divide(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a.floor_div(&b)),
        (a, b) => {
            panic!("error: operation / is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn less_than(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Bool(a < b),
        (a, b) => {
            panic!("error: operation < is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn less_than_or_equal(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Bool(a <= b),
        (a, b) => {
            panic!("error: operation <= is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}
pub fn greater_than(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Bool(a > b),
        (a, b) => {
            panic!("error: operation < is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn greater_than_or_equal(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Bool(a >= b),
        (a, b) => {
            panic!("error: operation < is not defined for types {} and {}",
                   a.get_type_string(),
                   b.get_type_string())
        }
    }
}

pub fn strict_equals(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Bool(a == b),
        (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
        (_, _) => Value::Bool(false),
    }
}
