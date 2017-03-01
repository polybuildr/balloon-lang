use std::collections::HashMap;

use value::*;
use ast::*;

pub struct Environment {
    symbol_tables: Vec<HashMap<String, Value>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            symbol_tables: Vec::new()
        }
    }

    pub fn start_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.symbol_tables.pop();
    }

    pub fn declare(&mut self, variable: &Variable, value: &Value) {
        match *variable {
            Variable::Identifier(_, ref id) => {
                self.symbol_tables.last_mut().unwrap().insert(id.clone(), *value);
            }
        };
    }

    pub fn set(&mut self, identifier: &String, value: Value) {
        for table in self.symbol_tables.iter_mut().rev() {
            // TODO: Entry API
            if table.contains_key(identifier) {
                table.insert(identifier.clone(), value);
                return;
            }
        }
        panic!(format!("reference error: '{}' was not declared", identifier));
    }

    pub fn get_value(&mut self, identifier: &String) -> Value {
        for table in self.symbol_tables.iter().rev() {
            if let Some(val) = table.get(identifier) {
                return *val
            }
        }
        panic!(format!("reference error: '{}' was not declared", identifier));
    }
}
