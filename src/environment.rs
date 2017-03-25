use std::rc::Rc;
use std::cell::RefCell;

use fnv::FnvHashMap;

use value::*;
use ast::*;

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    symbol_table: FnvHashMap<String, Value>,
}

// Testing to see if linked list Env system works
// impl Drop for Environment {
//     fn drop(&mut self) {
//         println!("{:?}", self.symbol_table);
//     }
// }

impl Environment {
    pub fn new() -> Environment {
        Environment { parent: None, symbol_table: FnvHashMap::default() }
    }

    pub fn create_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), symbol_table: FnvHashMap::default() };
        Rc::new(RefCell::new(env))
    }

    pub fn declare(&mut self, variable: &Variable, value: &Value) {
        match *variable {
            Variable::Identifier(_, ref id) => {
                self.symbol_table.insert(id.clone(), *value);
            }
        };
    }

    pub fn set(&mut self, identifier: &String, value: Value) -> bool {
        // TODO: Entry API
        if self.symbol_table.contains_key(identifier) {
            self.symbol_table.insert(identifier.clone(), value);
            return true;
        } else {
            match self.parent {
                Some(ref parent) => return parent.borrow_mut().set(identifier, value),
                None => return false,
            };
        }
    }

    // TODO: Why &mut?
    pub fn get_value(&mut self, identifier: &String) -> Option<Value> {
        if let Some(val) = self.symbol_table.get(identifier) {
            return Some(*val);
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow_mut().get_value(identifier),
                None => None,
            }
        }
    }
}
