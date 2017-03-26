use std::rc::Rc;
use std::cell::RefCell;

use fnv::FnvHashMap;

use value::*;
use function::*;

#[derive(Debug)]
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
    pub fn new_root() -> Rc<RefCell<Environment>> {
        let mut env = Environment::new();
        let builtin_functions = &[("println",
                                   Function::NativeVoid(CallSign {
                                                            num_params: 0,
                                                            variadic: true,
                                                        },
                                                        native_println))];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(&name.to_string(), &Value::Function(func.clone()));
        }
        Rc::new(RefCell::new(env))
    }

    pub fn new() -> Environment {
        Environment {
            parent: None,
            symbol_table: FnvHashMap::default(),
        }
    }

    pub fn create_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment {
            parent: Some(parent),
            symbol_table: FnvHashMap::default(),
        };
        Rc::new(RefCell::new(env))
    }

    pub fn declare(&mut self, identifier: &String, value: &Value) {
        self.symbol_table.insert(identifier.clone(), value.clone());
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
            return Some(val.clone());
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow_mut().get_value(identifier),
                None => None,
            }
        }
    }
}
