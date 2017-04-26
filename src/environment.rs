use std::rc::Rc;
use std::cell::RefCell;

use fnv::FnvHashMap;

use value::*;
use function::*;
use typechecker::ConstraintType;

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
                                                            param_types: vec![],
                                                        },
                                                        native_println)),
                                  ("run_http_server",
                                   Function::NativeVoid(CallSign {
                                                            num_params: 1,
                                                            variadic: false,
                                                            param_types:
                                                                vec![Some(ConstraintType::Function)],
                                                        },
                                                        native_run_http_server)),
                                  ("len",
                                   Function::NativeReturning(CallSign {
                                                                 num_params: 1,
                                                                 variadic: false,
                                                                 param_types: vec![None],
                                                             },
                                                             native_len))];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(&name.to_string(), &Value::Function(Box::new(func.clone())));
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

    pub fn declare(&mut self, identifier: &str, value: &Value) {
        self.symbol_table
            .insert(identifier.to_owned(), value.clone());
    }

    pub fn set(&mut self, identifier: &str, value: Value) -> bool {
        // TODO: Entry API
        if self.symbol_table.contains_key(identifier) {
            self.symbol_table.insert(identifier.to_owned(), value);
            true
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow_mut().set(identifier, value),
                None => false,
            }
        }
    }

    // TODO: Why &mut?
    pub fn get_value(&mut self, identifier: &str) -> Option<Value> {
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
