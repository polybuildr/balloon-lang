use std::rc::Rc;
use std::cell::RefCell;

use value::*;
use ast;
use environment::Environment;

#[derive(Clone, Debug)]
pub struct CallSign {
    pub num_params: usize,
    pub variadic: bool,
}

#[derive(Clone, Debug)]
pub enum Function {
    NativeVoid(CallSign, fn(Vec<Value>)),
    NativeReturning(CallSign, fn(Vec<Value>) -> Value),
    User {
        returning: bool,
        call_sign: CallSign,
        body: Vec<ast::StatementNode>,
        env: Rc<RefCell<Environment>>,
    },
}

pub fn native_println(args: Vec<Value>) {
    if args.len() == 0 {
        return;
    }
    if args.len() == 1 {
        println!("{}", args.get(0).unwrap());
    } else {
        print!("{}", args.get(0).unwrap());
        for arg in args.iter().skip(1) {
            print!(" {}", arg);
        }
        println!("");
    }
}
