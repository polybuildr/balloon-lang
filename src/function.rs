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
        param_list: Vec<String>,
        body: Box<ast::StatementNode>,
        env: Rc<RefCell<Environment>>,
    },
}

impl Function {
    pub fn get_call_sign(&self) -> CallSign {
        match self {
            &Function::NativeVoid(ref call_sign, _) => call_sign.clone(),
            &Function::NativeReturning(ref call_sign, _) => call_sign.clone(),
            &Function::User { ref call_sign, .. } => call_sign.clone(),
        }
    }
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
