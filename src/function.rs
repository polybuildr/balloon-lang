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
        match *self {
            Function::NativeVoid(ref call_sign, _) |
            Function::NativeReturning(ref call_sign, _) |
            Function::User { ref call_sign, .. } => call_sign.clone(),
        }
    }
}

#[allow(needless_pass_by_value)]
pub fn native_println(args: Vec<Value>) {
    if args.is_empty() {
        return;
    }
    if args.len() == 1 {
        println!("{}", args[0]);
    } else {
        print!("{}", args[0]);
        for arg in args.iter().skip(1) {
            print!(" {}", arg);
        }
        println!("");
    }
}
