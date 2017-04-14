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

#[allow(needless_pass_by_value)]
pub fn native_run_http_server(args: Vec<Value>) {
    use std::net::{TcpListener, Shutdown};
    use interpreter::call_func;
    use std::io::Write;

    let ref handler_val = args[0];

    let handler_func = match *handler_val {
        Value::Function(ref f) => f,
        _ => panic!("http_server: handler is not a Function"),
    };
    let server = TcpListener::bind("0.0.0.0:8000").unwrap();
    for stream in server.incoming() {
        let handler_response = call_func(handler_func, &vec![]);

        let handler_response_value = if let Ok(Some(val)) = handler_response {
            val
        } else {
            panic!("http_server: handler did not return a value");
        };

        match stream {
            Ok(mut stream) => {
                let response =
                    "HTTP/1.0 200 OK\r\nServer:Balloon\r\nContent-Type:text/html\r\n\r\n"
                        .to_owned() + &handler_response_value.to_string();
                stream.write_all(response.as_bytes()).unwrap();
                stream.flush().unwrap();
                stream.shutdown(Shutdown::Both).unwrap();
            }
            Err(e) => {
                println!("ERROR: {:?}", e);
            }
        }
    }
}
