use std::rc::Rc;
use std::cell::RefCell;

use value::*;
use ast;
use environment::Environment;
use runtime::RuntimeError;

#[derive(Clone, Debug)]
pub struct CallSign {
    pub num_params: usize,
    pub variadic: bool,
}

#[derive(Clone, Debug)]
pub enum Function {
    NativeVoid(CallSign, fn(Vec<Value>) -> Result<(), RuntimeError>),
    NativeReturning(CallSign, fn(Vec<Value>) -> Result<Value, RuntimeError>),
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
pub fn native_println(args: Vec<Value>) -> Result<(), RuntimeError> {
    if args.is_empty() {
        return Ok(());
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
    Ok(())
}

#[allow(needless_pass_by_value)]
pub fn native_len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let val = &args[0];
    match *val {
        Value::Tuple(ref v) => Ok(Value::Number(Number::Integer(v.len() as i64))),
        ref non_tuple_val => Err(RuntimeError::GeneralRuntimeError(format!("cannot get len of {:?}", non_tuple_val.get_type()))),
    }
}

#[allow(needless_pass_by_value)]
pub fn native_run_http_server(args: Vec<Value>) -> Result<(), RuntimeError> {
    use std::net::{TcpListener, Shutdown};
    // TODO: refactor this to not call into ast_walk_interpreter
    use ast_walk_interpreter::call_func;
    use std::io::Write;

    let ref handler_val = args[0];

    let handler_func = match *handler_val {
        Value::Function(ref f) => f,
        _ => {
            return Err(RuntimeError::GeneralRuntimeError("http_server: handler is not a Function"
                .to_owned()))
        }
    };
    let server = TcpListener::bind("0.0.0.0:8000").unwrap();
    for stream in server.incoming() {
        let handler_response = call_func(handler_func, &vec![]);

        let handler_response_value = if let Ok(Some(val)) = handler_response {
            val
        } else {
            return Err(RuntimeError::GeneralRuntimeError("http_server: handler did not return a \
                                                          value"
                .to_owned()));
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
                return Err(RuntimeError::GeneralRuntimeError(format!("http_server: Error in \
                                                                      TcpStream: {:?}",
                                                                     e)
                    .to_owned()));
            }
        }
    }
    Ok(())
}
