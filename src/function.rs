/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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
        call_sign: CallSign,
        param_names: Vec<String>,
        body: Box<ast::StmtNode>,
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

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
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

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn native_assert(args: Vec<Value>) -> Result<(), RuntimeError> {
    let val = &args[0];
    if !val.is_truthy() {
        Err(RuntimeError::GeneralRuntimeError(
            format!("assert: assertion failed for value {}", val),
        ))
    } else {
        Ok(())
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn native_assert_eq(args: Vec<Value>) -> Result<(), RuntimeError> {
    let (val1, val2) = (&args[0], &args[1]);

    if val1 != val2 {
        Err(RuntimeError::GeneralRuntimeError(
            format!("assert_eq: {} != {}", val1, val2),
        ))
    } else {
        Ok(())
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn native_len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let val = &args[0];
    match *val {
        Value::Tuple(ref v) => Ok(Value::Number(Number::Integer(v.len() as i64))),
        ref non_tuple_val => {
            Err(RuntimeError::GeneralRuntimeError(
                format!("cannot get len of {:?}", non_tuple_val.get_type()),
            ))
        }
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn native_run_http_server(args: Vec<Value>) -> Result<(), RuntimeError> {
    use hyper::server::{Server, Request, Response};
    use hyper::header::ContentType;
    use hyper::uri::RequestUri;
    use std::sync::mpsc::channel;
    use std::sync::Mutex;
    use std::thread;
    use std::sync::PoisonError;

    use ast_walk_interpreter::call_func;

    let handler_val = &args[0];

    let handler_func = match *handler_val {
        Value::Function(ref f) => f,
        _ => {
            return Err(RuntimeError::GeneralRuntimeError(
                "http_server: handler is not a Function".to_owned(),
            ))
        }
    };

    let maybe_hyper_server = Server::http("0.0.0.0:8000");

    if let Err(e) = maybe_hyper_server {
        return Err(RuntimeError::GeneralRuntimeError(
            format!("http_server: {}", e),
        ));
    }

    let server = maybe_hyper_server.unwrap();
    // channel from server to interpreter
    let (sender, receiver) = channel();
    let sender_mutex = Mutex::new(sender);
    thread::spawn(|| {
        println!("http_server: listening on 0.0.0.0:8000 on a new thread");
        let handle_result = server.handle(move |req: Request, mut res: Response| {
            let sender = match sender_mutex.lock() {
                Ok(sender) => sender,
                Err(PoisonError { .. }) => panic!("http_server: threading error (lock poisoned)"),
            };
            if let RequestUri::AbsolutePath(path) = req.uri {
                // channel from interpreter to server
                let (rev_sender, rev_receiver) = channel();
                if sender.send((path, rev_sender)).is_err() {
                    panic!("http_server: threading error (could not send on reverse channel)");
                }
                let response_string: String = match rev_receiver.recv() {
                    Ok(response_string) => response_string,
                    Err(_) => {
                        // assume some clean disconnect
                        return;
                    }
                };
                res.headers_mut().set(ContentType::html());
                if res.send(response_string.as_bytes()).is_err() {
                    panic!("http_server: could not send response");
                }
            } else {
                panic!("http_server: unknown kind of request");
            }
        });
        if handle_result.is_err() {
            panic!("http_server: could not handle requests");
        }
    });

    loop {
        match receiver.recv() {
            Err(_) => {
                // assume some clean disconnect
                break;
            }
            Ok(msg) => {
                let (path, sender) = msg;
                let possible_response_value = call_func(handler_func, &[Value::String(path)])?;
                let response_value = match possible_response_value {
                    None => {
                        return Err(RuntimeError::GeneralRuntimeError(
                            "http_server: handler \
                             function did not return a \
                             value"
                                .to_owned(),
                        ));
                    }
                    Some(val) => val,
                };
                if sender.send(response_value.to_string()).is_err() {
                    return Err(RuntimeError::GeneralRuntimeError(
                        "http_server: threading error \
                         (could not send on reverse \
                         channel)"
                            .to_owned(),
                    ));
                }
            }
        }
    }

    Ok(())
}
