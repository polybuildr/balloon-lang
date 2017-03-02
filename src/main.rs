use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

extern crate ansi_term;

extern crate rustyline;

// include output of rust-peg given grammar.rustpeg
mod parser {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

mod ast;
mod interpreter;
mod value;
mod operations;
mod environment;
mod repl;
mod error;

use interpreter::InterpreterError;

use error::*;

fn main() {
    let args: Vec<_> = env::args().collect();

    if args.len() == 1 {
        repl::run_repl();
        return;
    }

    if args.len() < 3 || (args[1] != "run" && args[1] != "parse") {
        println!("usage: balloon run|parse FILE");
        return;
    }
    let file_name = &args[2];
    let result = parse_file(file_name).and_then(|ast| if args[1] == "parse" {
        println!("{:#?}", ast);
        Ok(())
    } else {
        match interpreter::interpret_program(&ast) {
            Ok(_) => Ok(()),
            Err(e) => Err(e.into()),
        }
    });
    if let Err(err) = result {
        match err {
            ProcessingError::ParseError(parse_error) => {
                let (parse_error, line_content) = get_error_and_line_for_file(&parse_error,
                                                                              file_name);
                print_parse_error(file_name.to_string(), line_content, parse_error);
            }
            ProcessingError::IoError(io_error) => {
                match io_error.kind() {
                    io::ErrorKind::NotFound => println!("{}", io_error),
                    e => println!("An error occurred.\n{:?}", e),
                };
            }
            ProcessingError::InterpreterError(e) => {
                match e {
                    InterpreterError::ReferenceError(id) => {
                        println!("reference error: `{}` was not declared", id);
                    }
                    InterpreterError::UndeclaredAssignment(id) => {
                        println!("reference error: cannot assign to undeclared `{}`", id);
                    }
                    InterpreterError::BinaryTypeError(binary_op, val1, val2) => {
                        println!("type error: `{}` cannot operate on types {} and {}",
                                 binary_op,
                                 val1.get_type_string(),
                                 val2.get_type_string());
                    }
                    InterpreterError::UnaryTypeError(unary_op, val) => {
                        println!("type error: `{}` cannot operate on type {}",
                                 unary_op,
                                 val.get_type_string());
                    }
                }
            }
        };
    }
}

fn parse_file(name: &String) -> Result<Vec<ast::Statement>, ProcessingError> {
    let mut input_file = File::open(name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x = parser::program(&input);
    Ok(x?)
}
