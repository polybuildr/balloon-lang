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
mod checker;

use interpreter::InterpreterError;

use error::*;

fn print_usage() {
    println!("usage: balloon run|parse FILE");
}

enum RunMode {
    Parse,
    Run,
    Repl,
    Check,
    Unknown,
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let run_mode = match args.len() {
        1 => RunMode::Repl,
        2 => RunMode::Run,
        3 => {
            if args[1] == "run" {
                RunMode::Run
            } else if args[1] == "parse" {
                RunMode::Parse
            } else if args[1] == "check" {
                RunMode::Check
            } else {
                RunMode::Unknown
            }
        }
        _ => RunMode::Unknown,
    };

    if let RunMode::Repl = run_mode {
        repl::run_repl();
        return;
    }

    let file_name = &args[2];
    let ast_result = parse_file(file_name);
    if let Err(err) = ast_result {
        match err {
            ProcessingError::ParseError(parse_error) => {
                let (parse_error, line_content) = get_error_and_line_for_file(&parse_error,
                                                                              file_name);
                print_parse_error(file_name.to_string(), line_content, parse_error);
                return;
            }
            ProcessingError::IoError(io_error) => {
                match io_error.kind() {
                    io::ErrorKind::NotFound => println!("{}", io_error),
                    e => println!("An error occurred.\n{:?}", e),
                };
                return;
            }
        }
    }

    let ast = ast_result.unwrap();

    match run_mode {
        RunMode::Parse => println!("{:#?}", ast),
        RunMode::Run => interpret_ast(ast),
        RunMode::Check => check_ast(ast),
        RunMode::Unknown => print_usage(),
        RunMode::Repl => unreachable!(),
    }
}

fn parse_file(name: &String) -> Result<Vec<ast::Statement>, ProcessingError> {
    let mut input_file = File::open(name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x = parser::program(&input);
    Ok(x?)
}

fn interpret_ast(ast: Vec<ast::Statement>) {
    let result = interpreter::interpret_program(&ast);
    if let Err(e) = result {
        match e {
            InterpreterError::ReferenceError(id) => {
                println!("reference error: `{}` was not declared", id);
            }
            InterpreterError::UndeclaredAssignment(id) => {
                println!("reference error: cannot assign to undeclared `{}`", id);
            }
            InterpreterError::BinaryTypeError(binary_op, type1, type2) => {
                println!("type error: `{}` cannot operate on types {} and {}",
                         binary_op,
                         type1,
                         type2);
            }
            InterpreterError::UnaryTypeError(unary_op, typ) => {
                println!("type error: `{}` cannot operate on type {}", unary_op, typ);
            }
        }
    }
}

fn check_ast(ast: Vec<ast::Statement>) {
    let result = checker::check_program(&ast);
    println!("{:?}", result);
}
