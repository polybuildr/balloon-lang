use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

extern crate ansi_term;

extern crate fnv;

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
mod typechecker;
mod function;

#[cfg(test)]
mod interpreter_test;

#[cfg(test)]
mod typechecker_test;

use interpreter::*;

use error::*;

fn print_usage() {
    println!("usage: balloon [MODE] FILE

where MODE is one of:
--run      (default) runs the file
--check    type check the file
--parse    only parse the file, don't run it");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl::run_repl(),
        2 => run_file(&args[1]),
        3 => {
            match args[1].as_str() {
                "--run" => run_file(&args[2]),
                "--check" => typecheck_file(&args[2]),
                "--parse" => {
                    if let Some(ast) = parse_file(&args[2]) {
                        println!("{:#?}", ast);
                    }
                }
                _ => print_usage(),
            };
        }
        _ => print_usage(),
    };
}

fn parse_file(file_name: &str) -> Option<Vec<ast::StatementNode>> {
    match try_parse_file(file_name) {
        Err(err) => {
            match err {
                ProcessingError::ParseError(parse_error) => {
                    let (parse_error, line_content) = get_error_and_line_for_file(&parse_error,
                                                                                  file_name);
                    print_parse_error(file_name, &line_content, &parse_error);
                }
                ProcessingError::IoError(io_error) => {
                    match io_error.kind() {
                        io::ErrorKind::NotFound => println!("{}", io_error),
                        e => println!("An error occurred.\n{:?}", e),
                    };
                }
            }
            None
        }
        Ok(ast) => Some(ast),
    }
}

fn try_parse_file(file_name: &str) -> Result<Vec<ast::StatementNode>, ProcessingError> {
    let mut input_file = File::open(file_name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x = parser::program(&input);
    Ok(x?)
}

fn run_file(file_name: &str) {
    if let Some(ast) = parse_file(file_name) {
        let mut machine = Interpreter::new();
        let result = machine.run_ast_as_program(&ast);
        if let Err(e) = result {
            let file_content = read_file(file_name);
            let span = offset_span_to_source_span(e.1, &file_content);
            print_interpreter_error_for_file(e.0, span, &file_content, file_name);
        }
    }
}

fn typecheck_file(file_name: &str) {
    if let Some(ast) = parse_file(file_name) {
        let result = typechecker::check_program(&ast);
        match result {
            Ok(_) => println!("No problems detected in {}.", file_name),
            Err(errs) => {
                let num_issues = errs.len();
                let file_content = read_file(file_name);
                for error in errs {
                    let span = offset_span_to_source_span(error.1, &file_content);
                    print_typechecker_error_for_file(error.0, span, &file_content, file_name);
                    println!("");
                }
                println!("{} {} detected in {}.",
                         num_issues,
                         if num_issues > 1 { "issues" } else { "issue" },
                         file_name);
            }
        }
    }
}

fn read_file(file_name: &str) -> String {
    let mut buf_reader = io::BufReader::new(File::open(file_name).unwrap());
    let mut file_content = String::new();
    buf_reader.read_to_string(&mut file_content).unwrap();
    file_content
}
