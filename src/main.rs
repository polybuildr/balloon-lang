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
mod builtins;

use interpreter::*;

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
    let mut file_name_args_idx = 2;
    let run_mode = match args.len() {
        1 => RunMode::Repl,
        2 => {
            file_name_args_idx = 1;
            RunMode::Run
        }
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

    let file_name = &args[file_name_args_idx];
    let ast_result = parse_file(file_name);
    if let Err(err) = ast_result {
        match err {
            ProcessingError::ParseError(parse_error) => {
                let (parse_error, line_content) = get_error_and_line_for_file(&parse_error,
                                                                              file_name);
                print_parse_error(file_name, line_content, parse_error);
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
        RunMode::Run => interpret_ast(ast, file_name),
        RunMode::Check => check_ast(ast, file_name),
        RunMode::Unknown => print_usage(),
        RunMode::Repl => unreachable!(),
    }
}

fn parse_file(name: &String) -> Result<Vec<ast::StatementNode>, ProcessingError> {
    let mut input_file = File::open(name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x = parser::program(&input);
    Ok(x?)
}

fn read_file(file_name: &String) -> String {
    let mut buf_reader = io::BufReader::new(File::open(file_name).unwrap());
    let mut file_content = String::new();
    buf_reader.read_to_string(&mut file_content).unwrap();
    return file_content;
}

fn interpret_ast(ast: Vec<ast::StatementNode>, file_name: &String) {
    let mut machine = Interpreter::new();
    let result = machine.interpret_program(&ast);
    if let Err(e) = result {
        let file_content = read_file(file_name);
        let span = offset_span_to_source_span(e.1, &file_content);
        print_interpreter_error_for_file(e.0, span, &file_content, file_name);
    }
}

fn check_ast(ast: Vec<ast::StatementNode>, file_name: &String) {
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
