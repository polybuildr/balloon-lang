/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

extern crate ansi_term;

extern crate fnv;

extern crate rustyline;

extern crate linear_map;

extern crate hyper;

#[cfg(feature = "llvm-backend")]
extern crate llvm_sys;
#[cfg(feature = "llvm-backend")]
extern crate itertools;
#[cfg(feature = "llvm-backend")]
extern crate libc;

// include output of rust-peg given grammar.rustpeg
mod parser {
    #![cfg_attr(feature = "cargo-clippy", allow(clippy))]
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

mod ast;
mod runtime;
mod ast_walk_interpreter;
#[cfg(feature = "llvm-backend")]
mod llvm_interpreter;
mod value;
mod operations;
mod environment;
mod repl;
mod error;
mod typechecker;
mod function;

#[cfg(test)]
mod interpreter_test;

#[cfg(all(test, feature = "file-tests"))]
mod file_test {
    include!(concat!(env!("OUT_DIR"), "/file_tests.rs"));
}

use runtime::*;
use ast_walk_interpreter::AstWalkInterpreter;
#[cfg(feature = "llvm-backend")]
use llvm_interpreter::LLVMInterpreter;

use error::*;

// FIXME: How do you represent the usage style in POSIX notation?
fn print_usage() {
    if cfg!(feature = "llvm-backend") {
        println!(
            "usage: balloon [--repl-llvm | [MODE] FILE]

--repl-llvm     launches the experimental REPL"
        );
    } else {
        println!("usage: balloon [[MODE] FILE]");
    }
    println!(
        "
where MODE is one of:
--run           (default) runs the file [FILE]
--check         type check the file [FILE]
--parse         only parse the file [FILE], don't run it

Not passing any arguments to balloon will start the REPL."
    );
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => repl::run_repl(AstWalkInterpreter::new()),
        2 => match args[1].as_str() {
            #[cfg(feature = "llvm-backend")]
            "--repl-llvm" => repl::run_repl(LLVMInterpreter::new()),
            filepath => run_file(filepath, AstWalkInterpreter::new()),
        },
        3 => {
            match args[1].as_str() {
                "--run" => run_file(&args[2], AstWalkInterpreter::new()),
                "--check" => typecheck_file(&args[2]),
                "--parse" => if let Some(ast) = parse_file(&args[2]) {
                    println!("{:#?}", ast);
                },
                _ => print_usage(),
            };
        }
        _ => print_usage(),
    };
}

fn parse_file(file_name: &str) -> Option<Vec<ast::StmtNode>> {
    match try_parse_file(file_name) {
        Err(err) => {
            match err {
                ProcessingError::ParseError(parse_error) => {
                    let (parse_error, line_content) =
                        get_error_and_line_for_file(&parse_error, file_name);
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

fn try_parse_file(file_name: &str) -> Result<Vec<ast::StmtNode>, ProcessingError> {
    let mut input_file = File::open(file_name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x = parser::program(&input);
    Ok(x?)
}

fn run_file<T: Interpreter>(file_name: &str, mut machine: T) {
    if let Some(ast) = parse_file(file_name) {
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
        let mut checker = typechecker::TypeChecker::new();
        checker.check_program(&ast);
        let issues = checker.get_issues();

        if issues.is_empty() {
            println!("No problems detected in {}.", file_name);
        } else {
            let num_issues = issues.len();
            let file_content = read_file(file_name);
            for issue in issues {
                let span = offset_span_to_source_span(issue.1, &file_content);
                print_typechecker_error_for_file(issue.0, span, &file_content, file_name);
                println!("");
            }
            println!(
                "{} {} detected in {}.",
                num_issues,
                if num_issues > 1 { "issues" } else { "issue" },
                file_name
            );
        }
    }
}

fn read_file(file_name: &str) -> String {
    let mut buf_reader = io::BufReader::new(File::open(file_name).unwrap());
    let mut file_content = String::new();
    buf_reader.read_to_string(&mut file_content).unwrap();
    file_content
}
