use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

// include output of rust-peg given grammar.rustpeg
mod parser {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

mod ast;

#[derive(Debug)]
enum Error {
    ParseError(parser::ParseError),
    IoError(io::Error)
}

impl From<io::Error> for Error {
    fn from(from: io::Error) -> Self {
        Error::IoError(from)
    }
}

impl From<parser::ParseError> for Error {
    fn from(from: parser::ParseError) -> Self {
        Error::ParseError(from)
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("usage: balloon FILE");
        return;
    }
    let ast = parse_file(args[1].clone());
    match ast {
        Ok(r) => { println!("{:#?}", r); },
        Err(e) => { println!("{:#?}", e); },
    };
}

fn parse_file(name: String) -> Result<Vec<ast::Statement>, Error> {
    let mut input_file = File::open(name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    let x: Result<Vec<ast::Statement>, parser::ParseError> = parser::program(&input);
    Ok(x?)
}
