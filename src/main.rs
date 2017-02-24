use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

extern crate ansi_term;
use ansi_term::Style;
use ansi_term::Colour::Red;

// include output of rust-peg given grammar.rustpeg
mod parser {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

mod ast;
mod interpreter;
mod value;
mod operations;

#[derive(Debug)]
enum ProcessingError {
    ParseError(parser::ParseError),
    IoError(io::Error)
}

impl From<io::Error> for ProcessingError {
    fn from(from: io::Error) -> Self {
        ProcessingError::IoError(from)
    }
}

impl From<parser::ParseError> for ProcessingError {
    fn from(from: parser::ParseError) -> Self {
        ProcessingError::ParseError(from)
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 3 || (args[1] != "run" && args[1] != "parse") {
        println!("usage: balloon run|parse FILE");
        return;
    }
    let file_name = &args[2];
    let result = parse_file(file_name).and_then(|ast| {
        if args[1] == "parse" {
            println!("{:#?}", ast);
            Ok(())
        } else {
            interpreter::interpret_program(&ast);
            Ok(())
        }
    });
    if let Err(err) = result {
        match err {
            ProcessingError::ParseError(parse_error) => {
                println!("{:?}", parse_error);
                let (mut line_number, mut column_number) = (parse_error.line, parse_error.column);

                let mut buf_reader = io::BufReader::new(File::open(file_name).unwrap());
                let mut line = buf_reader.by_ref().lines().nth(line_number - 1);

                let line_content;

                // error was in last line which was empty
                if let None = line {
                    line_number -= 1;
                    buf_reader.seek(io::SeekFrom::Start(0)).unwrap();
                    // more helpful to point to end of previous line
                    line = buf_reader.lines().nth(line_number - 1);
                    line_content = line.unwrap().unwrap();
                    column_number = line_content.len() + 1;
                } else {
                    line_content = line.unwrap().unwrap();
                }

                println!("{}: {}: line {}, column_number {}: expected one of {:?}",
                    Style::new().bold().paint(file_name.clone()),
                    Red.bold().paint("parse error"),
                    line_number,
                    column_number,
                    parse_error.expected
                );
                println!("{}", line_content);
                let mut pointer_string = String::from_utf8(vec![b' '; column_number - 1]).unwrap();
                pointer_string.push('^');
                println!("{}", Style::new().bold().paint(pointer_string));
            },
            ProcessingError::IoError(io_error) => {
                match io_error.kind() {
                    io::ErrorKind::NotFound => println!("{}", io_error),
                    e => println!("An error occurred.\n{:?}", e),
                };
            },
        };
    }
}

fn parse_file(name: &String) -> Result<Vec<ast::Statement>, ProcessingError> {
    let mut input_file = File::open(name)?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;
    if !input.ends_with("\n") {
        input.push('\n');
    }
    let x = parser::program(&input);
    Ok(x?)
}
