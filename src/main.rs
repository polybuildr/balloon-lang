use std::env;
use std::io::prelude::*;
use std::io;
use std::fs::File;

extern crate ansi_term;
use ansi_term::Style;
use ansi_term::Colour::Red;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

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
    
    if args.len() == 1 {
        repl();
        return;
    }

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
                let (parse_error, line_content) = get_error_and_line_for_file(&parse_error, file_name);
                print_parse_error(file_name.to_string(), line_content, parse_error);
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
    let x = parser::program(&input);
    Ok(x?)
}

fn  get_error_and_line_for_file(parse_error: &parser::ParseError, file_name: &str) -> (parser::ParseError, String) {
    let mut parse_error = parse_error.clone();
    let mut buf_reader = io::BufReader::new(File::open(file_name).unwrap());
    let mut line = buf_reader.by_ref().lines().nth(parse_error.line - 1);

    let line_content;

    // error was in last line which was empty
    if let None = line {
        parse_error.line -= 1;
        buf_reader.seek(io::SeekFrom::Start(0)).unwrap();
        // more helpful to point to end of previous line
        line = buf_reader.lines().nth(parse_error.line - 1);
        line_content = line.unwrap().unwrap();
        parse_error.column = line_content.len() + 1;
    } else {
        line_content = line.unwrap().unwrap();
    }
    (parse_error, line_content)
}

fn print_parse_error(file_name: String, line_content: String, parse_error: parser::ParseError) {
    println!("{}: {}: line {}, col {}: expected one of {:?}",
        Style::new().bold().paint(file_name.clone()),
        Red.bold().paint("parse error"),
        parse_error.line,
        parse_error.column,
        parse_error.expected
    );
    println!("{}", line_content);
    let mut pointer_string = String::from_utf8(vec![b' '; parse_error.column - 1]).unwrap();
    pointer_string.push('^');
    println!("{}", Style::new().bold().paint(pointer_string));
}

fn repl() {
    println!("Balloon REPL");
    let mut rl = Editor::<()>::new();
    let mut repl = interpreter::Repl::new();
    repl.start();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let orig_input = String::from(line.trim());
                let mut input = orig_input.clone();
                if !input.ends_with(";") {
                    input.push(';');
                }
                match parser::program(&input) {
                    Err(parse_error) => {
                        print_parse_error("repl".to_string(), orig_input, parse_error);
                    },
                    Ok(ast) => repl.execute(&ast),
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    repl.end();
}
