use std::io::prelude::*;
use std::io;
use std::fs::File;

use ansi_term::Style;
use ansi_term::Colour::Red;

use parser;

#[derive(Debug)]
pub enum ProcessingError {
    ParseError(parser::ParseError),
    IoError(io::Error),
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

pub fn get_error_and_line_for_file(parse_error: &parser::ParseError,
                                   file_name: &str)
                                   -> (parser::ParseError, String) {
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

pub fn print_parse_error(file_name: String,
                         line_content: String,
                         parse_error: parser::ParseError) {
    println!("{}: {}: line {}, col {}: expected one of {:?}",
             Style::new().bold().paint(file_name.clone()),
             Red.bold().paint("parse error"),
             parse_error.line,
             parse_error.column,
             parse_error.expected);
    println!("{}", line_content);
    let mut pointer_string = String::from_utf8(vec![b' '; parse_error.column - 1]).unwrap();
    pointer_string.push('^');
    println!("{}", Style::new().bold().paint(pointer_string));
}
