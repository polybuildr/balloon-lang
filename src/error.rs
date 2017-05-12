use std::io::prelude::*;
use std::io;
use std::fs::File;
use std::str;

use ansi_term::Style;
use ansi_term::Colour::{Red, Yellow};

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
    if line.is_none() {
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

pub fn print_parse_error(file_name: &str, line_content: &str, parse_error: &parser::ParseError) {
    println!("{}: {}: line {}, col {}: expected one of {:?}",
             Style::new().bold().paint((*file_name).to_owned()),
             Red.bold().paint("parse error"),
             parse_error.line,
             parse_error.column,
             parse_error.expected);
    println!("{}", line_content);
    let mut pointer_string = String::from_utf8(vec![b' '; parse_error.column - 1]).unwrap();
    pointer_string.push('^');
    println!("{}", Style::new().bold().paint(pointer_string));
}

use ast::OffsetSpan;

#[derive(Debug)]
pub struct SourceSpan {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

pub fn offset_span_to_source_span(span: OffsetSpan, input: &str) -> SourceSpan {
    let (start_line, start_col) = offset_to_line_and_col(input, span.0);
    let (end_line, end_col) = offset_to_line_and_col(input, span.1 - 1);
    SourceSpan {
        start_line: start_line,
        start_col: start_col,
        end_line: end_line,
        end_col: end_col,
    }
}

fn offset_to_line_and_col(input: &str, pos: usize) -> (usize, usize) {
    let mut remaining = pos;
    let mut line_num: usize = 1;
    for line in input.lines() {
        let line_length = line.len() + 1;
        if remaining < line_length {
            return (line_num, remaining + 1);
        }
        remaining -= line_length;
        line_num += 1;
    }
    (line_num, remaining + 1)
}

use runtime::RuntimeError;
use typechecker::TypeCheckerIssue;

fn adjust_source_span(span: &mut SourceSpan, file_content: &str) {
    if (span.start_line != span.end_line) && span.end_col == 1 {
        span.end_col = 0;
    }
    while (span.end_col == 0) && span.end_line > span.start_line {
        span.end_line -= 1;
        span.end_col = file_content
            .lines()
            .nth(span.end_line - 1)
            .unwrap()
            .len();
    }
}

pub fn print_interpreter_error_for_file(err: RuntimeError,
                                        span: SourceSpan,
                                        file_content: &str,
                                        file_name: &str) {
    print_typechecker_error_for_file(TypeCheckerIssue::RuntimeError(err),
                                     span,
                                     file_content,
                                     file_name);
}

pub fn print_typechecker_error_for_file(err: TypeCheckerIssue,
                                        span: SourceSpan,
                                        file_content: &str,
                                        file_name: &str) {
    let mut span = span;
    let mut error_to_print_after_this = None;
    adjust_source_span(&mut span, file_content);
    if span.start_line == span.end_line {
        println!("in {}, line {}, col {}:",
                 Style::new().bold().paint(file_name.to_string()),
                 span.start_line,
                 span.start_col);
    } else {
        println!("in {}, starting on line {}, col {}:",
                 Style::new().bold().paint(file_name.to_string()),
                 span.start_line,
                 span.start_col);
    }
    let mut is_error = true;
    match err {
        TypeCheckerIssue::RuntimeError(e) => {
            match e {
                RuntimeError::ReferenceError(id) => {
                    println!("{}: `{}` was not declared",
                             Red.bold().paint("reference error"),
                             id);
                }
                RuntimeError::UndeclaredAssignment(id) => {
                    println!("{}: cannot assign to undeclared `{}`",
                             Red.bold().paint("reference error"),
                             id);
                }
                RuntimeError::BinaryTypeError(binary_op, type1, type2) => {
                    println!("{}: `{}` cannot operate on types {} and {}",
                             Red.bold().paint("type error"),
                             binary_op,
                             type1,
                             type2);
                }
                RuntimeError::UnaryTypeError(unary_op, typ) => {
                    println!("{}: `{}` cannot operate on type {}",
                             Red.bold().paint("type error"),
                             unary_op,
                             typ);
                }
                RuntimeError::NoneError(possible_id) => {
                    match possible_id {
                        Some(id) => {
                            println!("{}: tried to use return value of non-returning function \
                                      `{}`",
                                     Red.bold().paint("missing value error"),
                                     id);
                        }
                        None => {
                            println!("{}: tried to use return value of non-returning function",
                                     Red.bold().paint("missing value error"));
                        }
                    }
                }
                RuntimeError::CallToNonFunction(possible_id, other_type) => {
                    match possible_id {
                        Some(id) => {
                            println!("{}: cannot call `{}` ({}) as Function",
                                     Red.bold().paint("type error"),
                                     id,
                                     other_type);
                        }
                        None => {
                            println!("{}: cannot call {} as Function",
                                     Red.bold().paint("type error"),
                                     other_type);
                        }
                    }
                }
                RuntimeError::ArgumentLength(possible_id) => {
                    match possible_id {
                        Some(id) => {
                            println!("{}: function `{}` called with incorrect number of arguments",
                                     Red.bold().paint("arguments mismatch"),
                                     id);
                        }
                        None => {
                            println!("{}: function called with incorrect number of arguments",
                                     Red.bold().paint("arguments mismatch"));
                        }
                    }
                }
                RuntimeError::GeneralRuntimeError(message) => {
                    println!("{}: {}", Red.bold().paint("runtime error"), message);
                }
                RuntimeError::InsideFunctionCall(error_with_position) => {
                    println!("{}:", Red.bold().paint("error in function call"));
                    let unboxed_error_with_position = *error_with_position;
                    let (next_error, next_pos) = unboxed_error_with_position;
                    error_to_print_after_this = Some((TypeCheckerIssue::RuntimeError(next_error),
                                                      next_pos));
                }
                RuntimeError::IndexOutOfBounds(index) => {
                    println!("{}: index `{}` is out of bounds of the tuple",
                             Red.bold().paint("index of out bounds"),
                             index);
                }
                RuntimeError::SubscriptOnNonSubscriptable(typ) => {
                    println!("{}: cannot subscript type {}",
                             Red.bold().paint("type error"),
                             typ);
                }
                RuntimeError::NonIntegralSubscript(typ) => {
                    println!("{}: cannot use non-integral {:?} as subscript",
                             Red.bold().paint("non integral subscript"),
                             typ);
                }
                RuntimeError::BreakOutsideLoop => {
                    println!("{}: break statement appeared outside of a loop",
                             Red.bold().paint("break outside loop"));
                }
                RuntimeError::ContinueOutsideLoop => {
                    println!("{}: continue statement appeared outside of a loop",
                             Red.bold().paint("continue outside loop"));
                }
                RuntimeError::ReturnOutsideFunction => {
                    println!("{}: return statement appeared outside of a function",
                             Red.bold().paint("return outside function"));
                }
            }
        }
        TypeCheckerIssue::MultipleTypesFromBranchWarning(id) => {
            println!("{}: `{}` gets different types in branches",
                     Style::new().bold().paint("multiple types from branch"),
                     id);
            is_error = false;
        }
        TypeCheckerIssue::InsideFunctionCall(issue_with_position) => {
            println!("{}:", Red.bold().paint("issue in function call"));
            error_to_print_after_this = Some(*issue_with_position);
        }
        TypeCheckerIssue::FunctionReturnsMultipleTypes => {
            println!("{}: different branches of the function return different types",
                     Style::new()
                         .bold()
                         .paint("function returns multiple types"));
            is_error = false;
        }
        TypeCheckerIssue::UnreachableCodeAfterReturn => {
            println!("{}: code was found after a return statement, making it unreachable",
                     Style::new().bold().paint("unreachable code"));
            is_error = false;
        }
        TypeCheckerIssue::PossibleNoneError(possible_id) => {
            match possible_id {
                Some(id) => {
                    println!("{}: tried to use return value of function `{}` that \
                             does not always return a value",
                             Red.bold().paint("possibly missing value"),
                             id);
                }
                None => {
                    println!("{}: tried to use return value of function that \
                             does not always return a value",
                             Red.bold().paint("possibly missing value"));
                }
            }
        }
    }

    if span.start_line == span.end_line {
        let left_padding_size = span.start_line.to_string().len() + 3;
        println!("{} | {}",
                 span.start_line,
                 file_content.lines().nth(span.start_line - 1).unwrap());
        let left_padding = String::from_utf8(vec![b' '; span.start_col + left_padding_size - 1])
            .unwrap();
        let pointer_string = String::from_utf8(vec![b'^'; span.end_col + 1 - span.start_col])
            .unwrap();
        println!("{}{}", left_padding, Yellow.bold().paint(pointer_string));
    } else {
        let first_line_start_bytes = file_content
            .lines()
            .nth(span.start_line - 1)
            .unwrap()
            .bytes()
            .take(span.start_col - 1)
            .collect::<Vec<u8>>();
        let first_line_start_string = str::from_utf8(&first_line_start_bytes).unwrap();
        let first_line_rest_bytes = file_content
            .lines()
            .nth(span.start_line - 1)
            .unwrap()
            .bytes()
            .skip(span.start_col - 1)
            .collect::<Vec<u8>>();
        let first_line_rest_string = str::from_utf8(&first_line_rest_bytes).unwrap();

        let max_idx_width = span.end_line.to_string().len();

        println!("{line_num:width$} |", line_num = "", width = max_idx_width);

        println!("{line_num:width$} | {}{}",
                 first_line_start_string,
                 if is_error {
                         Red.bold()
                     } else {
                         Style::new().bold()
                     }
                     .paint(first_line_rest_string),
                 line_num = span.start_line,
                 width = max_idx_width);

        for line_num in span.start_line..span.end_line - 1 {
            println!("{line_num:width$} | {}",
                     if is_error {
                             Red.bold()
                         } else {
                             Style::new().bold()
                         }
                         .paint(file_content.lines().nth(line_num).unwrap()),
                     line_num = line_num + 1,
                     width = max_idx_width);
        }

        if span.end_col != 0 {
            let last_line_start_bytes = file_content
                .lines()
                .nth(span.end_line - 1)
                .unwrap()
                .bytes()
                .take(span.end_col)
                .collect::<Vec<u8>>();
            let last_line_start_string = str::from_utf8(&last_line_start_bytes).unwrap();
            let last_line_rest_bytes = file_content
                .lines()
                .nth(span.end_line - 1)
                .unwrap()
                .bytes()
                .skip(span.end_col)
                .collect::<Vec<u8>>();
            let last_line_rest_string = str::from_utf8(&last_line_rest_bytes).unwrap();
            println!("{line_num:width$} | {}{}",
                     if is_error {
                             Red.bold()
                         } else {
                             Style::new().bold()
                         }
                         .paint(last_line_start_string),
                     last_line_rest_string,
                     line_num = span.end_line,
                     width = max_idx_width);
        }

        println!("{line_num:width$} |", line_num = "", width = max_idx_width);
    }
    if let Some(next_err) = error_to_print_after_this {
        let span = offset_span_to_source_span(next_err.1, file_content);
        print_typechecker_error_for_file(next_err.0,
                                         span,
                                         file_content,
                                         &("function in ".to_owned() + file_name));
    }
}
