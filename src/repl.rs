use rustyline::error::ReadlineError;
use rustyline::Editor;

use runtime::*;
use error::*;
use parser;
use runtime::StatementResult;

pub fn run_repl<T : Interpreter>(mut machine: T) {
    println!("Balloon REPL");
    let mut rl = Editor::<()>::new();
    // let mut machine = Interpreter::new();

    let file_name = "repl".to_string();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                let orig_input = String::from(line.trim());
                let mut input = orig_input.clone();
                if !input.ends_with(';') && parser::program(&input).is_err() {
                    input.push(';');
                }
                match parser::program(&input) {
                    Err(parse_error) => {
                        print_parse_error(&file_name, &orig_input, &parse_error);
                    }
                    Ok(ast) => {
                        match machine.run_ast_as_statements(&ast) {
                            Err(e) => {
                                let span = offset_span_to_source_span(e.1, &input);
                                print_interpreter_error_for_file(e.0, span, &input, &file_name);
                            }
                            Ok(possible_result) => {
                                if let Some(result) = possible_result {
                                    if let StatementResult::Value(v) = result {
                                        println!("{:?}", v);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
