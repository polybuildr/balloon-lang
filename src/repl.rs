use rustyline::error::ReadlineError;
use rustyline::Editor;

use interpreter::*;
use error::*;
use parser;

pub fn run_repl() {
    println!("Balloon REPL");
    let mut rl = Editor::<()>::new();
    let mut machine = Interpreter::new();
    machine.setup_for_repl();
    let file_name = "repl".to_string();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                let orig_input = String::from(line.trim());
                let mut input = orig_input.clone();
                if !input.ends_with(";") {
                    input.push(';');
                }
                match parser::program(&input) {
                    Err(parse_error) => {
                        print_parse_error(&file_name, orig_input, parse_error);
                    }
                    Ok(ast) => {
                        if let Err(e) = machine.interpret_statements(&ast) {
                            let span = offset_span_to_source_span(e.1, &input);
                            print_interpreter_error_for_file(e.0, span, &input, &file_name);
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
    machine.cleanup_for_repl();
}
