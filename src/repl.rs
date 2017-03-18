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
                        print_parse_error("repl".to_string(), orig_input, parse_error);
                    }
                    Ok(ast) => {
                        if let Err(e) = machine.interpret_statements(&ast) {
                            match e {
                                InterpreterError::ReferenceError(id) => {
                                    println!("reference error: `{}` was not declared", id);
                                }
                                InterpreterError::UndeclaredAssignment(id) => {
                                    println!("reference error: cannot assign to undeclared `{}` ",
                                             id);
                                }
                                InterpreterError::BinaryTypeError(binary_op, type1, type2) => {
                                    println!("type error: `{}` cannot operate on types {} and {}",
                                             binary_op,
                                             type1,
                                             type2);
                                }
                                InterpreterError::UnaryTypeError(unary_op, typ) => {
                                    println!("type error: `{}` cannot operate on type {}",
                                             unary_op,
                                             typ);
                                }
                                InterpreterError::NoneError(id) => {
                                    println!("missing value error: tried to use return value of non-returning function `{}`", id);
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
    machine.cleanup_for_repl();
}
