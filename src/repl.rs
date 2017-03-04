use rustyline::error::ReadlineError;
use rustyline::Editor;

use environment::Environment;
use interpreter::*;
use error::*;
use parser;

pub fn run_repl() {
    println!("Balloon REPL");
    let mut rl = Editor::<()>::new();
    let mut env = Environment::new();
    env.start_scope();
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
                        if let Err(e) = interpret_statements(&ast, &mut env) {
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
    env.end_scope();
}
