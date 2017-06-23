/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use parser;
use runtime::Interpreter;
use runtime::StmtResult;
use ast_walk_interpreter::AstWalkInterpreter;
use value::Value;
use value::Number;

fn run_and_get_last_value(code: &str) -> Value {
    match run_and_get_last_result(code) {
        StmtResult::Value(ref v) => v.clone(),
        _ => panic!("Cannot unwrap value"),
    }
}

fn run_and_get_last_result(code: &str) -> StmtResult {
    let ast = parser::program(code);
    match ast {
        Ok(ast) => {
            let mut ast_walk_interpreter = AstWalkInterpreter::new();
            let reference_val = ast_walk_interpreter
                .run_ast_as_program(&ast)
                .unwrap()
                .unwrap();
            return reference_val;
            // Test plan forward once LLVM backend is written
            // let mut llvm_interpreter = LLVMInterpreter::new()
            // let llvm_val = llvm_interpreter.run_ast_as_program(&ast)
            // .unwrap().unwrap();
            //
            // if (llvm_val == reference_val) {
            // reference_val
            // }
            // else {
            // panic!("LLVM value: {:?} does not agree with Reference Value {:?}"
            // }
            //
        }
        Err(_) => panic!("{:?}", ast),
    }
}

#[test]
fn i64_support() {
    assert_eq!(
        run_and_get_last_value("9223372036854775807;"),
        Value::Number(Number::Integer(9223372036854775807))
    );
}

#[test]
fn f64_support() {
    assert_eq!(
        run_and_get_last_value("1234567890.012345678;"),
        Value::Number(Number::Float(1234567890.012345678))
    );
}
