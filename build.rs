extern crate peg;

use std::io;
use std::io::{Read, Write};
use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::env;

fn main() {
    peg::cargo_build("src/grammar.rustpeg");
    if cfg!(feature = "file-tests") {
        generate_tests().unwrap();
    }
}

fn generate_tests() -> io::Result<()> {
    generate_run_pass_tests()?;
    generate_run_fail_tests()?;
    Ok(())
}

fn generate_run_pass_tests() -> io::Result<()> {
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let output_path = out_dir.join("run_pass_files").with_extension("rs");
    let mut output_file = File::create(&output_path).unwrap();
    output_file.write_all(b"
use parser;
use runtime::Interpreter;
use ast_walk_interpreter::AstWalkInterpreter;
")?;

    for entry in fs::read_dir("tests/run-pass")? {
        let entry = entry?;
        let name = entry.file_name();
        let mut file = File::open(entry.path()).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        let test_name = name.into_string().unwrap().replace("-", "_").replace(".", "_");
        output_file.write_all(
            make_run_pass_test_fn(&test_name, &content).as_bytes()
        )?;
    }
    Ok(())
}

fn make_run_pass_test_fn(name: &str, code: &str) -> String {
    format!("
#[test]
fn {name}() {{
    let code = r#\"{code}\"#;
    let ast = parser::program(code).unwrap();
    let mut ast_walk_interpreter = AstWalkInterpreter::new();
    ast_walk_interpreter
        .run_ast_as_program(&ast)
        .unwrap();
}}
", name=name, code=code)
}

fn generate_run_fail_tests() -> io::Result<()> {
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let output_path = out_dir.join("run_fail_files").with_extension("rs");
    let mut output_file = File::create(&output_path).unwrap();
    output_file.write_all(b"
use parser;
use runtime::Interpreter;
use ast_walk_interpreter::AstWalkInterpreter;
")?;

    for entry in fs::read_dir("tests/run-fail")? {
        let entry = entry?;
        let name = entry.file_name();
        if entry.path().extension().unwrap() != "bl" {
            continue;
        }
        let content = read_file(entry.path());
        let test_name = name.into_string().unwrap().replace("-", "_").replace(".", "_");
        let expected_err_to_str = read_file(entry.path().with_extension("err"));
        output_file.write_all(
            make_run_fail_test_fn(&test_name, &content, &expected_err_to_str.trim()).as_bytes()
        )?;
    }
    Ok(())
}

fn make_run_fail_test_fn(name: &str, code: &str, expected_err_str: &str) -> String {
    format!("
#[test]
fn {name}() {{
    let code = r#\"{code}\"#;
    let ast = parser::program(code).unwrap();
    let mut ast_walk_interpreter = AstWalkInterpreter::new();
    let err = ast_walk_interpreter
        .run_ast_as_program(&ast)
        .unwrap_err();
    assert_eq!(
        format!(\"{{:?}}\", err),
        r#\"{expected_err_str}\"#
    );
}}
", name=name, code=code, expected_err_str=expected_err_str)
}

fn read_file(path: PathBuf) -> String {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    content
}
