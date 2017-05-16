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
    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let output_path = out_dir.join("file_tests").with_extension("rs");
    let mut output_file = File::create(&output_path).unwrap();
    output_file
        .write_all(b"
use parser;
use runtime::Interpreter;
use ast_walk_interpreter::AstWalkInterpreter;
use typechecker::TypeChecker;
")?;
    let mut tests = Vec::new();
    tests.append(&mut generate_run_pass_tests()?);
    tests.append(&mut generate_run_fail_tests()?);
    tests.append(&mut generate_typecheck_fail_tests()?);
    let test_fns_str = tests.concat();
    output_file.write_all(test_fns_str.as_bytes())?;
    Ok(())
}

fn generate_run_pass_tests() -> io::Result<Vec<String>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir("tests/run-pass")? {
        let entry = entry?;
        let name = entry.file_name();
        let mut file = File::open(entry.path()).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        let test_name = "run_pass_".to_owned() +
                        &name.into_string()
                             .unwrap()
                             .replace("-", "_")
                             .replace(".", "_");
        tests.push(make_run_pass_test_fn(&test_name, &content));
    }
    Ok(tests)
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
",
            name = name,
            code = code)
}

fn generate_run_fail_tests() -> io::Result<Vec<String>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir("tests/run-fail")? {
        let entry = entry?;
        let name = entry.file_name();
        if entry.path().extension().unwrap() != "bl" {
            continue;
        }
        let content = read_file(entry.path());
        let test_name = "run_fail_".to_owned() +
                        &name.into_string()
                             .unwrap()
                             .replace("-", "_")
                             .replace(".", "_");
        let expected_err_to_str = read_file(entry.path().with_extension("err"));
        tests.push(make_run_fail_test_fn(&test_name, &content, &expected_err_to_str.trim()));
    }
    Ok(tests)
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
",
            name = name,
            code = code,
            expected_err_str = expected_err_str)
}

fn read_file(path: PathBuf) -> String {
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();
    content
}

fn generate_typecheck_fail_tests() -> io::Result<Vec<String>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir("tests/typecheck-fail")? {
        let entry = entry?;
        let name = entry.file_name();
        if entry.path().extension().unwrap() != "bl" {
            continue;
        }
        let content = read_file(entry.path());
        let test_name = "typecheck_fail_".to_owned() +
                        &name.into_string()
                             .unwrap()
                             .replace("-", "_")
                             .replace(".", "_");
        let expected_err_to_str = read_file(entry.path().with_extension("err"));
        tests.push(make_typecheck_fail_test_fn(&test_name, &content, &expected_err_to_str.trim()));
    }
    Ok(tests)
}

fn make_typecheck_fail_test_fn(name: &str, code: &str, expected_err_str: &str) -> String {
    format!("
#[test]
fn {name}() {{
    let code = r#\"{code}\"#;
    let ast = parser::program(code).unwrap();
    let mut checker = TypeChecker::new();
    checker.check_program(&ast);
    let issues = checker.get_issues();
    assert!(!issues.is_empty());
    assert_eq!(
        format!(\"{{:?}}\", issues),
        r#\"{expected_err_str}\"#
    );
}}
",
            name = name,
            code = code,
            expected_err_str = expected_err_str)
}
