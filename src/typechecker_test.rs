use parser;
use typechecker::check_program;
use typechecker::Type;
use typechecker::{TypeCheckerIssue, TypeCheckerIssueWithPosition};
use interpreter::InterpreterError;
use ast::{BinaryOp, UnaryOp};

fn check_and_get_result(code: &str) -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let ast = parser::program(code);
    match ast {
        Ok(ast) => {
            let result = check_program(&ast);
            result
        }
        Err(_) => panic!("{:?}", ast),
    }
}

#[test]
fn check_no_reference_error() {
    let result = check_and_get_result("var x = 5; x;");
    assert!(result.is_ok());
}

#[test]
fn check_reference_error() {
    let result = check_and_get_result("var x = 5; y;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::ReferenceError("y".to_owned())), (11, 12))
    ]);
}

#[test]
fn check_no_undeclared_assignment() {
    let result = check_and_get_result("var x = 5;");
    assert!(result.is_ok());
}

#[test]
fn check_undeclared_assignment_error() {
    let result = check_and_get_result("x = 5;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::UndeclaredAssignment("x".to_owned())), (0, 2))
    ]);
}

#[test]
fn check_no_binary_type_error() {
    let result = check_and_get_result("5 + 5.5 < 10 - 2 and 5;");
    assert!(result.is_ok());
}

#[test]
fn check_binary_type_error_add() {
    let result = check_and_get_result("1 + true;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::BinaryTypeError(BinaryOp::Add, Type::Number, Type::Bool)), (0, 8))
    ]);

    let result = check_and_get_result("true + 1;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::BinaryTypeError(BinaryOp::Add, Type::Bool, Type::Number)), (0, 8))
    ]);

    let result = check_and_get_result("false + true;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::BinaryTypeError(BinaryOp::Add, Type::Bool, Type::Bool)), (0, 12))
    ]);
}

#[test]
fn check_no_unary_minus_error() {
    let result = check_and_get_result("-10.9;");
    assert!(result.is_ok());
}

#[test]
fn check_unary_minus_error() {
    let result = check_and_get_result("-false;");
    assert_eq!(result.unwrap_err(), [
        (TypeCheckerIssue::InterpreterError(InterpreterError::UnaryTypeError(UnaryOp::Minus, Type::Bool)), (1, 6))
    ]);
}

#[test]
fn check_multiple_types_from_branch() {
    let result = check_and_get_result("var x = 5; if true { x = true; } else { x = 10; } ");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::MultipleTypesFromBranchWarning("x".to_owned()), (11, 50))]);
}

// #[test]
// fn check_none_error_for_builtin() {
//     let result = check_and_get_result("-println(10);");
//     assert_eq!(result.unwrap_err(), [
//         (TypeCheckerIssue::InterpreterError(InterpreterError::NoneError(Some("println".to_owned()))), (1, 10))
//     ]);
// }
