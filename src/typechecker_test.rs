use parser;
use typechecker::check_program;
use typechecker::{TypeCheckerIssue, TypeCheckerIssueWithPosition};
use runtime::RuntimeError;
use typechecker::{Type, ConstraintType};
use ast::*;

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
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::ReferenceError("y".to_owned())),
                 (11, 12))]);
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
        (TypeCheckerIssue::RuntimeError(RuntimeError::UndeclaredAssignment("x".to_owned())), (0, 2))
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
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::BinaryTypeError(BinOp::Add,
                                                                              Type::Number,
                                                                              Type::Bool)),
                 (0, 8))]);

    let result = check_and_get_result("true + 1;");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::BinaryTypeError(BinOp::Add,
                                                                              Type::Bool,
                                                                              Type::Number)),
                 (0, 8))]);

    let result = check_and_get_result("false + true;");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::BinaryTypeError(BinOp::Add,
                                                                              Type::Bool,
                                                                              Type::Bool)),
                 (0, 12))]);
}

#[test]
fn check_no_unary_minus_error() {
    let result = check_and_get_result("-10.9;");
    assert!(result.is_ok());
}

#[test]
fn check_unary_minus_error() {
    let result = check_and_get_result("-false;");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::UnaryTypeError(UnOp::Neg,
                                                                             Type::Bool)),
                 (1, 6))]);
}

#[test]
fn check_multiple_types_from_branch() {
    let result = check_and_get_result("var x = 5; if true { x = true; } else { x = 10; } ");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::MultipleTypesFromBranchWarning("x".to_owned()), (11, 50))]);
}

#[test]
fn check_none_error_for_builtin() {
    let result = check_and_get_result("-println(10);");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::NoneError(Some("println"
                     .to_owned()))),
                 (1, 12))]);
}

#[test]
fn check_arg_mismatch_fail() {
    let result = check_and_get_result("fn f(x) {} f(1);");
    assert!(result.is_ok());
}

#[test]
fn check_arg_mismatch_pass() {
    let result = check_and_get_result("fn f(x) {} f();");
    assert_eq!(result.unwrap_err(),
               [((TypeCheckerIssue::RuntimeError(RuntimeError::ArgumentLength(None))), (11, 14))]);
}

#[test]
fn check_type_error_in_fn() {
    let result = check_and_get_result("fn f() { true + 1; }");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::BinaryTypeError(BinOp::Add,
                                                                              Type::Bool,
                                                                              Type::Number)),
                 (9, 17))]);
}

#[test]
fn check_no_reference_error_in_fn() {
    let result = check_and_get_result("var x = 1; fn f() { x; }");
    assert!(result.is_ok());
}

#[test]
fn check_reference_error_in_fn() {
    let result = check_and_get_result("fn f() { x; }");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::ReferenceError("x".to_owned())),
                 (9, 10))]);
}

#[test]
fn check_nested_multiple_types_error() {
    let result = check_and_get_result("var x = 1;
if 1 {
    if 2 {
        x = true;
    } else {
        x = 1;
    }
} else {
    x = 5;
}");
    assert_eq!(result.unwrap_err(),
               [(TypeCheckerIssue::MultipleTypesFromBranchWarning("x".to_owned()), (22, 81))]);
}

#[test]
fn check_non_integral_subscript() {
    assert_eq!(check_and_get_result("(1, 2, 3)[true];").unwrap_err(),
               [(TypeCheckerIssue::RuntimeError(RuntimeError::NonIntegralSubscript(Type::Bool)),
                 (10, 14))]);
}

#[test]
fn check_function_body_on_call() {
    let code = "fn add(a, b) {
    return a + b;
}

add(true, true);
add(1, 1);
add(true, 1);
add(false, false);";
    assert_eq!(check_and_get_result(code).unwrap_err(), [
        (
            TypeCheckerIssue::InsideFunctionCall(
                Box::new(
                    (
                        TypeCheckerIssue::RuntimeError(
                            RuntimeError::BinaryTypeError(BinOp::Add, Type::Bool, Type::Bool)
                        ),
                        (26, 31)
                    )
                )
            ),
            (36, 51)
        ),
        (
            TypeCheckerIssue::InsideFunctionCall(
                Box::new(
                    (
                        TypeCheckerIssue::RuntimeError(
                            RuntimeError::BinaryTypeError(BinOp::Add, Type::Bool, Type::Number)
                        ),
                        (26, 31)
                    )
                )
            ),
            (64, 76)
        ),
    ]);
}

#[test]
fn check_return_type() {
    let code = "fn add(a, b): Number {
    return true;
}";
    assert_eq!(check_and_get_result(code).unwrap_err(),
               [(TypeCheckerIssue::ReturnTypeMismatch(Some(ConstraintType::Number),
                                                      Some(ConstraintType::Bool)),
                 (27, 40))]);
}

#[test]
fn check_inferred_return_type() {
    let code = "fn bool_id(a): Bool {
    return a;
}

bool_id(1);";
    assert_eq!(check_and_get_result(code).unwrap_err(),
        [(TypeCheckerIssue::InsideFunctionCall(
            Box::new((TypeCheckerIssue::ReturnTypeMismatch(Some(ConstraintType::Bool), Some(ConstraintType::Number)), (26, 36)))
        ), (39, 49))]
    );
}
