use parser;
use runtime::Interpreter;
use runtime::StatementResult;
use ast_walk_interpreter::AstWalkInterpreter;
use value::Value;
use value::Number;

fn run_and_get_last_value(code: &str) -> Value {
    match run_and_get_last_result(code) {
        StatementResult::Value(ref v) => v.clone(),
        _ => panic!("Cannot unwrap value"),
    }
}

fn run_and_get_last_result(code: &str) -> StatementResult {
    let ast = parser::program(code);
    match ast {
        Ok(ast) => {
            let mut ast_walk_interpreter = AstWalkInterpreter::new();
            let reference_val = ast_walk_interpreter.run_ast_as_program(&ast)
                .unwrap()
                .unwrap();
            return reference_val;
            /*
             * Test plan forward once LLVM backend is written
            let mut llvm_interpreter = LLVMInterpreter::new()
            let llvm_val = llvm_interpreter.run_ast_as_program(&ast)
                .unwrap().unwrap();

            if (llvm_val == reference_val) {
                reference_val
            }
            else {
                panic!("LLVM value: {:?} does not agree with Reference Value {:?}"
            }
            */
        }
        Err(_) => panic!("{:?}", ast),
    }
}

#[test]
fn literal_value_int() {
    assert_eq!(run_and_get_last_value("7;"),
               Value::Number(Number::Integer(7)));
}

#[test]
fn literal_value_float() {
    assert_eq!(run_and_get_last_value("7.0;"),
               Value::Number(Number::Float(7.0)));
}

#[test]
fn literal_value_bool_true() {
    assert_eq!(run_and_get_last_value("true;"), Value::Bool(true));
}

#[test]
fn literal_value_bool_false() {
    assert_eq!(run_and_get_last_value("false;"), Value::Bool(false));
}

#[test]
fn variable_reassignment_with_type_change() {
    assert_eq!(run_and_get_last_value("var x = 10; x = false; x;"),
               Value::Bool(false));
}

#[test]
fn identifier_name() {
    assert_eq!(run_and_get_last_value("var _x_X123_ = false; _x_X123_;"),
               Value::Bool(false));
}

#[test]
fn arithmetic_1() {
    assert_eq!(run_and_get_last_value("(1 + 3 / (1 + 1)) * 12 - 1;"),
               Value::Number(Number::Float(29.0)));
}

#[test]
fn arithmetic_2() {
    assert_eq!(run_and_get_last_value("3 - 5 + 12 / 4 - 1;"),
               Value::Number(Number::Float(0.0)));
}

#[test]
fn arithmetic_3() {
    assert_eq!(run_and_get_last_value("10 // 3 + 5 - 12;"),
               Value::Number(Number::Integer(-4)));
}

#[test]
fn arithmetic_4() {
    assert_eq!(run_and_get_last_value("-10 / 4 + -5 - 12;"),
               Value::Number(Number::Float(-19.5)));
}

#[test]
fn i64_support() {
    assert_eq!(run_and_get_last_value("9223372036854775807;"),
               Value::Number(Number::Integer(9223372036854775807)));
}

#[test]
fn f64_support() {
    assert_eq!(run_and_get_last_value("1234567890.012345678;"),
               Value::Number(Number::Float(1234567890.012345678)));
}


#[test]
fn logical_and() {
    assert_eq!(run_and_get_last_value("true and true;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("false and true;"),
               Value::Bool(false));
    assert_eq!(run_and_get_last_value("true and false;"),
               Value::Bool(false));
    assert_eq!(run_and_get_last_value("false and false;"),
               Value::Bool(false));

    assert_eq!(run_and_get_last_value("2 and 1;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("3.0 and 22.5;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("5 and 0;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("5.6 and 0.0;"), Value::Bool(false));
}

#[test]
fn logical_or() {
    assert_eq!(run_and_get_last_value("true or true;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("false or true;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("true or false;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("false or false;"),
               Value::Bool(false));

    assert_eq!(run_and_get_last_value("2 or 1;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("3.0 or 22.5;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("5 or 0;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("0 or 0.0;"), Value::Bool(false));
}

#[test]
fn logical_not() {
    assert_eq!(run_and_get_last_value("not true;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("not false;"), Value::Bool(true));

    assert_eq!(run_and_get_last_value("not 5;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("not 0;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("not 0.0;"), Value::Bool(true));
}

#[test]
fn comparisons() {
    assert_eq!(run_and_get_last_value("1 < 2;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("1 > 2;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("1.0 < 2.0;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("1.0 > 2.0;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("4 <= 4;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("2 >= 2;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("3 == 3;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("7 == 7.0;"), Value::Bool(true));
    assert_eq!(run_and_get_last_value("7 == 7.1;"), Value::Bool(false));
    assert_eq!(run_and_get_last_value("true == 1;"), Value::Bool(false));
}

#[test]
fn variable_declaration() {
    assert_eq!(run_and_get_last_value("var x = 5; x;"),
               Value::Number(Number::Integer(5)));
}

#[test]
fn variable_reassignment() {
    assert_eq!(run_and_get_last_value("var x = 10; x = 42; x;"),
               Value::Number(Number::Integer(42)));
}

#[test]
fn id_in_expr() {
    assert_eq!(run_and_get_last_value("var x = 5; x / 2 + x;"),
               Value::Number(Number::Float(7.5)));
}

#[test]
fn create_tuple() {
    assert_eq!(run_and_get_last_value("(1, 2, 3);"),
               Value::Tuple(vec![Value::Number(Number::Integer(1)),
                                 Value::Number(Number::Integer(2)),
                                 Value::Number(Number::Integer(3))]));
}

#[test]
fn add_tuples() {
    assert_eq!(run_and_get_last_value("(\"a\",) + (\"t\",);"),
               Value::Tuple(vec![Value::String("a".to_owned()), Value::String("t".to_owned())]));
}

#[test]
fn index_tuple() {
    assert_eq!(run_and_get_last_value("(1, 2, 3)[0];"),
               Value::Number(Number::Integer(1)));
}

#[test]
fn mix_call_and_index() {
    assert_eq!(run_and_get_last_value("((fn() { return false; },), 2, 3)[0][0]();"),
               Value::Bool(false));
    assert_eq!(run_and_get_last_value("fn() { return fn() { return (1, 2, true); }; }()()[2];"),
               Value::Bool(true));
}

#[test]
fn if_expr_true() {
    assert_eq!(run_and_get_last_value("var x = 5; if 1 < 2 { x = 6; } else { x = 7; } x;"),
               Value::Number(Number::Integer(6)));
}

#[test]
fn if_expr_false() {
    assert_eq!(run_and_get_last_value("var x = 5; if 1 > 2 { x = 6; } else { x = 7; } x;"),
               Value::Number(Number::Integer(7)));
}

#[test]
fn loop_and_break() {
    assert_eq!(run_and_get_last_value("var x = 5; loop { if x > 20 { break; } x = x + 5; } x;"),
               Value::Number(Number::Integer(25)));
}

#[test]
fn block_env() {
    assert_eq!(run_and_get_last_value("var x = 5; { var x = 10; x = 20; } x;"),
               Value::Number(Number::Integer(5)));
    assert_eq!(run_and_get_last_value("var x = 5; { var x = 10; x = 20; x; }"),
               Value::Number(Number::Integer(20)));
}

#[test]
fn test_println_runs() {
    assert_eq!(run_and_get_last_result("println();"), StatementResult::None);
    assert_eq!(run_and_get_last_result("println(5, true,);"),
               StatementResult::None);
}

#[test]
fn test_factorial() {
    assert_eq!(run_and_get_last_value("fn factorial(n) { if n < 2 { return 1; } return n * \
                                       factorial(n - 1); } factorial(5); "),
               Value::Number(Number::Integer(120)));
    assert_eq!(run_and_get_last_value("fn factorial(n) { if n < 2 { return 1; } return n * \
                                       factorial(n - 1); } factorial(8); "),
               Value::Number(Number::Integer(40320)));
}

#[test]
fn test_curried_add() {
    let code = "fn addX(x) {
    return fn(y) {
        return x + y;
    };
}

addX(10)(20);
";
    assert_eq!(run_and_get_last_value(code),
               Value::Number(Number::Integer(30)));
}

#[test]
fn test_y_combinator() {
    let code = "fn Y(f) {
    var lazywrapper = fn () { return Y (f); };
    return f(lazywrapper);
}

fn factorialwrap(lazywrapfact) {
    fn factorial(i) {
        if (i == 0) {
            return 1;
        } else {
            return i * lazywrapfact()(i - 1);
        }
    }
    return factorial;
}

var fact = Y(factorialwrap);

fact(4);
";
    assert_eq!(run_and_get_last_value(code),
               Value::Number(Number::Integer(24)));
}

#[test]
fn test_string_concat() {
    assert_eq!(run_and_get_last_value("\"abc\" + \"def\";"),
               Value::String("abcdef".to_owned()));
    assert_eq!(run_and_get_last_value("\"abc\" + 123;"),
               Value::String("abc123".to_owned()));
    assert_eq!(run_and_get_last_value("\"abc\" + true;"),
               Value::String("abctrue".to_owned()));
    assert_eq!(run_and_get_last_value("456 + \"abc\";"),
               Value::String("456abc".to_owned()));
    assert_eq!(run_and_get_last_value("false + \"abc\";"),
               Value::String("falseabc".to_owned()));
}

#[test]
fn test_len() {
    assert_eq!(run_and_get_last_value("len((1, 2, 3));"),
               Value::Number(Number::Integer(3)));
}
