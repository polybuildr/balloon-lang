use std::rc::Rc;
use std::cell::RefCell;

use ast::*;
use value::*;
use operations;
use environment::Environment;
use typechecker::Type;

#[derive(Debug)]
pub enum InterpreterError {
    /// When an undeclared identifier is used on the RHS
    ReferenceError(String),
    /// When an undeclared identifier is assigned to
    UndeclaredAssignment(String),
    /// When a binary op cannot be performed on the given types
    BinaryTypeError(BinaryOp, Type, Type),
    /// When a unary op cannot be performed on the given type
    UnaryTypeError(UnaryOp, Type),
    /// When a non-returning function's return value is used
    NoneError(Option<String>),
    /// When a call is made to a non-function value
    CallToNonFunction(Option<String>, Type),
    /// When the number of arguments don't match
    ArgumentLength(Option<String>),
}

pub type InterpreterErrorWithPosition = (InterpreterError, OffsetSpan);

#[derive(Debug, PartialEq)]
pub enum StatementResult {
    None,
    Break,
    Value(Value),
}

pub struct Interpreter {
    pub root_env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { root_env: Environment::new_root() }
    }

    pub fn run_ast_as_program(&mut self,
                              program: &Vec<StatementNode>)
                              -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
        interpret_program(program, self.root_env.clone())
    }

    pub fn run_ast_as_statements
        (&mut self,
         statements: &Vec<StatementNode>)
         -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
        interpret_statements(statements, self.root_env.clone())
    }
}

fn interpret_program(program: &Vec<StatementNode>,
                     env: Rc<RefCell<Environment>>)
                     -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
    let result = interpret_statements(program, env.clone())?;
    Ok(result)
}

fn interpret_statements(statements: &Vec<StatementNode>,
                        env: Rc<RefCell<Environment>>)
                        -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
    let mut last_result = None;
    for statement in statements.iter() {
        last_result = Some(interpret_statement(statement, env.clone())?);
    }
    Ok(last_result)
}

fn interpret_statement(s: &StatementNode,
                       env: Rc<RefCell<Environment>>)
                       -> Result<StatementResult, InterpreterErrorWithPosition> {
    match s.data {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &expr)?;
            match variable {
                &Variable::Identifier(ref binding_type, ref name) => {
                    env.borrow_mut().declare(&name, &val);
                }
            };
            Ok(StatementResult::None)
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &expr)?;
            match lhs_expr.data {
                LhsExpr::Identifier(ref id) => {
                    if !env.borrow_mut().set(id, val) {
                        return Err((InterpreterError::UndeclaredAssignment(id.clone()),
                                    lhs_expr.pos));
                    }
                }
            };
            Ok(StatementResult::None)
        }
        Statement::Block(ref statements) => {
            let child_env = Environment::create_child(env.clone());
            let mut last_result = StatementResult::None;
            for statement in statements.iter() {
                last_result = interpret_statement(statement, child_env.clone())?;
                if let StatementResult::Break = last_result {
                    return Ok(StatementResult::Break);
                }
            }
            return Ok(last_result);
        }
        Statement::Expression(ref expr) => {
            let val = interpret_expr(expr, env.clone())?;
            match val {
                None => Ok(StatementResult::None),
                Some(x) => Ok(StatementResult::Value(x)),
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            let possible_val = interpret_expr(if_expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &if_expr)?;
            if val.is_truthy() {
                if let StatementResult::Break = interpret_statement(then_block, env.clone())? {
                    return Ok(StatementResult::Break);
                }
            }
            return Ok(StatementResult::None);
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let possible_val = interpret_expr(if_expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &if_expr)?;
            if val.is_truthy() {
                if let StatementResult::Break = interpret_statement(then_block, env.clone())? {
                    return Ok(StatementResult::Break);
                }
            } else {
                if let StatementResult::Break = interpret_statement(else_block, env.clone())? {
                    return Ok(StatementResult::Break);
                }
            }
            Ok(StatementResult::None)
        }
        Statement::Loop(ref block) => {
            let child_env = Environment::create_child(env.clone());
            loop {
                if let StatementResult::Break = interpret_statement(block, child_env.clone())? {
                    break;
                }
            }
            Ok(StatementResult::None)
        }
        Statement::Break => Ok(StatementResult::Break),
        Statement::Empty => Ok(StatementResult::None),
    }
}
fn interpret_expr(e: &ExprNode,
                  env: Rc<RefCell<Environment>>)
                  -> Result<Option<Value>, InterpreterErrorWithPosition> {
    match e.data {
        Expr::Literal(ref x) => Ok(Some(Value::from(x.clone()))),
        Expr::Identifier(ref id) => {
            match env.borrow_mut().get_value(&id) {
                Some(v) => Ok(Some(v)),
                None => Err((InterpreterError::ReferenceError(id.clone()), e.pos)),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &expr)?;
            match *op {
                UnaryOp::Minus => {
                    match operations::unary_minus(val) {
                        Ok(v) => Ok(Some(v)),
                        Err(err) => Err((err, e.pos)),
                    }
                }
            }
        }
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &expr)?;
            match *op {
                LogicalUnaryOp::Not => Ok(Some(Value::Bool(!val.is_truthy()))),
            }
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let possible_val_1 = interpret_expr(expr1, env.clone())?;
            let val1 = check_val_for_none_error(&possible_val_1, &expr1)?;
            let possible_val_2 = interpret_expr(expr2, env.clone())?;
            let val2 = check_val_for_none_error(&possible_val_2, &expr2)?;
            let retval = match *op {
                BinaryOp::Add => operations::add(val1, val2),
                BinaryOp::Sub => operations::subtract(val1, val2),
                BinaryOp::Mul => operations::multiply(val1, val2),
                BinaryOp::Div => operations::divide(val1, val2),
                BinaryOp::FloorDiv => operations::floor_divide(val1, val2),
                BinaryOp::LessThan => operations::less_than(val1, val2),
                BinaryOp::LessThanOrEqual => operations::less_than_or_equal(val1, val2),
                BinaryOp::GreaterThan => operations::greater_than(val1, val2),
                BinaryOp::GreaterThanOrEqual => operations::greater_than_or_equal(val1, val2),
                BinaryOp::StrictEquals => operations::strict_equals(val1, val2),
            };
            match retval {
                Ok(v) => Ok(Some(v)),
                Err(err) => Err((err, e.pos)),
            }
        }
        Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
            match *op {
                LogicalBinaryOp::LogicalAnd => {
                    let possible_val_1 = interpret_expr(expr1, env.clone())?;
                    let val1 = check_val_for_none_error(&possible_val_1, &expr1)?;
                    if !val1.is_truthy() {
                        return Ok(Some(Value::Bool(false)));
                    }
                    let possible_val_2 = interpret_expr(expr2, env.clone())?;
                    let val2 = check_val_for_none_error(&possible_val_2, &expr2)?;
                    Ok(Some(Value::Bool(val2.is_truthy())))
                }
                LogicalBinaryOp::LogicalOr => {
                    let possible_val_1 = interpret_expr(expr1, env.clone())?;
                    let val1 = check_val_for_none_error(&possible_val_1, &expr1)?;
                    if val1.is_truthy() {
                        return Ok(Some(Value::Bool(true)));
                    }
                    let possible_val_2 = interpret_expr(expr2, env.clone())?;
                    let val2 = check_val_for_none_error(&possible_val_2, &expr2)?;
                    Ok(Some(Value::Bool(val2.is_truthy())))
                }
            }
        }
        Expr::FunctionCall(ref expr, ref args) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, &expr)?;
            let func = match val {
                Value::Function(f) => f,
                v => {
                    if let Expr::Identifier(ref id) = expr.data {
                        return Err((InterpreterError::CallToNonFunction(Some(id.clone()),
                                                                        v.get_type()),
                                    e.pos));
                    }
                    return Err((InterpreterError::CallToNonFunction(None, v.get_type()), e.pos));
                }
            };

            let mut arg_vals = Vec::new();
            for arg in args.iter() {
                let possible_val = interpret_expr(arg, env.clone())?;
                let val = check_val_for_none_error(&possible_val, &arg)?;
                arg_vals.push(val);
            }
            use function::*;
            match func {
                Function::NativeVoid(call_sign, native_fn) => {
                    if !call_sign.variadic && call_sign.num_params != arg_vals.len() {
                        if let Expr::Identifier(ref id) = expr.data {
                            return Err((InterpreterError::ArgumentLength(Some(id.clone())), e.pos));
                        }
                        return Err((InterpreterError::ArgumentLength(None), e.pos));
                    }
                    native_fn(arg_vals);
                    Ok(None)
                }
                Function::NativeReturning(call_sign, native_fn) => {
                    if !call_sign.variadic && call_sign.num_params != arg_vals.len() {
                        if let Expr::Identifier(ref id) = expr.data {
                            return Err((InterpreterError::ArgumentLength(Some(id.clone())), e.pos));
                        }
                        return Err((InterpreterError::ArgumentLength(None), e.pos));
                    }
                    Ok(Some(native_fn(arg_vals)))
                }
                Function::User { returning, call_sign, body, env } => {
                    unimplemented!();
                }
            }
        }
    }
}

fn check_val_for_none_error(val: &Option<Value>, expr: &ExprNode) -> Result<Value, InterpreterErrorWithPosition> {
    if let &None = val {
        if let Expr::FunctionCall(ref f_expr, _) = expr.data {
            if let Expr::Identifier(ref id) = f_expr.data {
                return Err((InterpreterError::NoneError(Some(id.clone())), expr.pos));
            }
            return Err((InterpreterError::NoneError(None), expr.pos));
        }
    }
    Ok(val.clone().unwrap())
}
