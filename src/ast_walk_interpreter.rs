use std::rc::Rc;
use std::cell::RefCell;
use std::usize;

use ast::*;
use value::*;
use operations;
use environment::Environment;
use function::*;
use runtime::*;
use typechecker::ConstraintType;

pub struct AstWalkInterpreter {
    pub root_env: Rc<RefCell<Environment>>,
}


impl AstWalkInterpreter {
    pub fn new() -> AstWalkInterpreter {
        AstWalkInterpreter { root_env: Environment::new_root() }
    }
}

fn interpret_program(program: &[StmtNode],
                     env: Rc<RefCell<Environment>>)
                     -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let result = interpret_statements(program, env.clone())?;
    Ok(result)
}

fn interpret_statements(statements: &[StmtNode],
                        env: Rc<RefCell<Environment>>)
                        -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
    let mut last_result = None;
    for statement in statements.iter() {
        last_result = Some(interpret_statement(statement, env.clone())?);
    }
    Ok(last_result)
}

fn interpret_statement(s: &StmtNode,
                       env: Rc<RefCell<Environment>>)
                       -> Result<StmtResult, RuntimeErrorWithPosition> {
    match s.data {
        Stmt::VariableDeclaration(ref variable, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, expr)?;
            match *variable {
                Variable::Identifier(_, ref name) => {
                    env.borrow_mut().declare(name, &val);
                }
            };
            Ok(StmtResult::None)
        }
        Stmt::Assignment(ref lhs_expr, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, expr)?;
            match lhs_expr.data {
                LhsExpr::Identifier(ref id) => {
                    if !env.borrow_mut().set(id, val) {
                        return Err((RuntimeError::UndeclaredAssignment(id.clone()), lhs_expr.pos));
                    }
                }
            };
            Ok(StmtResult::None)
        }
        Stmt::Block(ref statements) => {
            let child_env = Environment::create_child(env.clone());
            let mut last_result = StmtResult::None;
            for statement in statements.iter() {
                last_result = interpret_statement(statement, child_env.clone())?;
                if let StmtResult::Break = last_result {
                    return Ok(StmtResult::Break);
                } else if let StmtResult::Return(_) = last_result {
                    return Ok(last_result);
                }
            }
            Ok(last_result)
        }
        Stmt::Expression(ref expr) => {
            let val = interpret_expr(expr, env.clone())?;
            match val {
                None => Ok(StmtResult::None),
                Some(x) => Ok(StmtResult::Value(x)),
            }
        }
        Stmt::IfThen(ref if_expr, ref then_block) => {
            let possible_val = interpret_expr(if_expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, if_expr)?;
            if val.is_truthy() {
                let result = interpret_statement(then_block, env.clone())?;
                if let StmtResult::Break = result {
                    return Ok(StmtResult::Break);
                } else if let StmtResult::Return(_) = result {
                    return Ok(result);
                }
            }
            Ok(StmtResult::None)
        }
        Stmt::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let possible_val = interpret_expr(if_expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, if_expr)?;
            if val.is_truthy() {
                let result = interpret_statement(then_block, env.clone())?;
                if let StmtResult::Break = result {
                    return Ok(StmtResult::Break);
                } else if let StmtResult::Return(_) = result {
                    return Ok(result);
                }
            } else {
                let result = interpret_statement(else_block, env.clone())?;
                if let StmtResult::Break = result {
                    return Ok(StmtResult::Break);
                } else if let StmtResult::Return(_) = result {
                    return Ok(result);
                }
            }
            Ok(StmtResult::None)
        }
        Stmt::Loop(ref block) => {
            let child_env = Environment::create_child(env.clone());
            loop {
                let result = interpret_statement(block, child_env.clone())?;
                if let StmtResult::Break = result {
                    break;
                } else if let StmtResult::Return(_) = result {
                    return Ok(result);
                }
            }
            Ok(StmtResult::None)
        }
        Stmt::Return(ref possible_expr) => {
            match *possible_expr {
                Some(ref expr) => {
                    let possible_val = interpret_expr(expr, env.clone())?;
                    let val = check_val_for_none_error(&possible_val, expr)?;
                    Ok(StmtResult::Return(Some(val)))
                }
                None => Ok(StmtResult::Return(None)),
            }
        }
        Stmt::Break => Ok(StmtResult::Break),
        Stmt::Empty => Ok(StmtResult::None),
    }
}
fn interpret_expr(e: &ExprNode,
                  env: Rc<RefCell<Environment>>)
                  -> Result<Option<Value>, RuntimeErrorWithPosition> {
    match e.data {
        Expr::Literal(ref x) => Ok(Some(Value::from(x.clone()))),
        Expr::Identifier(ref id) => {
            match env.borrow_mut().get_value(id) {
                Some(v) => Ok(Some(v)),
                None => Err((RuntimeError::ReferenceError(id.clone()), e.pos)),
            }
        }
        Expr::Tuple(ref elems) => {
            let mut values = Vec::new();
            for elem_expr in elems {
                let possible_val = interpret_expr(elem_expr, env.clone())?;
                let val = check_val_for_none_error(&possible_val, elem_expr)?;
                values.push(val);
            }
            Ok(Some(Value::Tuple(values)))
        }
        Expr::Unary(ref op, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, expr)?;
            match *op {
                UnOp::Minus => {
                    match operations::unary_minus(val) {
                        Ok(v) => Ok(Some(v)),
                        Err(err) => Err((err, e.pos)),
                    }
                }
            }
        }
        Expr::UnaryLogical(ref op, ref expr) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, expr)?;
            match *op {
                LogicalUnOp::Not => Ok(Some(Value::Bool(!val.is_truthy()))),
            }
        }
        Expr::Binary(ref expr1, ref op, ref expr2) => {
            let possible_val_1 = interpret_expr(expr1, env.clone())?;
            let val1 = check_val_for_none_error(&possible_val_1, expr1)?;
            let possible_val_2 = interpret_expr(expr2, env.clone())?;
            let val2 = check_val_for_none_error(&possible_val_2, expr2)?;
            let retval = match *op {
                BinOp::Add => operations::add(val1, val2),
                BinOp::Sub => operations::subtract(val1, val2),
                BinOp::Mul => operations::multiply(val1, val2),
                BinOp::Div => operations::divide(val1, val2),
                BinOp::Lt => operations::less_than(val1, val2),
                BinOp::Lte => operations::less_than_or_equal(val1, val2),
                BinOp::Gt => operations::greater_than(val1, val2),
                BinOp::Gte => operations::greater_than_or_equal(val1, val2),
                BinOp::StrictEquals => Ok(Value::Bool(val1 == val2)),
            };
            match retval {
                Ok(v) => Ok(Some(v)),
                Err(err) => Err((err, e.pos)),
            }
        }
        Expr::BinaryLogical(ref expr1, ref op, ref expr2) => {
            match *op {
                LogicalBinOp::LogicalAnd => {
                    let possible_val_1 = interpret_expr(expr1, env.clone())?;
                    let val1 = check_val_for_none_error(&possible_val_1, expr1)?;
                    if !val1.is_truthy() {
                        return Ok(Some(Value::Bool(false)));
                    }
                    let possible_val_2 = interpret_expr(expr2, env.clone())?;
                    let val2 = check_val_for_none_error(&possible_val_2, expr2)?;
                    Ok(Some(Value::Bool(val2.is_truthy())))
                }
                LogicalBinOp::LogicalOr => {
                    let possible_val_1 = interpret_expr(expr1, env.clone())?;
                    let val1 = check_val_for_none_error(&possible_val_1, expr1)?;
                    if val1.is_truthy() {
                        return Ok(Some(Value::Bool(true)));
                    }
                    let possible_val_2 = interpret_expr(expr2, env.clone())?;
                    let val2 = check_val_for_none_error(&possible_val_2, expr2)?;
                    Ok(Some(Value::Bool(val2.is_truthy())))
                }
            }
        }
        Expr::MemberByIdx(ref object_expr, ref index_expr) => {
            let possible_object = interpret_expr(object_expr, env.clone())?;
            let object = check_val_for_none_error(&possible_object, object_expr)?;
            let possible_index = interpret_expr(index_expr, env.clone())?;
            let index = check_val_for_none_error(&possible_index, index_expr)?;
            match object {
                Value::Tuple(ref v) => {
                    match index {
                        Value::Number(Number::Integer(i)) => {
                            if i < 0 {
                                return Err((RuntimeError::IndexOutOfBounds(i), e.pos));
                            }
                            match v.get(i as usize) {
                                Some(x) => Ok(Some(x.clone())),
                                None => Err((RuntimeError::IndexOutOfBounds(i), e.pos)),
                            }
                        }
                        non_int_index => {
                            Err((RuntimeError::NonIntegralSubscript(non_int_index.get_type()),
                                 index_expr.pos))
                        }
                    }
                }
                obj => {
                    Err((RuntimeError::SubscriptOnNonSubscriptable(obj.get_type()),
                         object_expr.pos))
                }
            }
        }
        Expr::FnDef(ref possible_id, ref param_list, ref body, ref ret_type_hint) => {
            let (param_names, param_types): (Vec<String>, Vec<Option<ConstraintType>>) =
                param_list.iter().cloned().unzip();
            let func = Function::User {
                ret_type: ret_type_hint.clone(),
                call_sign: CallSign {
                    num_params: param_list.len(),
                    variadic: false,
                    param_types: param_types,
                },
                param_names: param_names.clone(),
                body: body.clone(),
                env: env.clone(),
            };
            let func_val = Value::Function(Box::new(func));
            if let Some(ref id) = *possible_id {
                env.borrow_mut().declare(id, &func_val);
            }
            Ok(Some(func_val))
        }
        Expr::FnCall(ref expr, ref args) => {
            let possible_val = interpret_expr(expr, env.clone())?;
            let val = check_val_for_none_error(&possible_val, expr)?;
            let func = match val {
                Value::Function(f) => f,
                v => {
                    if let Expr::Identifier(ref id) = expr.data {
                        return Err((RuntimeError::CallToNonFunction(Some(id.clone()),
                                                                    v.get_type()),
                                    expr.pos));
                    }
                    return Err((RuntimeError::CallToNonFunction(None, v.get_type()), expr.pos));
                }
            };
            let mut arg_vals = Vec::new();
            for arg in args.iter() {
                let possible_val = interpret_expr(arg, env.clone())?;
                let val = check_val_for_none_error(&possible_val, arg)?;
                arg_vals.push(val);
            }

            let call_sign = func.get_call_sign();
            check_args_compat(&arg_vals, &call_sign, e)?;

            let call_func_result = call_func(&func, &arg_vals);
            match call_func_result {
                Ok(possible_val) => Ok(possible_val),
                Err(runtime_error) => Err((runtime_error, e.pos)),
            }
        }
    }
}

pub fn call_func(func: &Function, arg_vals: &[Value]) -> Result<Option<Value>, RuntimeError> {
    match *func {
        Function::NativeVoid(_, ref native_fn) => {
            native_fn(arg_vals.to_vec())?;
            Ok(None)
        }
        Function::NativeReturning(_, ref native_fn) => Ok(Some(native_fn(arg_vals.to_vec())?)),
        Function::User { ref param_names, ref body, ref env, .. } => {
            // TODO: returning
            let function_env = Environment::create_child(env.clone());
            for (param, arg) in param_names.iter().zip(arg_vals.iter()) {
                function_env.borrow_mut().declare(param, arg);
            }
            let inner_env = Environment::create_child(function_env);
            let result = interpret_statement(body, inner_env);
            match result {
                Err(error_with_position) => {
                    Err(RuntimeError::InsideFunctionCall(Box::new(error_with_position)))
                }
                Ok(statement_result) => {
                    if let StmtResult::Return(possible_val) = statement_result {
                        match possible_val {
                            Some(val) => Ok(Some(val)),
                            None => Ok(None),
                        }
                    } else {
                        Ok(None)
                    }
                }
            }
        }
    }
}

fn check_val_for_none_error(val: &Option<Value>,
                            expr: &ExprNode)
                            -> Result<Value, RuntimeErrorWithPosition> {
    if val.is_none() {
        if let Expr::FnCall(ref f_expr, _) = expr.data {
            if let Expr::Identifier(ref id) = f_expr.data {
                return Err((RuntimeError::NoneError(Some(id.clone())), expr.pos));
            }
            return Err((RuntimeError::NoneError(None), expr.pos));
        }
    }
    Ok(val.clone().unwrap())
}

fn check_args_compat(arg_vals: &[Value],
                     call_sign: &CallSign,
                     expr: &ExprNode)
                     -> Result<(), RuntimeErrorWithPosition> {
    if !call_sign.variadic && call_sign.num_params != arg_vals.len() {
        if let Expr::Identifier(ref id) = expr.data {
            return Err((RuntimeError::ArgumentLength(Some(id.clone())), expr.pos));
        }
        return Err((RuntimeError::ArgumentLength(None), expr.pos));
    }
    Ok(())
}

impl Interpreter for AstWalkInterpreter {
    fn run_ast_as_statements(&mut self,
                             statements: &[StmtNode])
                             -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_statements(statements, self.root_env.clone())
    }

    fn run_ast_as_program(&mut self,
                          program: &[StmtNode])
                          -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        interpret_program(program, self.root_env.clone())
    }
}
