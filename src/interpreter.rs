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
    NoneError(String),
}

pub type InterpreterErrorWithPosition = (InterpreterError, OffsetSpan);

pub enum StatementResult {
    None,
    Break,
    Value(Value),
}

#[cfg(test)]
impl StatementResult {
    pub fn unwrap_value(&self) -> Value {
        if let StatementResult::Value(v) = *self {
            v
        } else {
            panic!("Cannot unwrap value");
        }
    }
}

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { env: Environment::new() }
    }

    pub fn run_ast_as_program(&mut self,
                              program: &Vec<StatementNode>)
                              -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
        interpret_program(program, &mut self.env)
    }

    pub fn setup_for_repl(&mut self) {
        self.env.start_scope();
    }

    pub fn cleanup_for_repl(&mut self) {
        self.env.end_scope();
    }

    pub fn run_ast_as_statements
        (&mut self,
         statements: &Vec<StatementNode>)
         -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
        interpret_statements(statements, &mut self.env)
    }
}

fn interpret_program(program: &Vec<StatementNode>,
                     env: &mut Environment)
                     -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
    env.start_scope();
    let result = interpret_statements(program, env)?;
    env.end_scope();
    Ok(result)
}

fn interpret_statements(statements: &Vec<StatementNode>,
                        env: &mut Environment)
                        -> Result<Option<StatementResult>, InterpreterErrorWithPosition> {
    let mut last_result = None;
    for statement in statements.iter() {
        last_result = Some(interpret_statement(statement, env)?);
    }
    Ok(last_result)
}

fn interpret_statement(s: &StatementNode,
                       env: &mut Environment)
                       -> Result<StatementResult, InterpreterErrorWithPosition> {
    match s.data {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let val = interpret_expr(expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                }
            }
            env.declare(variable, &val.unwrap());
            Ok(StatementResult::None)
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let val = interpret_expr(expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                }
            }
            match lhs_expr.data {
                LhsExpr::Identifier(ref id) => {
                    if !env.set(id, val.unwrap()) {
                        return Err((InterpreterError::UndeclaredAssignment(id.clone()),
                                    lhs_expr.pos));
                    }
                }
            };
            Ok(StatementResult::None)
        }
        Statement::Block(ref statements) => {
            env.start_scope();
            for statement in statements.iter() {
                if let StatementResult::Break = interpret_statement(statement, env)? {
                    env.end_scope();
                    return Ok(StatementResult::Break);
                }
            }
            env.end_scope();
            return Ok(StatementResult::None);
        }
        Statement::Expression(ref expr) => {
            let val = interpret_expr(expr, env)?;
            match val {
                None => Ok(StatementResult::None),
                Some(x) => Ok(StatementResult::Value(x)),
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            let val = interpret_expr(if_expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = if_expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), if_expr.pos));
                }
            }
            if val.unwrap().is_truthy() {
                if let StatementResult::Break = interpret_statement(then_block, env)? {
                    return Ok(StatementResult::Break);
                }
            }
            return Ok(StatementResult::None);
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let val = interpret_expr(if_expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = if_expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), if_expr.pos));
                }
            }
            if val.unwrap().is_truthy() {
                if let StatementResult::Break = interpret_statement(then_block, env)? {
                    return Ok(StatementResult::Break);
                }
            } else {
                if let StatementResult::Break = interpret_statement(else_block, env)? {
                    return Ok(StatementResult::Break);
                }
            }
            Ok(StatementResult::None)
        }
        Statement::Loop(ref block) => {
            env.start_scope();
            loop {
                if let StatementResult::Break = interpret_statement(block, env)? {
                    break;
                }
            }
            env.end_scope();
            Ok(StatementResult::None)
        }
        Statement::Break => Ok(StatementResult::Break),
        Statement::Empty => Ok(StatementResult::None),
    }
}
fn interpret_expr(e: &ExprNode,
                  env: &mut Environment)
                  -> Result<Option<Value>, InterpreterErrorWithPosition> {
    match e.data {
        Expr::Literal(ref x) => Ok(Some(Value::from(x.clone()))),
        Expr::Identifier(ref id) => {
            match env.get_value(&id) {
                Some(v) => Ok(Some(v)),
                None => Err((InterpreterError::ReferenceError(id.clone()), e.pos)),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            let val = interpret_expr(expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                }
            }
            match *op {
                UnaryOp::Minus => {
                    match operations::unary_minus(val.unwrap()) {
                        Ok(v) => Ok(Some(v)),
                        Err(err) => Err((err, e.pos)),
                    }
                }
            }
        }
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            let val = interpret_expr(expr, env)?;
            if let None = val {
                if let Expr::FunctionCall(ref id, _) = expr.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                }
            }
            match *op {
                LogicalUnaryOp::Not => Ok(Some(Value::Bool(!val.unwrap().is_truthy()))),
            }
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let possible_val_1 = interpret_expr(expr1, env)?;
            if let None = possible_val_1 {
                if let Expr::FunctionCall(ref id, _) = expr1.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                }
            }
            let possible_val_2 = interpret_expr(expr2, env)?;
            if let None = possible_val_2 {
                if let Expr::FunctionCall(ref id, _) = expr2.data {
                    return Err((InterpreterError::NoneError(id.clone()), expr2.pos));
                }
            }
            let val1 = possible_val_1.unwrap();
            let val2 = possible_val_2.unwrap();
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
                    let val1 = interpret_expr(expr1, env)?;
                    if let None = val1 {
                        if let Expr::FunctionCall(ref id, _) = expr1.data {
                            return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                        }
                    }
                    if !val1.unwrap().is_truthy() {
                        return Ok(Some(Value::Bool(false)));
                    }
                    let val2 = interpret_expr(expr2, env)?;
                    if let None = val2 {
                        if let Expr::FunctionCall(ref id, _) = expr2.data {
                            return Err((InterpreterError::NoneError(id.clone()), expr2.pos));
                        }
                    }
                    Ok(Some(Value::Bool(val2.unwrap().is_truthy())))
                }
                LogicalBinaryOp::LogicalOr => {
                    let val1 = interpret_expr(expr1, env)?;
                    if let None = val1 {
                        if let Expr::FunctionCall(ref id, _) = expr1.data {
                            return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                        }
                    }
                    if val1.unwrap().is_truthy() {
                        return Ok(Some(Value::Bool(true)));
                    }
                    let val2 = interpret_expr(expr2, env)?;
                    if let None = val2 {
                        if let Expr::FunctionCall(ref id, _) = expr2.data {
                            return Err((InterpreterError::NoneError(id.clone()), expr2.pos));
                        }
                    }
                    Ok(Some(Value::Bool(val2.unwrap().is_truthy())))
                }
            }
        }
        Expr::FunctionCall(ref id, ref args) => {
            use builtins;
            use builtins::Function;
            // check for builtins
            let possible_wrapped_func = builtins::get_builtin_from_name(id.as_ref());
            if let None = possible_wrapped_func {
                return Err((InterpreterError::ReferenceError(id.clone()), e.pos));
            }
            let wrapped_func = possible_wrapped_func.unwrap();

            let mut arg_vals = Vec::new();
            for arg in args.iter() {
                let val = interpret_expr(arg, env)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = arg.data {
                        return Err((InterpreterError::NoneError(id.clone()), arg.pos));
                    }
                }
                arg_vals.push(val.unwrap());
            }
            match wrapped_func {
                Function::Void(f) => {
                    f(arg_vals);
                    Ok(None)
                }
                Function::Returning(f) => Ok(Some(f(arg_vals))),
            }
        }
    }
}
