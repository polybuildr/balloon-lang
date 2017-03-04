use ast::*;
use value::*;
use operations;
use environment::Environment;
use checker::Type;

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
}

pub fn interpret_program(program: &Vec<Statement>) -> Result<(), InterpreterError> {
    let mut env = Environment::new();
    env.start_scope();
    interpret_statements(program, &mut env)?;
    env.end_scope();
    Ok(())
}

pub fn interpret_statements(statements: &Vec<Statement>,
                            env: &mut Environment)
                            -> Result<(), InterpreterError> {
    for statement in statements.iter() {
        interpret_statement(statement, env)?;
    }
    Ok(())
}

pub fn interpret_statement(s: &Statement, env: &mut Environment) -> Result<(), InterpreterError> {
    match *s {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let val = interpret_expr(expr, env)?;
            env.declare(variable, &val);
            println!("{:?} => {}", variable, val);
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let val = interpret_expr(expr, env)?;
            match *lhs_expr {
                LhsExpr::Identifier(ref id) => env.set(id, val)?,
            };
        }
        Statement::Block(ref statements) => {
            env.start_scope();
            for statement in statements.iter() {
                interpret_statement(statement, env)?;
            }
            env.end_scope();
        }
        Statement::Expression(ref expr) => {
            let val = interpret_expr(expr, env)?;
            println!("Expression => {}", val);
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            let val = interpret_expr(if_expr, env)?;
            if val.is_truthy() {
                interpret_statement(then_block, env)?;
            }
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let val = interpret_expr(if_expr, env)?;
            if val.is_truthy() {
                interpret_statement(then_block, env)?;
            } else {
                interpret_statement(else_block, env)?;
            }
        }
        Statement::Empty => {}
    };
    Ok(())
}

fn interpret_expr(e: &Expr, env: &mut Environment) -> Result<Value, InterpreterError> {
    match *e {
        Expr::Literal(ref x) => Ok(Value::from(x.clone())),
        Expr::Identifier(ref id) => env.get_value(&id),
        Expr::UnaryExpression(ref op, ref expr) => {
            let val = interpret_expr(expr, env)?;
            match *op {
                UnaryOp::Minus => operations::unary_minus(val),
            }
        }
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            let val = interpret_expr(expr, env)?;
            match *op {
                LogicalUnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
            }
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let val1 = interpret_expr(expr1, env)?;
            let val2 = interpret_expr(expr2, env)?;
            match *op {
                BinaryOp::Add => Ok(operations::add(val1, val2)?),
                BinaryOp::Sub => Ok(operations::subtract(val1, val2)?),
                BinaryOp::Mul => Ok(operations::multiply(val1, val2)?),
                BinaryOp::Div => Ok(operations::divide(val1, val2)?),
                BinaryOp::FloorDiv => Ok(operations::floor_divide(val1, val2)?),
                BinaryOp::LessThan => Ok(operations::less_than(val1, val2)?),
                BinaryOp::LessThanOrEqual => Ok(operations::less_than_or_equal(val1, val2)?),
                BinaryOp::GreaterThan => Ok(operations::greater_than(val1, val2)?),
                BinaryOp::GreaterThanOrEqual => Ok(operations::greater_than_or_equal(val1, val2)?),
                BinaryOp::StrictEquals => Ok(operations::strict_equals(val1, val2)?),
            }
        }
        Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
            match *op {
                LogicalBinaryOp::LogicalAnd => {
                    let val1 = interpret_expr(expr1, env)?;
                    if !val1.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                    let val2 = interpret_expr(expr2, env)?;
                    Ok(Value::Bool(val2.is_truthy()))
                }
                LogicalBinaryOp::LogicalOr => {
                    let val1 = interpret_expr(expr1, env)?;
                    if val1.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                    let val2 = interpret_expr(expr2, env)?;
                    Ok(Value::Bool(val2.is_truthy()))
                }
            }
        }
    }
}
