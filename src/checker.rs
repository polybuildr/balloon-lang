use std::collections::HashMap;
use std::fmt;

use ast::*;
use ast;
use interpreter::InterpreterError;

#[derive(Copy, Clone, Debug)]
pub enum Type {
    Number,
    Bool,
    Any,
}

impl From<ast::Literal> for Type {
    fn from(from: ast::Literal) -> Self {
        match from {
            ast::Literal::Integer(_) => Type::Number,
            ast::Literal::Float(_) => Type::Number,
            ast::Literal::Bool(_) => Type::Bool,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "Number"),
            Type::Bool => write!(f, "Bool"),
            Type::Any => write!(f, "Any"),
        }
    }
}

pub struct TypeEnvironment {
    symbol_tables: Vec<HashMap<String, Type>>,
}

impl TypeEnvironment {
    pub fn new() -> TypeEnvironment {
        TypeEnvironment { symbol_tables: Vec::new() }
    }

    pub fn start_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.symbol_tables.pop();
    }

    pub fn declare(&mut self, variable: &Variable, typ: &Type) {
        match *variable {
            Variable::Identifier(_, ref id) => {
                self.symbol_tables.last_mut().unwrap().insert(id.clone(), *typ);
            }
        };
    }

    pub fn set(&mut self, identifier: &String, typ: Type) -> Result<(), InterpreterError> {
        for table in self.symbol_tables.iter_mut().rev() {
            // TODO: Entry API
            if table.contains_key(identifier) {
                table.insert(identifier.clone(), typ);
                return Ok(());
            }
        }
        Err(InterpreterError::UndeclaredAssignment(identifier.clone()))
    }

    pub fn get_type(&mut self, identifier: &String) -> Result<Type, InterpreterError> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(typ) = table.get(identifier) {
                return Ok(*typ);
            }
        }
        Err(InterpreterError::ReferenceError(identifier.clone()))
    }
}

pub fn check_program(ast: &Vec<Statement>) -> Result<(), Vec<InterpreterError>> {
    let mut env = TypeEnvironment::new();
    env.start_scope();
    let result = check_statements(ast, &mut env);
    env.end_scope();
    result
}

pub fn check_statements(ast: &Vec<Statement>,
                        env: &mut TypeEnvironment)
                        -> Result<(), Vec<InterpreterError>> {
    let mut errors = Vec::new();
    for statement in ast.iter() {
        if let Err(mut e) = check_statement(statement, env) {
            errors.append(&mut e);
        }
    }
    if errors.len() == 0 {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn check_statement(s: &Statement,
                       env: &mut TypeEnvironment)
                       -> Result<(), Vec<InterpreterError>> {
    let mut errors = Vec::new();
    match *s {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let checked_type = match check_expr(expr, env) {
                Ok(t) => t,
                Err(mut e) => {
                    errors.append(&mut e);
                    Type::Any
                }
            };
            env.declare(variable, &checked_type);
            // println!("{:?} => {}", variable, checked_type);
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let checked_type = match check_expr(expr, env) {
                Ok(t) => t,
                Err(mut e) => {
                    errors.append(&mut e);
                    Type::Any
                }
            };
            match *lhs_expr {
                LhsExpr::Identifier(ref id) => {
                    if let Err(e) = env.set(id, checked_type) {
                        errors.push(e);
                    }
                }
            };
        }
        Statement::Block(ref statements) => {
            env.start_scope();
            if let Err(mut e) = check_statements(statements, env) {
                errors.append(&mut e);
            }
            env.end_scope();
        }
        Statement::Expression(ref expr) => {
            if let Err(mut e) = check_expr(expr, env) {
                errors.append(&mut e);
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            if let Err(mut e) = check_expr(if_expr, env) {
                errors.append(&mut e);
            }
            if let Err(mut e) = check_statement(then_block, env) {
                errors.append(&mut e);
            }
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            if let Err(mut e) = check_expr(if_expr, env) {
                errors.append(&mut e);
            }
            if let Err(mut e) = check_statement(then_block, env) {
                errors.append(&mut e);
            }
            if let Err(mut e) = check_statement(else_block, env) {
                errors.append(&mut e);
            }
        }
        Statement::Empty => {}
    };
    if errors.len() == 0 {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_expr(expr: &Expr, env: &mut TypeEnvironment) -> Result<Type, Vec<InterpreterError>> {
    match *expr {
        Expr::Literal(ref x) => Ok(Type::from(x.clone())),
        Expr::Identifier(ref id) => {
            match env.get_type(&id) {
                Ok(t) => Ok(t),
                Err(e) => Err(vec![e]),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            match check_expr(expr, env) {
                Ok(t) => {
                    match *op {
                        UnaryOp::Minus => {
                            match check_unary_minus_for_type(t) {
                                Ok(t) => Ok(t),
                                Err(e) => Err(vec![e]),
                            }
                        }
                    }
                }
                Err(e) => Err(e),
            }
        }
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            match check_expr(expr, env) {
                Ok(_) => {
                    match *op {
                        LogicalUnaryOp::Not => Ok(Type::Bool),
                    }
                }
                Err(e) => Err(e),
            }
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let mut errors = Vec::new();
            let checked_type_1 = match check_expr(expr1, env) {
                Ok(t) => t,
                Err(mut e) => {
                    errors.append(&mut e);
                    Type::Any
                }
            };
            let checked_type_2 = match check_expr(expr2, env) {
                Ok(t) => t,
                Err(mut e) => {
                    errors.append(&mut e);
                    Type::Any
                }
            };
            use ast::BinaryOp::*;
            let result = match *op {
                ref op @ Add |
                ref op @ Sub |
                ref op @ Mul |
                ref op @ Div |
                ref op @ FloorDiv => {
                    check_binary_arithmetic_for_types(op.clone(), checked_type_1, checked_type_2)
                }
                ref op @ LessThan |
                ref op @ LessThanOrEqual |
                ref op @ GreaterThan |
                ref op @ GreaterThanOrEqual => {
                    check_binary_comparison_for_types(op.clone(), checked_type_1, checked_type_2)
                }
                StrictEquals => Ok(Type::Bool),
            };
            match result {
                Err(e) => {
                    errors.push(e);
                    Err(errors)
                }
                Ok(t) => {
                    if errors.len() == 0 {
                        Ok(t)
                    } else {
                        Err(errors)
                    }
                }
            }
        }
        Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
            let mut errors = Vec::new();
            match *op {
                LogicalBinaryOp::LogicalAnd |
                LogicalBinaryOp::LogicalOr => {
                    if let Err(mut e) = check_expr(expr1, env) {
                        errors.append(&mut e);
                    };
                    if let Err(mut e) = check_expr(expr2, env) {
                        errors.append(&mut e);
                    };
                    if errors.len() == 0 {
                        Ok(Type::Bool)
                    } else {
                        Err(errors)
                    }
                }
            }
        }
    }
}

fn check_unary_minus_for_type(typ: Type) -> Result<Type, InterpreterError> {
    match typ {
        Type::Number => Ok(Type::Number),
        Type::Any => Ok(Type::Any),
        _ => Err(InterpreterError::UnaryTypeError(UnaryOp::Minus, typ)),
    }
}

fn check_binary_arithmetic_for_types(op: BinaryOp,
                                     t1: Type,
                                     t2: Type)
                                     -> Result<Type, InterpreterError> {
    match (t1, t2) {
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Any, _) => Ok(Type::Any),
        (_, Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1, t2)),
    }
}

fn check_binary_comparison_for_types(op: BinaryOp,
                                     t1: Type,
                                     t2: Type)
                                     -> Result<Type, InterpreterError> {
    match (t1, t2) {
        (Type::Number, Type::Number) => Ok(Type::Bool),
        (Type::Any, _) => Ok(Type::Any),
        (_, Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1, t2)),
    }
}
