use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

use ast::*;
use ast;
use interpreter::InterpreterError;

#[derive(Copy, Clone, Debug, PartialEq)]
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

#[derive(Debug)]
pub enum TypeCheckerIssue {
    InterpreterError(InterpreterError),
    MultipleTypesFromBranchWarning(String),
}

pub type TypeCheckerIssueWithPosition = (TypeCheckerIssue, OffsetSpan);

impl From<InterpreterError> for TypeCheckerIssue {
    fn from(from: InterpreterError) -> Self {
        TypeCheckerIssue::InterpreterError(from)
    }
}

#[derive(Clone)]
pub struct TypeEnvironment {
    pub symbol_tables: Vec<HashMap<String, Type>>,
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

    pub fn set(&mut self, identifier: &String, typ: Type) -> bool {
        for table in self.symbol_tables.iter_mut().rev() {
            // TODO: Entry API
            if table.contains_key(identifier) {
                table.insert(identifier.clone(), typ);
                return true;
            }
        }
        false
    }

    pub fn get_type(&mut self, identifier: &String) -> Option<Type> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(typ) = table.get(identifier) {
                return Some(*typ);
            }
        }
        None
    }

    pub fn get_all_keys(&self) -> HashSet<String> {
        let mut keys = HashSet::new();
        for table in self.symbol_tables.iter() {
            for key in table.keys() {
                keys.insert(key.clone());
            }
        }
        keys
    }
}

pub fn check_program(ast: &Vec<StatementNode>) -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut env = TypeEnvironment::new();
    env.start_scope();
    let result = check_statements(ast, &mut env);
    env.end_scope();
    result
}

pub fn check_statements(ast: &Vec<StatementNode>,
                        env: &mut TypeEnvironment)
                        -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    for statement in ast.iter() {
        if let Err(mut e) = check_statement(statement, env) {
            issues.append(&mut e);
        }
    }
    if issues.len() == 0 {
        Ok(())
    } else {
        Err(issues)
    }
}

pub fn check_statement(s: &StatementNode,
                       env: &mut TypeEnvironment)
                       -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    match s.data {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let checked_type = match check_expr(expr, env) {
                Ok(t) => t,
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            env.declare(variable, &checked_type);
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let checked_type = match check_expr(expr, env) {
                Ok(t) => t,
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            match lhs_expr.data {
                LhsExpr::Identifier(ref id) => {
                    if !env.set(id, checked_type) {
                        issues.push((InterpreterError::UndeclaredAssignment(id.clone()).into(),
                                     lhs_expr.pos));
                    }
                }
            };
        }
        Statement::Block(ref statements) => {
            env.start_scope();
            if let Err(mut e) = check_statements(statements, env) {
                issues.append(&mut e);
            }
            env.end_scope();
        }
        Statement::Expression(ref expr) => {
            if let Err(mut e) = check_expr(expr, env) {
                issues.append(&mut e);
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            if let Err(mut e) = check_expr(if_expr, env) {
                issues.append(&mut e);
            }
            if let Err(mut e) = check_statement(then_block, env) {
                issues.append(&mut e);
            }
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let mut then_env = env.clone();
            let mut else_env = env.clone();
            if let Err(mut e) = check_expr(if_expr, env) {
                issues.append(&mut e);
            }
            if let Err(mut e) = check_statement(then_block, &mut then_env) {
                issues.append(&mut e);
            }
            if let Err(mut e) = check_statement(else_block, &mut else_env) {
                issues.append(&mut e);
            }

            for name in then_env.get_all_keys() {
                let then_type = then_env.get_type(&name).unwrap();
                if else_env.get_type(&name).unwrap() != then_type {
                    issues.push((TypeCheckerIssue::MultipleTypesFromBranchWarning(name.clone()),
                                 s.pos));
                    env.set(&name, Type::Any);
                } else {
                    env.set(&name, then_type);
                }
            }
        }
        Statement::Loop(ref block) => {
            if let Err(mut e) = check_statement(block, env) {
                issues.append(&mut e);
            }
        }
        Statement::Break => {}
        Statement::Empty => {}
    };
    if issues.len() == 0 {
        Ok(())
    } else {
        Err(issues)
    }
}

fn check_expr(expr: &ExprNode,
              env: &mut TypeEnvironment)
              -> Result<Type, Vec<TypeCheckerIssueWithPosition>> {
    match expr.data {
        Expr::Literal(ref x) => Ok(Type::from(x.clone())),
        Expr::Identifier(ref id) => {
            match env.get_type(&id) {
                Some(t) => Ok(t),
                None => Err(vec![(InterpreterError::ReferenceError(id.clone()).into(), expr.pos)]),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            match check_expr(expr, env) {
                Ok(t) => {
                    match *op {
                        UnaryOp::Minus => {
                            match check_unary_minus_for_type(t) {
                                Ok(t) => Ok(t),
                                Err(e) => Err(vec![(e, expr.pos)]),
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
            let mut issues = Vec::new();
            let checked_type_1 = match check_expr(expr1, env) {
                Ok(t) => t,
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            let checked_type_2 = match check_expr(expr2, env) {
                Ok(t) => t,
                Err(mut e) => {
                    issues.append(&mut e);
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
                    issues.push((e, expr.pos));
                    Err(issues)
                }
                Ok(t) => {
                    if issues.len() == 0 {
                        Ok(t)
                    } else {
                        Err(issues)
                    }
                }
            }
        }
        Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
            let mut issues = Vec::new();
            match *op {
                LogicalBinaryOp::LogicalAnd |
                LogicalBinaryOp::LogicalOr => {
                    if let Err(mut e) = check_expr(expr1, env) {
                        issues.append(&mut e);
                    };
                    if let Err(mut e) = check_expr(expr2, env) {
                        issues.append(&mut e);
                    };
                    if issues.len() == 0 {
                        Ok(Type::Bool)
                    } else {
                        Err(issues)
                    }
                }
            }
        }
        Expr::FunctionCall(ref id, ref args) => {
            match id.as_ref() {
                "println" => {}
                _ => {
                    return Err(vec![(InterpreterError::ReferenceError(id.clone()).into(),
                                     expr.pos)]);
                }
            };
            let mut issues = Vec::new();
            for arg in args.iter() {
                if let Err(mut e) = check_expr(arg, env) {
                    issues.append(&mut e);
                }
            }
            if issues.len() == 0 {
                Ok(Type::Any)
            } else {
                Err(issues)
            }
        }
    }
}

fn check_unary_minus_for_type(typ: Type) -> Result<Type, TypeCheckerIssue> {
    match typ {
        Type::Number => Ok(Type::Number),
        Type::Any => Ok(Type::Any),
        _ => Err(InterpreterError::UnaryTypeError(UnaryOp::Minus, typ).into()),
    }
}

fn check_binary_arithmetic_for_types(op: BinaryOp,
                                     t1: Type,
                                     t2: Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Any, _) => Ok(Type::Any),
        (_, Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1, t2).into()),
    }
}

fn check_binary_comparison_for_types(op: BinaryOp,
                                     t1: Type,
                                     t2: Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (Type::Number, Type::Number) => Ok(Type::Bool),
        (Type::Any, _) => Ok(Type::Any),
        (_, Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1, t2).into()),
    }
}
