use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use ast::*;
use ast;
use interpreter::InterpreterError;
use function::*;

#[derive(Clone, Debug)]
pub enum Type {
    Number,
    Bool,
    Any,
    Function(Function),
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Number, &Type::Number) => true,
            (&Type::Bool, &Type::Bool) => true,
            (&Type::Function(_), &Type::Function(_)) => true,
            _ => false,
        }
    }
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
            Type::Function(_) => write!(f, "Function"),
        }
    }
}

#[derive(Debug, PartialEq)]
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
    pub symbol_table: HashMap<String, Type>,
    parent: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl fmt::Debug for TypeEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.symbol_table)
    }
}

impl TypeEnvironment {
    pub fn new_root() -> Rc<RefCell<TypeEnvironment>> {
        let mut env = TypeEnvironment::new();
        let builtin_functions = &[("println",
                                   Function::NativeVoid(CallSign {
                                                            num_params: 0,
                                                            variadic: true,
                                                        },
                                                        native_println))];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(&name.to_string(), &Type::Function(func.clone()));
        }
        Rc::new(RefCell::new(env))
    }

    pub fn new() -> TypeEnvironment {
        TypeEnvironment { symbol_table: HashMap::new(), parent: None }
    }

    pub fn create_clone(env: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        let cloned_env = env.borrow().clone();
        Rc::new(RefCell::new(cloned_env))
    }

    pub fn create_child(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        let env = TypeEnvironment { parent: Some(parent), symbol_table: HashMap::default() };
        Rc::new(RefCell::new(env))
    }

    pub fn declare(&mut self, id: &String, typ: &Type) {
        self.symbol_table.insert(id.clone(), typ.clone());
    }

    pub fn set(&mut self, identifier: &String, typ: Type) -> bool {
        if self.symbol_table.contains_key(identifier) {
            self.symbol_table.insert(identifier.clone(), typ);
            return true;
        } else {
            match self.parent {
                Some(ref parent) => return parent.borrow_mut().set(identifier, typ),
                None => return false,
            };
        }
    }

    pub fn get_type(&self, identifier: &String) -> Option<Type> {
        if let Some(typ) = self.symbol_table.get(identifier) {
            return Some(typ.clone());
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow().get_type(identifier),
                None => None,
            }
        }
    }

    pub fn get_all_keys(&self) -> HashSet<String> {
        let mut keys = HashSet::new();
        for key in self.symbol_table.keys() {
            keys.insert(key.clone());
        }
        if let Some(ref parent) = self.parent {
            for key in parent.borrow().get_all_keys() {
                keys.insert(key.clone());
            }
        }
        keys
    }
}

pub fn check_program(ast: &Vec<StatementNode>) -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let root_env = TypeEnvironment::new_root();
    let result = check_statements(ast, root_env.clone());
    result
}

pub fn check_statements(ast: &Vec<StatementNode>,
                        env: Rc<RefCell<TypeEnvironment>>)
                        -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    for statement in ast.iter() {
        if let Err(mut e) = check_statement(statement, env.clone()) {
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
                       env: Rc<RefCell<TypeEnvironment>>)
                       -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    match s.data {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let checked_type = match check_expr(expr, env.clone()) {
                Ok(possible_type) => {
                    match possible_type {
                        None => {
                            if let Expr::FunctionCall(ref id, _) = expr.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr.pos));
                            }
                            Type::Any
                        }
                        Some(t) => t,
                    }
                }
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            match variable {
                &Variable::Identifier(_, ref id) => {
                    env.borrow_mut().declare(id, &checked_type);
                }
            };
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            println!("called assignment!");
            let checked_type = match check_expr(expr, env.clone()) {
                Ok(possible_type) => {
                    match possible_type {
                        None => {
                            if let Expr::FunctionCall(ref id, _) = expr.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr.pos));
                            }
                            Type::Any
                        }
                        Some(t) => t,
                    }
                }
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            println!("checked_type: {:?}", checked_type);
            match lhs_expr.data {
                LhsExpr::Identifier(ref id) => {
                    if !env.borrow_mut().set(id, checked_type) {
                        println!("env after assign: {:?}", env.borrow());
                        issues.push((InterpreterError::UndeclaredAssignment(id.clone()).into(),
                                     lhs_expr.pos));
                    }
                }
            };
        }
        Statement::Block(ref statements) => {
            let child_env = TypeEnvironment::create_child(env);
            if let Err(mut e) = check_statements(statements, child_env) {
                issues.append(&mut e);
            }
        }
        Statement::Expression(ref expr) => {
            if let Err(mut e) = check_expr(expr, env.clone()) {
                issues.append(&mut e);
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            let if_expr_result = check_expr(if_expr, env.clone());
            if let Err(mut e) = if_expr_result {
                issues.append(&mut e);
            } else if let Ok(None) = if_expr_result {
                if let Expr::FunctionCall(ref id, _) = if_expr.data {
                    return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                     if_expr.pos)]);
                }
            }
            if let Err(mut e) = check_statement(then_block, env.clone()) {
                issues.append(&mut e);
            }
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let then_env = TypeEnvironment::create_clone(env.clone());
            let else_env = TypeEnvironment::create_clone(env.clone());
            let if_expr_result = check_expr(if_expr, env.clone());
            if let Err(mut e) = if_expr_result {
                issues.append(&mut e);
            } else if let Ok(None) = if_expr_result {
                if let Expr::FunctionCall(ref id, _) = if_expr.data {
                    return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                     if_expr.pos)]);
                }
            }
            
            if let Err(mut e) = check_statement(then_block, then_env.clone()) {
                issues.append(&mut e);
            }
            
            if let Err(mut e) = check_statement(else_block, else_env.clone()) {
                issues.append(&mut e);
            }

            let names = then_env.borrow().get_all_keys();
            for name in names {
                let then_type = then_env.borrow().get_type(&name).unwrap();
                let else_type = else_env.borrow().get_type(&name).unwrap();
                println!("{}: {:?} and {:?}", name, then_type, else_type);
                if  else_type != then_type {
                    issues.push((TypeCheckerIssue::MultipleTypesFromBranchWarning(name.clone()),
                                    s.pos));
                    env.borrow_mut().set(&name, Type::Any);
                } else {
                    env.borrow_mut().set(&name, then_type);
                }
            }
        }
        Statement::Loop(ref block) => {
            if let Err(mut e) = check_statement(block, env.clone()) {
                issues.append(&mut e);
            }
        }
        Statement::Break => {}
        Statement::Empty => {}
        Statement::Return(_) => unimplemented!(),
    };
    if issues.len() == 0 {
        Ok(())
    } else {
        Err(issues)
    }
}

fn check_expr(expr: &ExprNode,
              env: Rc<RefCell<TypeEnvironment>>)
              -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    match expr.data {
        Expr::Literal(ref x) => Ok(Some(Type::from(x.clone()))),
        Expr::Identifier(ref id) => {
            match env.borrow().get_type(&id) {
                Some(t) => Ok(Some(t)),
                None => Err(vec![(InterpreterError::ReferenceError(id.clone()).into(), expr.pos)]),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            match check_expr(expr, env.clone()) {
                Ok(possible_type) => {
                    if let None = possible_type {
                        if let Expr::FunctionCall(ref id, _) = expr.data {
                            return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr.pos)]);
                        }
                    }
                    match *op {
                        UnaryOp::Minus => {
                            match check_unary_minus_for_type(possible_type.unwrap()) {
                                Ok(t) => Ok(Some(t)),
                                Err(e) => Err(vec![(e, expr.pos)]),
                            }
                        }
                    }
                }
                Err(e) => Err(e),
            }
        }
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            match check_expr(expr, env.clone()) {
                Ok(possible_type) => {
                    if let None = possible_type {
                        if let Expr::FunctionCall(ref id, _) = expr.data {
                            return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr.pos)]);
                        }
                    }
                    match *op {
                        LogicalUnaryOp::Not => Ok(Some(Type::Bool)),
                    }
                }
                Err(e) => Err(e),
            }
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let mut issues = Vec::new();
            let checked_type_1 = match check_expr(expr1, env.clone()) {
                Ok(possible_type) => {
                    match possible_type {
                        None => {
                            if let Expr::FunctionCall(ref id, _) = expr1.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr1.pos));
                            }
                            Type::Any
                        }
                        Some(t) => t,
                    }
                }
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            let checked_type_2 = match check_expr(expr2, env.clone()) {
                Ok(possible_type) => {
                    match possible_type {
                        None => {
                            if let Expr::FunctionCall(ref id, _) = expr2.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr2.pos));
                            }
                            Type::Any
                        }
                        Some(t) => t,
                    }
                }
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
                    check_binary_arithmetic_for_types(op.clone(), &checked_type_1, &checked_type_2)
                }
                ref op @ LessThan |
                ref op @ LessThanOrEqual |
                ref op @ GreaterThan |
                ref op @ GreaterThanOrEqual => {
                    check_binary_comparison_for_types(op.clone(), &checked_type_1, &checked_type_2)
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
                        Ok(Some(t))
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
                    let result1 = check_expr(expr1, env.clone());
                    if let Err(mut e) = result1 {
                        issues.append(&mut e);
                    } else if let Ok(None) = result1 {
                        if let Expr::FunctionCall(ref id, _) = expr1.data {
                            issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                       expr1.pos));
                        }
                    };
                    let result2 = check_expr(expr2, env.clone());
                    if let Err(mut e) = result2 {
                        issues.append(&mut e);
                    } else if let Ok(None) = result2 {
                        if let Expr::FunctionCall(ref id, _) = expr2.data {
                            issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                       expr2.pos));
                        }
                    }
                    if issues.len() == 0 {
                        Ok(Some(Type::Bool))
                    } else {
                        Err(issues)
                    }
                }
            }
        }
        Expr::FunctionCall(_, _) => {
            unimplemented!();
        }
        Expr::FunctionDefinition(_, _, _) => unimplemented!(),
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
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::Any, _) => Ok(Type::Any),
        (_, &Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_comparison_for_types(op: BinaryOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Bool),
        (&Type::Any, _) => Ok(Type::Any),
        (_, &Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
    }
}

fn try_get_name_of_fn(expr: &Box<ExprNode>) -> Option<String> {
    if let Expr::Identifier(ref id) = expr.data {
        Some(id.to_string())
    } else {
        None
    }
}
