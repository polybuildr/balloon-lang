use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use ast::*;
use ast;
use interpreter::InterpreterError;
use function::*;

#[derive(Clone, Debug)]
pub enum FunctionType {
    NativeVoid(CallSign),
    NativeReturning(CallSign),
    User {
        returning: bool,
        call_sign: CallSign,
        param_list: Vec<String>,
        body: Box<ast::StatementNode>,
        env: Rc<RefCell<TypeEnvironment>>,
    },
}

impl FunctionType {
    pub fn get_call_sign(&self) -> CallSign {
        match *self {
            FunctionType::NativeVoid(ref call_sign) |
            FunctionType::NativeReturning(ref call_sign) |
            FunctionType::User { ref call_sign, .. } => call_sign.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Number,
    Bool,
    Any,
    Function(Option<FunctionType>),
    String,
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Number, &Type::Number) |
            (&Type::Bool, &Type::Bool) |
            (&Type::Function(_), &Type::Function(_)) |
            (&Type::Any, _) |
            (_, &Type::Any) => true,
            _ => false,
        }
    }
}

impl From<ast::Literal> for Type {
    fn from(from: ast::Literal) -> Self {
        match from {
            ast::Literal::Integer(_) |
            ast::Literal::Float(_) => Type::Number,
            ast::Literal::Bool(_) => Type::Bool,
            ast::Literal::String(_) => Type::String,
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
            Type::String => write!(f, "String"),
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
    pub symbol_table: BTreeMap<String, Type>,
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
                                   FunctionType::NativeVoid(CallSign {
                                       num_params: 0,
                                       variadic: true,
                                   }))];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(&name.to_string(), &Type::Function(Some(func.clone())));
        }
        Rc::new(RefCell::new(env))
    }

    pub fn new() -> TypeEnvironment {
        TypeEnvironment {
            symbol_table: BTreeMap::new(),
            parent: None,
        }
    }

    pub fn create_clone(env: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        let cloned_env = env.borrow().clone();
        Rc::new(RefCell::new(cloned_env))
    }

    pub fn create_child(parent: Rc<RefCell<TypeEnvironment>>) -> Rc<RefCell<TypeEnvironment>> {
        let env = TypeEnvironment {
            parent: Some(parent),
            symbol_table: BTreeMap::default(),
        };
        Rc::new(RefCell::new(env))
    }

    pub fn declare(&mut self, id: &str, typ: &Type) {
        self.symbol_table.insert(id.to_owned(), typ.clone());
    }

    pub fn set(&mut self, identifier: &str, typ: Type) -> bool {
        if self.symbol_table.contains_key(identifier) {
            self.symbol_table.insert(identifier.to_owned(), typ);
            true
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow_mut().set(identifier, typ),
                None => false,
            }
        }
    }

    pub fn get_type(&self, identifier: &str) -> Option<Type> {
        if let Some(typ) = self.symbol_table.get(identifier) {
            return Some(typ.clone());
        } else {
            match self.parent {
                Some(ref parent) => parent.borrow().get_type(identifier),
                None => None,
            }
        }
    }

    pub fn get_all_pairs(&self) -> Vec<(String, Type)> {
        let mut pairs = Vec::new();
        for (key, value) in &self.symbol_table {
            pairs.push((key.clone(), value.clone()));
        }
        if let Some(ref parent) = self.parent {
            pairs.append(&mut parent.borrow().get_all_pairs());
        }
        pairs
    }
}

pub fn check_program(ast: &[StatementNode]) -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let root_env = TypeEnvironment::new_root();
    check_statements(ast, root_env.clone())
}

pub fn check_statements(ast: &[StatementNode],
                        env: Rc<RefCell<TypeEnvironment>>)
                        -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    for statement in ast.iter() {
        if let Err(mut e) = check_statement(statement, env.clone()) {
            issues.append(&mut e);
        }
    }
    if issues.is_empty() {
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
            check_statement_variable_declaration(variable, expr, env.clone(), &mut issues);
        }
        Statement::Assignment(ref lhs_expr, ref expr) => {
            check_statement_assignment(lhs_expr, expr, env.clone(), &mut issues);
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
            check_statement_if_then(if_expr, then_block, env.clone(), &mut issues);
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            check_statement_if_then_else(s,
                                         if_expr,
                                         then_block,
                                         else_block,
                                         env.clone(),
                                         &mut issues);
        }
        Statement::Loop(ref block) => {
            if let Err(mut e) = check_statement(block, env.clone()) {
                issues.append(&mut e);
            }
        }
        Statement::Break | Statement::Empty => {}
        Statement::Return(ref possible_expr) => {
            check_statement_return(possible_expr, env.clone(), &mut issues);
        }
    };
    if issues.is_empty() {
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
            match env.borrow().get_type(id) {
                Some(t) => Ok(Some(t)),
                None => Err(vec![(InterpreterError::ReferenceError(id.clone()).into(), expr.pos)]),
            }
        }
        Expr::UnaryExpression(ref op, ref expr) => {
            match check_expr(expr, env.clone()) {
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id))
                                             .into(),
                                         expr.pos)]);
                    }
                    unreachable!();
                }
                Ok(Some(typ)) => {
                    match *op {
                        UnaryOp::Minus => {
                            match check_unary_minus_for_type(typ) {
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
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        return Err(vec![(InterpreterError::NoneError(try_get_name_of_fn(id))
                                             .into(),
                                         expr.pos)]);
                    }
                    unreachable!();
                }
                Ok(Some(_)) => {
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
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = expr1.data {
                        issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                     expr1.pos));
                    }
                    Type::Any
                }
                Ok(Some(t)) => t,
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            let checked_type_2 = match check_expr(expr2, env.clone()) {
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = expr2.data {
                        issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                     expr2.pos));
                    }
                    Type::Any
                }
                Ok(Some(t)) => t,
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
            };
            use ast::BinaryOp::*;
            let result = match *op {
                Add => check_add_for_types(&checked_type_1, &checked_type_2),
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
                    if issues.is_empty() {
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
                    match check_expr(expr1, env.clone()) {
                        Err(mut e) => {
                            issues.append(&mut e);
                        }
                        Ok(None) => {
                            if let Expr::FunctionCall(ref id, _) = expr1.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr1.pos));
                            }
                        }
                        Ok(Some(_)) => {}
                    };
                    match check_expr(expr2, env.clone()) {
                        Err(mut e) => {
                            issues.append(&mut e);
                        }
                        Ok(None) => {
                            if let Expr::FunctionCall(ref id, _) = expr2.data {
                                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id))
                                                 .into(),
                                             expr2.pos));
                            }
                        }
                        Ok(Some(_)) => {}
                    };
                    if issues.is_empty() {
                        Ok(Some(Type::Bool))
                    } else {
                        Err(issues)
                    }
                }
            }
        }
        Expr::FunctionCall(ref f_expr, ref args) => {
            let mut issues = Vec::new();
            let checked_type = match check_expr(f_expr, env.clone()) {
                Err(mut e) => {
                    issues.append(&mut e);
                    Type::Any
                }
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = f_expr.data {
                        issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                     f_expr.pos));
                    }
                    Type::Any
                }
                Ok(Some(t)) => t,
            };

            let mut arg_types = Vec::new();
            for arg in args.iter() {
                let possible_type = check_expr(arg, env.clone());
                let arg_type = match possible_type {
                    Err(mut e) => {
                        issues.append(&mut e);
                        Type::Any
                    }
                    Ok(None) => {
                        if let Expr::FunctionCall(ref id, _) = arg.data {
                            issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                       arg.pos));
                        }
                        Type::Any
                    }
                    Ok(Some(typ)) => typ,
                };
                arg_types.push(arg_type);
            }

            let func_type = match checked_type {
                Type::Function(None) => unreachable!(),
                Type::Function(Some(func_type)) => func_type,
                v => {
                    if let Expr::Identifier(ref id) = expr.data {
                        issues.push((InterpreterError::CallToNonFunction(Some(id.clone()), v)
                                         .into(),
                                     expr.pos));
                    } else {
                        issues.push((InterpreterError::CallToNonFunction(None, v).into(), expr.pos));
                    }
                    return Err(issues);
                }
            };

            if let Err(e) = check_args_compat(&arg_types, &func_type.get_call_sign(), expr) {
                issues.push(e);
            }

            if issues.is_empty() {
                let ret_type = match func_type {
                    FunctionType::NativeVoid(_) => None,
                    FunctionType::NativeReturning(_) |
                    FunctionType::User { .. } => Some(Type::Any),
                };
                Ok(ret_type)
            } else {
                Err(issues)
            }
        }
        Expr::FunctionDefinition(ref possible_id, ref param_list, ref body) => {
            let func = FunctionType::User {
                // FIXME
                returning: false,
                call_sign: CallSign {
                    num_params: param_list.len(),
                    variadic: false,
                },
                param_list: param_list.clone(),
                body: body.clone(),
                env: env.clone(),
            };
            let func_type = Type::Function(Some(func));
            if let Some(ref id) = *possible_id {
                env.borrow_mut().declare(id, &func_type);
            }
            let function_env = TypeEnvironment::create_clone(env);

            for param in param_list {
                function_env.borrow_mut().declare(param, &Type::Any);
            }
            let inner_env = TypeEnvironment::create_child(function_env);
            if let Err(e) = check_statement(body, inner_env) {
                return Err(e);
            }
            Ok(Some(func_type))
        }
    }
}

fn check_statement_variable_declaration(variable: &Variable,
                                        expr: &ExprNode,
                                        env: Rc<RefCell<TypeEnvironment>>,
                                        issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    let checked_type = match check_expr(expr, env.clone()) {
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr.data {
                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(), expr.pos));
            }
            Type::Any
        }
        Ok(Some(t)) => t,
        Err(mut e) => {
            issues.append(&mut e);
            Type::Any
        }
    };
    match *variable {
        Variable::Identifier(_, ref id) => {
            env.borrow_mut().declare(id, &checked_type);
        }
    };
}

fn check_statement_assignment(lhs_expr: &LhsExprNode,
                              expr: &ExprNode,
                              env: Rc<RefCell<TypeEnvironment>>,
                              issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    let checked_type = match check_expr(expr, env.clone()) {
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr.data {
                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(), expr.pos));
            }
            Type::Any
        }
        Ok(Some(t)) => t,
        Err(mut e) => {
            issues.append(&mut e);
            Type::Any
        }
    };
    match lhs_expr.data {
        LhsExpr::Identifier(ref id) => {
            if !env.borrow_mut().set(id, checked_type) {
                issues.push((InterpreterError::UndeclaredAssignment(id.clone()).into(),
                             lhs_expr.pos));
            }
        }
    };
}

fn check_statement_if_then(if_expr: &ExprNode,
                           then_block: &StatementNode,
                           env: Rc<RefCell<TypeEnvironment>>,
                           issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    let if_expr_result = check_expr(if_expr, env.clone());
    match if_expr_result {
        Err(mut e) => issues.append(&mut e),
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = if_expr.data {
                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                             if_expr.pos));
            }
        }
        Ok(Some(_)) => {}
    }
    if let Err(mut e) = check_statement(then_block, env.clone()) {
        issues.append(&mut e);
    }
}

fn check_statement_if_then_else(statement: &StatementNode,
                                if_expr: &ExprNode,
                                then_block: &StatementNode,
                                else_block: &StatementNode,
                                env: Rc<RefCell<TypeEnvironment>>,
                                issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    let then_env = TypeEnvironment::create_clone(env.clone());
    let else_env = TypeEnvironment::create_clone(env.clone());
    let if_expr_result = check_expr(if_expr, env.clone());
    match if_expr_result {
        Err(mut e) => {
            issues.append(&mut e);
        }
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = if_expr.data {
                issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                             if_expr.pos));
            }
        }
        Ok(Some(_)) => {}
    }

    if let Err(mut e) = check_statement(then_block, then_env.clone()) {
        issues.append(&mut e);
    }

    let then_pairs = then_env.borrow().get_all_pairs();

    if let Err(mut e) = check_statement(else_block, else_env.clone()) {
        issues.append(&mut e);
    }

    let else_pairs = else_env.borrow().get_all_pairs();

    for (then_pair, else_pair) in then_pairs.iter().zip(else_pairs.iter()) {
        let &(ref then_name, ref then_type) = then_pair;
        let &(ref else_name, ref else_type) = else_pair;
        if then_name != else_name {
            panic!("Unexpected behaviour when iterating through environments!");
        }
        if else_type != then_type {
            issues.push((TypeCheckerIssue::MultipleTypesFromBranchWarning(then_name.clone()),
                         statement.pos));
            env.borrow_mut().set(then_name, Type::Any);
        } else {
            env.borrow_mut().set(then_name, then_type.clone());
        }
    }
}

fn check_statement_return(possible_expr: &Option<ExprNode>,
                          env: Rc<RefCell<TypeEnvironment>>,
                          issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    if let Some(ref expr) = *possible_expr {
        match check_expr(expr, env.clone()) {
            Ok(None) => {
                if let Expr::FunctionCall(ref id, _) = expr.data {
                    issues.push((InterpreterError::NoneError(try_get_name_of_fn(id)).into(),
                                 expr.pos));
                }
                unreachable!();
            }
            Ok(Some(_)) => {
                // TODO
            }
            Err(mut e) => {
                issues.append(&mut e);
            }
        };
    }
}

fn check_unary_minus_for_type(typ: Type) -> Result<Type, TypeCheckerIssue> {
    match typ {
        Type::Number => Ok(Type::Number),
        Type::Any => Ok(Type::Any),
        _ => Err(InterpreterError::UnaryTypeError(UnaryOp::Minus, typ).into()),
    }
}

fn check_add_for_types(t1: &Type, t2: &Type) -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::String, _) |
        (_, &Type::String) => Ok(Type::String),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(BinaryOp::Add, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_arithmetic_for_types(op: BinaryOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(InterpreterError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_comparison_for_types(op: BinaryOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Bool),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
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

fn check_args_compat(arg_types: &[Type],
                     call_sign: &CallSign,
                     expr: &ExprNode)
                     -> Result<(), TypeCheckerIssueWithPosition> {
    if !call_sign.variadic && call_sign.num_params != arg_types.len() {
        if let Expr::Identifier(ref id) = expr.data {
            return Err((InterpreterError::ArgumentLength(Some(id.clone())).into(), expr.pos));
        }
        return Err((InterpreterError::ArgumentLength(None).into(), expr.pos));
    }
    Ok(())
}
