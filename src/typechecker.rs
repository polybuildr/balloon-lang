use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::Iterator;

use linear_map::LinearMap;

use ast::*;
use ast;
use runtime::RuntimeError;
use function::*;

#[derive(Clone, Debug)]
pub enum FunctionType {
    NativeVoid(CallSign),
    NativeReturning(CallSign),
    User {
        ret_type: Option<ConstraintType>,
        call_sign: CallSign,
        param_names: Vec<String>,
        body: Box<ast::StatementNode>,
        env: Rc<RefCell<TypeEnvironment>>,
        already_checked_param_types: LinearMap<Vec<ConstraintType>, ()>,
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
    Tuple,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstraintType {
    Number,
    Bool,
    Any,
    Function,
    Tuple,
    String,
}

impl ConstraintType {
    fn compatible_with(&self, other: &ConstraintType) -> bool {
        match (self, other) {
            (&ConstraintType::Number, &ConstraintType::Number) |
            (&ConstraintType::Bool, &ConstraintType::Bool) |
            (&ConstraintType::Function, &ConstraintType::Function) |
            (&ConstraintType::Tuple, &ConstraintType::Tuple) |
            (&ConstraintType::String, &ConstraintType::String) |
            (&ConstraintType::Any, _) |
            (_, &ConstraintType::Any) => true,
            _ => false,
        }
    }
}

impl From<ConstraintType> for Type {
    fn from(from: ConstraintType) -> Self {
        match from {
            ConstraintType::Any => Type::Any,
            ConstraintType::Bool => Type::Bool,
            ConstraintType::Function => Type::Function(None),
            ConstraintType::Number => Type::Number,
            ConstraintType::String => Type::String,
            ConstraintType::Tuple => Type::Tuple,
        }
    }
}

impl Eq for ConstraintType {}

impl From<Type> for ConstraintType {
    fn from(from: Type) -> Self {
        match from {
            Type::Any => ConstraintType::Any,
            Type::Bool => ConstraintType::Bool,
            Type::Function(_) => ConstraintType::Function,
            Type::Number => ConstraintType::Number,
            Type::String => ConstraintType::String,
            Type::Tuple => ConstraintType::Tuple,
        }
    }
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
            Type::Tuple => write!(f, "Tuple"),
        }
    }
}

impl fmt::Display for ConstraintType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Type::from(self.clone()))
    }
}

#[derive(Clone)]
pub struct Context {
    pub in_loop: bool,
    pub in_func: bool,
    pub func_ret_type: Option<ConstraintType>,
}

#[derive(Debug, PartialEq)]
pub enum TypeCheckerIssue {
    RuntimeError(RuntimeError),
    MultipleTypesFromBranchWarning(String),
    InsideFunctionCall(Box<TypeCheckerIssueWithPosition>),
    /// expected, actual
    ReturnTypeMismatch(Option<ConstraintType>, Option<ConstraintType>),
    /// expected, actual
    ArgumentTypeMismatch(ConstraintType, ConstraintType),
}

pub type TypeCheckerIssueWithPosition = (TypeCheckerIssue, OffsetSpan);

impl From<RuntimeError> for TypeCheckerIssue {
    fn from(from: RuntimeError) -> Self {
        TypeCheckerIssue::RuntimeError(from)
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
                                       param_types: vec![],
                                   })),
                                  ("len",
                                   FunctionType::NativeReturning(CallSign {
                                       num_params: 1,
                                       variadic: false,
                                       param_types: vec![None],
                                   })),
                                  ("run_http_server",
                                   FunctionType::NativeVoid(CallSign {
                                       num_params: 1,
                                       variadic: false,
                                       param_types: vec![Some(ConstraintType::Function)],
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
    let mut context = Context {
        in_loop: false,
        in_func: false,
        func_ret_type: None,
    };
    check_statements(ast, root_env.clone(), &mut context)
}

pub fn check_statements(ast: &[StatementNode],
                        env: Rc<RefCell<TypeEnvironment>>,
                        context: &mut Context)
                        -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    for statement in ast.iter() {
        if let Err(mut e) = check_statement(statement, env.clone(), context) {
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
                       env: Rc<RefCell<TypeEnvironment>>,
                       context: &mut Context)
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
            if let Err(mut e) = check_statements(statements, child_env, context) {
                issues.append(&mut e);
            }
        }
        Statement::Expression(ref expr) => {
            if let Err(mut e) = check_expr(expr, env.clone()) {
                issues.append(&mut e);
            }
        }
        Statement::IfThen(ref if_expr, ref then_block) => {
            check_statement_if_then(if_expr, then_block, env.clone(), context, &mut issues);
        }
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            check_statement_if_then_else(s,
                                         if_expr,
                                         then_block,
                                         else_block,
                                         env.clone(),
                                         context,
                                         &mut issues);
        }
        Statement::Loop(ref block) => {
            let old_in_loop_value = context.in_loop;
            context.in_loop = true;
            if let Err(mut e) = check_statement(block, env.clone(), context) {
                issues.append(&mut e);
            }
            context.in_loop = old_in_loop_value;
        }
        Statement::Break => {
            if context.in_loop != true {
                issues.push((RuntimeError::BreakOutsideLoop.into(), s.pos));
            }
        }
        Statement::Empty => {}
        Statement::Return(ref possible_expr) => {
            check_statement_return(possible_expr, s, env.clone(), context, &mut issues);
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
                None => Err(vec![(RuntimeError::ReferenceError(id.clone()).into(), expr.pos)]),
            }
        }
        Expr::Tuple(ref elems) => check_expr_tuple(elems, env.clone()),
        Expr::UnaryExpression(ref op, ref expr) => check_expr_unary_op(op, expr, env.clone()),
        Expr::UnaryLogicalExpression(ref op, ref expr) => {
            check_expr_unary_logical_op(op, expr, env.clone())
        }
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            check_expr_binary_expr(expr, expr1, op, expr2, env.clone())
        }
        Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
            check_expr_binary_logical_expr(expr1, op, expr2, env.clone())
        }
        Expr::FunctionCall(ref f_expr, ref args) => {
            check_expr_function_call(expr, f_expr, args, env.clone())
        }
        Expr::FunctionDefinition(ref possible_id, ref param_list, ref body, ref type_hint) => {
            check_expr_function_definition(possible_id, param_list, body, type_hint, env.clone())
        }
        Expr::MemberAccessByIndex(ref expr, ref index_expr) => {
            check_expr_member_access_by_index(expr, index_expr, env.clone())
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
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), expr.pos));
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
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), expr.pos));
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
                issues.push((RuntimeError::UndeclaredAssignment(id.clone()).into(), lhs_expr.pos));
            }
        }
    };
}

fn check_statement_if_then(if_expr: &ExprNode,
                           then_block: &StatementNode,
                           env: Rc<RefCell<TypeEnvironment>>,
                           context: &mut Context,
                           issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    let if_expr_result = check_expr(if_expr, env.clone());
    match if_expr_result {
        Err(mut e) => issues.append(&mut e),
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = if_expr.data {
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), if_expr.pos));
            }
        }
        Ok(Some(_)) => {}
    }
    if let Err(mut e) = check_statement(then_block, env.clone(), context) {
        issues.append(&mut e);
    }
}

fn check_statement_if_then_else(statement: &StatementNode,
                                if_expr: &ExprNode,
                                then_block: &StatementNode,
                                else_block: &StatementNode,
                                env: Rc<RefCell<TypeEnvironment>>,
                                context: &mut Context,
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
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), if_expr.pos));
            }
        }
        Ok(Some(_)) => {}
    }

    if let Err(mut e) = check_statement(then_block, then_env.clone(), context) {
        issues.append(&mut e);
    }

    let then_pairs = then_env.borrow().get_all_pairs();

    if let Err(mut e) = check_statement(else_block, else_env.clone(), context) {
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
                          return_statement: &StatementNode,
                          env: Rc<RefCell<TypeEnvironment>>,
                          context: &mut Context,
                          issues: &mut Vec<TypeCheckerIssueWithPosition>) {
    if context.in_func != true {
        issues.push((RuntimeError::ReturnOutsideFunction.into(), return_statement.pos));
        // if the return is outside a function, don't typecheck anything else wrt the return
        return;
    }
    match *possible_expr {
        Some(ref expr) => {
            match check_expr(expr, env.clone()) {
                Ok(None) => {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
                                     expr.pos));
                    } else {
                        unreachable!();
                    }
                }
                Ok(Some(actual_typ)) => {
                    let actual_return_constraint_type = ConstraintType::from(actual_typ);
                    match context.func_ret_type {
                        None => {
                            issues.push((
                                TypeCheckerIssue::ReturnTypeMismatch(
                                    None, Some(actual_return_constraint_type)
                                ),
                                return_statement.pos
                            ));
                        }
                        Some(ref expected_type) => {
                            if !actual_return_constraint_type.compatible_with(expected_type) {
                                issues.push((
                                    TypeCheckerIssue::ReturnTypeMismatch(
                                        Some(expected_type.clone()), Some(actual_return_constraint_type)
                                    ),
                                    return_statement.pos
                                ));
                            }
                        }
                    }
                }
                Err(mut e) => {
                    issues.append(&mut e);
                }
            }
        }
        None => {
            if !context.func_ret_type.is_none() {
                issues.push((TypeCheckerIssue::ReturnTypeMismatch(context.clone().func_ret_type,
                                                                  None),
                             return_statement.pos));
            }
        }
    };
}

fn check_expr_tuple(elems: &[ExprNode],
                    env: Rc<RefCell<TypeEnvironment>>)
                    -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    for elem_expr in elems {
        match check_expr(elem_expr, env.clone()) {
            Ok(None) => {
                if let Expr::FunctionCall(ref id, _) = elem_expr.data {
                    issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
                                 elem_expr.pos));
                }
                unreachable!();
            }
            Ok(Some(_)) => {
                // TODO?
            }
            Err(mut e) => {
                issues.append(&mut e);
            }
        };
    }
    if issues.is_empty() {
        Ok(Some(Type::Tuple))
    } else {
        Err(issues)
    }
}

fn check_expr_unary_op(op: &UnaryOp,
                       expr: &ExprNode,
                       env: Rc<RefCell<TypeEnvironment>>)
                       -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    match check_expr(expr, env.clone()) {
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr.data {
                return Err(vec![(RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
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

fn check_expr_unary_logical_op(op: &LogicalUnaryOp,
                               expr: &ExprNode,
                               env: Rc<RefCell<TypeEnvironment>>)
                               -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    match check_expr(expr, env.clone()) {
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr.data {
                return Err(vec![(RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
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

fn check_expr_binary_expr(binary_expr: &ExprNode,
                          expr1: &ExprNode,
                          op: &BinaryOp,
                          expr2: &ExprNode,
                          env: Rc<RefCell<TypeEnvironment>>)
                          -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    let checked_type_1 = match check_expr(expr1, env.clone()) {
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr1.data {
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), expr1.pos));
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
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), expr2.pos));
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
            issues.push((e, binary_expr.pos));
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

fn check_expr_binary_logical_expr(expr1: &ExprNode,
                                  op: &LogicalBinaryOp,
                                  expr2: &ExprNode,
                                  env: Rc<RefCell<TypeEnvironment>>)
                                  -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
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
                        issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
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
                        issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
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

fn check_expr_function_call(expr: &ExprNode,
                            f_expr: &ExprNode,
                            args: &[ExprNode],
                            env: Rc<RefCell<TypeEnvironment>>)
                            -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    let checked_type = match check_expr(f_expr, env.clone()) {
        Err(mut e) => {
            issues.append(&mut e);
            Type::Any
        }
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = f_expr.data {
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), f_expr.pos));
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
                    issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), arg.pos));
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
                issues.push((RuntimeError::CallToNonFunction(Some(id.clone()), v).into(),
                             expr.pos));
            } else {
                issues.push((RuntimeError::CallToNonFunction(None, v).into(), expr.pos));
            }
            return Err(issues);
        }
    };

    let func_call_sign = func_type.get_call_sign();
    if !func_call_sign.variadic && arg_types.len() != func_type.get_call_sign().param_types.len() {
        if let Expr::Identifier(ref id) = expr.data {
            return Err(vec![(RuntimeError::ArgumentLength(Some(id.clone())).into(), expr.pos)]);
        } else {
            return Err(vec![(RuntimeError::ArgumentLength(None).into(), expr.pos)]);
        }
    } else {
        if let Err(mut e) = check_args_compat(&arg_types, args, &func_call_sign) {
            issues.append(&mut e);
        }

        if let FunctionType::User { ref param_names,
                                    ref body,
                                    ref env,
                                    ref already_checked_param_types,
                                    ref ret_type,
                                    .. } = func_type {
            let function_env = TypeEnvironment::create_child(env.clone());
            for (param, arg) in param_names.iter().zip(arg_types.iter()) {
                function_env.borrow_mut().declare(param, arg);
            }
            let inner_env = TypeEnvironment::create_child(function_env);

            let constraint_types = arg_types.iter().map(|typ| typ.clone().into()).collect();
            if !already_checked_param_types.contains_key(&constraint_types) {
                if let Some(id) = try_get_name_of_fn(&Box::new(f_expr.clone())) {
                    let mut new_checked_param_types = already_checked_param_types.clone();
                    new_checked_param_types.insert(constraint_types, ());
                    let new_func_type =
                        get_function_type_with_updated_already_checked(&func_type,
                                                                       new_checked_param_types);
                    env.borrow_mut().set(&id, Type::Function(Some(new_func_type)));

                    let mut context = Context {
                        in_loop: false,
                        in_func: true,
                        func_ret_type: ret_type.clone(),
                    };
                    if let Err(errors) = check_statement(&body, inner_env, &mut context) {
                        for error in errors {
                            issues.push((TypeCheckerIssue::InsideFunctionCall(Box::new(error)),
                                         expr.pos));
                        }
                    }
                }
            }
        };
    }

    if issues.is_empty() {
        let ret_type = match func_type {
            FunctionType::NativeVoid(_) => None,
            FunctionType::NativeReturning(_) => Some(Type::Any),
            FunctionType::User { ref ret_type, .. } => {
                match *ret_type {
                    None => None,
                    Some(ref typ) => Some(typ.clone().into()),
                }
            }
        };
        Ok(ret_type)
    } else {
        Err(issues)
    }
}

fn check_expr_function_definition(possible_id: &Option<String>,
                                  param_list: &[(String, Option<ConstraintType>)],
                                  body: &Box<StatementNode>,
                                  type_hint: &Option<ConstraintType>,
                                  env: Rc<RefCell<TypeEnvironment>>)
                                  -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    let (param_names, param_types): (Vec<String>, Vec<Option<ConstraintType>>) =
        param_list.iter().cloned().unzip();
    let mut linear_map_with_any_set = LinearMap::new();
    linear_map_with_any_set.insert(vec![ConstraintType::Any; param_list.len()], ());
    let func = FunctionType::User {
        ret_type: type_hint.clone(),
        call_sign: CallSign {
            num_params: param_list.len(),
            variadic: false,
            param_types: param_types.clone(),
        },
        param_names: param_names.to_vec(),
        body: body.clone(),
        env: env.clone(),
        already_checked_param_types: linear_map_with_any_set,
    };
    let func_type = Type::Function(Some(func));
    if let Some(ref id) = *possible_id {
        env.borrow_mut().declare(id, &func_type);
    }
    let function_env = TypeEnvironment::create_clone(env);

    for param in param_names {
        function_env.borrow_mut().declare(&param, &Type::Any);
    }
    let inner_env = TypeEnvironment::create_child(function_env);
    let mut context = Context {
        in_loop: false,
        in_func: true,
        func_ret_type: type_hint.clone(),
    };
    if let Err(e) = check_statement(body, inner_env, &mut context) {
        return Err(e);
    }
    Ok(Some(func_type))
}

fn check_expr_member_access_by_index(expr: &ExprNode,
                                     index_expr: &ExprNode,
                                     env: Rc<RefCell<TypeEnvironment>>)
                                     -> Result<Option<Type>, Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    let object_type = match check_expr(expr, env.clone()) {
        Err(mut e) => {
            issues.append(&mut e);
            Type::Any
        }
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = expr.data {
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(), expr.pos));
            }
            Type::Any
        }
        Ok(Some(typ)) => typ,
    };
    match object_type {
        Type::Tuple => {}
        typ => {
            issues.push((RuntimeError::SubscriptOnNonSubscriptable(typ).into(), expr.pos));
        }
    };
    match check_expr(index_expr, env.clone()) {
        Err(mut e) => {
            issues.append(&mut e);
        }
        Ok(None) => {
            if let Expr::FunctionCall(ref id, _) = index_expr.data {
                issues.push((RuntimeError::NoneError(try_get_name_of_fn(id)).into(),
                                index_expr.pos));
            }
        }
        Ok(Some(typ)) => {
            match typ {
                Type::Number => {}
                non_integral_type => {
                    issues.push((RuntimeError::NonIntegralSubscript(non_integral_type).into(),
                                 index_expr.pos));
                }
            };
        }
    };
    if issues.is_empty() {
        Ok(Some(Type::Any))
    } else {
        Err(issues)
    }
}

fn check_unary_minus_for_type(typ: Type) -> Result<Type, TypeCheckerIssue> {
    match typ {
        Type::Number => Ok(Type::Number),
        Type::Any => Ok(Type::Any),
        _ => Err(RuntimeError::UnaryTypeError(UnaryOp::Minus, typ).into()),
    }
}

fn check_add_for_types(t1: &Type, t2: &Type) -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::String, _) |
        (_, &Type::String) => Ok(Type::String),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(RuntimeError::BinaryTypeError(BinaryOp::Add, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_arithmetic_for_types(op: BinaryOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(RuntimeError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_comparison_for_types(op: BinaryOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Bool),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(RuntimeError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
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
                     arg_nodes: &[ExprNode],
                     call_sign: &CallSign)
                     -> Result<(), Vec<TypeCheckerIssueWithPosition>> {
    let mut issues = Vec::new();
    let types_iter = arg_types.iter().zip(call_sign.clone().param_types);
    for (arg_node, (arg_type, possible_expected_type)) in arg_nodes.iter().zip(types_iter) {
        let arg_constraint_type = ConstraintType::from(arg_type.clone());
        match possible_expected_type {
            None => {
                continue;
            }
            Some(ref expected_type) => {
                if !arg_constraint_type.compatible_with(expected_type) {
                    issues.push((TypeCheckerIssue::ArgumentTypeMismatch(expected_type.clone(),
                                                                        arg_constraint_type),
                                 arg_node.pos));
                }
            }
        }
    }
    if issues.is_empty() {
        Ok(())
    } else {
        Err(issues)
    }
}

fn get_function_type_with_updated_already_checked(old_fn_type: &FunctionType, new_already_checked: LinearMap<Vec<ConstraintType>, ()>) -> FunctionType {
    if let &FunctionType::User { ref param_names,
                                 ref body,
                                 ref env,
                                 ref call_sign,
                                 ref ret_type,
                                 .. } = old_fn_type {
        FunctionType::User {
            param_names: param_names.clone(),
            body: body.clone(),
            env: env.clone(),
            already_checked_param_types: new_already_checked,
            call_sign: call_sign.clone(),
            ret_type: ret_type.clone(),
        }
    } else {
        panic!("Not a user function");
    }
}
