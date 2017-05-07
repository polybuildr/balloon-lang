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
        body: Box<ast::StmtNode>,
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
    Function(Box<Option<FunctionType>>),
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
            ConstraintType::Function => Type::Function(Box::new(None)),
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
            (&Type::String, &Type::String) |
            (&Type::Tuple, &Type::Tuple) |
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

impl Context {
    fn root() -> Context {
        Context {
            in_loop: false,
            in_func: false,
            func_ret_type: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
        let builtin_functions = &[
            ("println",
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
                                      })),
        ];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(&name.to_string(),
                        &Type::Function(Box::new(Some(func.clone()))));
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

pub struct TypeChecker {
    context: Context,
    issues: Vec<TypeCheckerIssueWithPosition>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            context: Context::root(),
            issues: Vec::new(),
        }
    }

    pub fn get_issues(&self) -> Vec<TypeCheckerIssueWithPosition> {
        return self.issues.clone();
    }

    pub fn check_program(&mut self, ast: &[StmtNode]) {
        let root_env = TypeEnvironment::new_root();
        self.check_statements(ast, root_env.clone());
    }

    pub fn check_statements(&mut self, ast: &[StmtNode], env: Rc<RefCell<TypeEnvironment>>) {
        for statement in ast.iter() {
            self.check_statement(statement, env.clone());
        }
    }

    pub fn check_statement(&mut self, s: &StmtNode, env: Rc<RefCell<TypeEnvironment>>) {
        match s.data {
            Stmt::VarDecl(ref variable, ref expr) => {
                self.check_statement_variable_declaration(variable, expr, env.clone());
            }
            Stmt::Assign(ref lhs_expr, ref expr) => {
                self.check_statement_assignment(lhs_expr, expr, env.clone());
            }
            Stmt::Block(ref statements) => {
                let child_env = TypeEnvironment::create_child(env);
                self.check_statements(statements, child_env);
            }
            Stmt::Expr(ref expr) => {
                self.check_expr(expr, env.clone());
            }
            Stmt::IfThen(IfThenStmt {
                             ref cond,
                             ref then_block,
                             ref maybe_else_block,
                         }) => {
                self.check_statement_if_then_else(s,
                                                  cond,
                                                  then_block,
                                                  maybe_else_block,
                                                  env.clone());
            }
            Stmt::Loop(ref block) => {
                let old_in_loop_value = self.context.in_loop;
                self.context.in_loop = true;
                self.check_statement(block, env.clone());
                self.context.in_loop = old_in_loop_value;
            }
            Stmt::Break => {
                if self.context.in_loop != true {
                    self.issues
                        .push((RuntimeError::BreakOutsideLoop.into(), s.pos));
                }
            }
            Stmt::Empty => {}
            Stmt::Return(ref possible_expr) => {
                self.check_statement_return(possible_expr, s, env.clone());
            }
        };
    }

    fn check_expr(&mut self, expr: &ExprNode, env: Rc<RefCell<TypeEnvironment>>) -> Option<Type> {
        match expr.data {
            Expr::Literal(ref x) => Some(Type::from(x.data.clone())),
            Expr::Identifier(ref id) => {
                match env.borrow().get_type(id) {
                    Some(t) => Some(t),
                    None => {
                        self.issues
                            .push((RuntimeError::ReferenceError(id.clone()).into(), expr.pos));
                        Some(Type::Any)
                    }
                }
            }
            Expr::Tuple(ref elems) => self.check_expr_tuple(elems, env.clone()),
            Expr::Unary(ref op, ref expr) => self.check_expr_unary_op(op, expr, env.clone()),
            Expr::UnaryLogical(ref op, ref expr) => {
                self.check_expr_unary_logical_op(op, expr, env.clone())
            }
            Expr::Binary(ref expr1, ref op, ref expr2) => {
                self.check_expr_binary_expr(expr, expr1, op, expr2, env.clone())
            }
            Expr::BinaryLogical(ref expr1, ref op, ref expr2) => {
                self.check_expr_binary_logical_expr(expr1, op, expr2, env.clone())
            }
            Expr::FnCall(ref f_expr, ref args) => {
                self.check_expr_function_call(expr, f_expr, args, env.clone())
            }
            Expr::FnDef(FnDefExpr {
                            ref maybe_id,
                            ref params,
                            ref body,
                            ref maybe_ret_type,
                        }) => {
                self.check_expr_function_definition(maybe_id,
                                                    params,
                                                    body,
                                                    maybe_ret_type,
                                                    env.clone())
            }
            Expr::MemberByIdx(ref expr, ref index_expr) => {
                self.check_expr_member_access_by_index(expr, index_expr, env.clone())
            }
        }
    }


    fn check_statement_variable_declaration(&mut self,
                                            variable: &Variable,
                                            expr: &ExprNode,
                                            env: Rc<RefCell<TypeEnvironment>>) {
        let checked_type = self.check_expr_as_value(expr, env.clone());
        match *variable {
            Variable::Identifier(_, ref id) => {
                env.borrow_mut().declare(id, &checked_type);
            }
        };
    }

    fn check_statement_assignment(&mut self,
                                  lhs_expr: &LhsExprNode,
                                  expr: &ExprNode,
                                  env: Rc<RefCell<TypeEnvironment>>) {
        let checked_type = self.check_expr_as_value(expr, env.clone());
        match lhs_expr.data {
            LhsExpr::Identifier(ref id) => {
                if !env.borrow_mut().set(id, checked_type) {
                    self.issues
                        .push((RuntimeError::UndeclaredAssignment(id.clone()).into(),
                               lhs_expr.pos));
                }
            }
        };
    }

    fn check_statement_if_then_else(&mut self,
                                    statement: &StmtNode,
                                    if_expr: &ExprNode,
                                    then_block: &StmtNode,
                                    maybe_else_block: &Option<Box<StmtNode>>,
                                    env: Rc<RefCell<TypeEnvironment>>) {
        let else_block = match *maybe_else_block {
            None => {
                StmtNode {
                    data: Stmt::Block(vec![]),
                    pos: (0, 0), // dummy span
                }
            }
            Some(ref block) => *block.clone(),
        };

        let then_env = TypeEnvironment::create_clone(env.clone());
        let else_env = TypeEnvironment::create_clone(env.clone());
        self.check_expr_as_value(if_expr, env.clone());

        self.check_statement(then_block, then_env.clone());

        let then_pairs = then_env.borrow().get_all_pairs();

        self.check_statement(&else_block, else_env.clone());

        let else_pairs = else_env.borrow().get_all_pairs();

        for (then_pair, else_pair) in then_pairs.iter().zip(else_pairs.iter()) {
            let &(ref then_name, ref then_type) = then_pair;
            let &(ref else_name, ref else_type) = else_pair;
            if then_name != else_name {
                panic!("Unexpected behaviour when iterating through environments!");
            }
            if else_type != then_type {
                self.issues
                    .push((TypeCheckerIssue::MultipleTypesFromBranchWarning(then_name.clone()),
                           statement.pos));
                env.borrow_mut().set(then_name, Type::Any);
            } else {
                env.borrow_mut().set(then_name, then_type.clone());
            }
        }
    }

    fn check_statement_return(&mut self,
                              possible_expr: &Option<ExprNode>,
                              return_statement: &StmtNode,
                              env: Rc<RefCell<TypeEnvironment>>) {
        if self.context.in_func != true {
            self.issues
                .push((RuntimeError::ReturnOutsideFunction.into(), return_statement.pos));
            // if the return is outside a function, don't typecheck anything else wrt the return
            return;
        }
        match *possible_expr {
            Some(ref expr) => {
                let actual_type = self.check_expr_as_value(expr, env.clone());
                let actual_return_constraint_type = ConstraintType::from(actual_type);
                match self.context.func_ret_type {
                    None => {
                        self.issues.push((
                            TypeCheckerIssue::ReturnTypeMismatch(
                                None, Some(actual_return_constraint_type)
                            ),
                            return_statement.pos
                        ));
                    }
                    Some(ref expected_type) => {
                        if !actual_return_constraint_type.compatible_with(expected_type) {
                            self.issues.push((
                                TypeCheckerIssue::ReturnTypeMismatch(
                                    Some(expected_type.clone()),
                                    Some(actual_return_constraint_type)
                                ),
                                return_statement.pos
                            ));
                        }
                    }
                }
            }
            None => {
                if !self.context.func_ret_type.is_none() {
                    self.issues.push((
                        TypeCheckerIssue::ReturnTypeMismatch(
                            self.context.clone().func_ret_type, None
                        ),
                        return_statement.pos
                    ));
                }
            }
        };
    }

    fn check_expr_tuple(&mut self,
                        elems: &[ExprNode],
                        env: Rc<RefCell<TypeEnvironment>>)
                        -> Option<Type> {
        for elem_expr in elems {
            self.check_expr_as_value(elem_expr, env.clone());
        }
        Some(Type::Tuple)
    }

    fn check_expr_unary_op(&mut self,
                           op: &UnOp,
                           expr: &ExprNode,
                           env: Rc<RefCell<TypeEnvironment>>)
                           -> Option<Type> {
        let typ = self.check_expr_as_value(expr, env.clone());
        match *op {
            UnOp::Neg => {
                match check_unary_minus_for_type(typ) {
                    Ok(t) => Some(t),
                    Err(e) => {
                        self.issues.push((e, expr.pos));
                        Some(Type::Any)
                    }
                }
            }
        }
    }

    fn check_expr_unary_logical_op(&mut self,
                                   op: &LogicalUnOp,
                                   expr: &ExprNode,
                                   env: Rc<RefCell<TypeEnvironment>>)
                                   -> Option<Type> {
        self.check_expr_as_value(expr, env.clone());
        match *op {
            LogicalUnOp::Not => Some(Type::Bool),
        }
    }

    fn check_expr_binary_expr(&mut self,
                              binary_expr: &ExprNode,
                              expr1: &ExprNode,
                              op: &BinOp,
                              expr2: &ExprNode,
                              env: Rc<RefCell<TypeEnvironment>>)
                              -> Option<Type> {
        let checked_type_1 = self.check_expr_as_value(expr1, env.clone());
        let checked_type_2 = self.check_expr_as_value(expr2, env.clone());
        use ast::BinOp::*;
        let result = match *op {
            Add => check_add_for_types(&checked_type_1, &checked_type_2),
            ref op @ Sub | ref op @ Mul | ref op @ Div => {
                check_binary_arithmetic_for_types(op.clone(), &checked_type_1, &checked_type_2)
            }
            ref op @ Lt | ref op @ Lte | ref op @ Gt | ref op @ Gte => {
                check_binary_comparison_for_types(op.clone(), &checked_type_1, &checked_type_2)
            }
            Eq => Ok(Type::Bool),
        };
        match result {
            Err(e) => {
                self.issues.push((e, binary_expr.pos));
                Some(Type::Any)
            }
            Ok(t) => Some(t),
        }
    }

    fn check_expr_binary_logical_expr(&mut self,
                                      expr1: &ExprNode,
                                      op: &LogicalBinOp,
                                      expr2: &ExprNode,
                                      env: Rc<RefCell<TypeEnvironment>>)
                                      -> Option<Type> {
        match *op {
            LogicalBinOp::And | LogicalBinOp::Or => {
                self.check_expr_as_value(expr1, env.clone());
                self.check_expr_as_value(expr2, env.clone());
            }
        }
        Some(Type::Bool)
    }


    fn check_expr_function_call(&mut self,
                                expr: &ExprNode,
                                f_expr: &ExprNode,
                                args: &[ExprNode],
                                env: Rc<RefCell<TypeEnvironment>>)
                                -> Option<Type> {
        let checked_type = self.check_expr_as_value(f_expr, env.clone());

        let mut arg_types = Vec::new();
        for arg in args.iter() {
            let arg_type = self.check_expr_as_value(arg, env.clone());
            arg_types.push(arg_type);
        }

        let func_type = match checked_type {
            Type::Function(possible_func) => {
                match *possible_func {
                    None => unreachable!(),
                    Some(func_type) => func_type,
                }
            }
            v => {
                if let Expr::Identifier(ref id) = expr.data {
                    self.issues
                        .push((RuntimeError::CallToNonFunction(Some(id.clone()), v).into(),
                               expr.pos));
                } else {
                    self.issues
                        .push((RuntimeError::CallToNonFunction(None, v).into(), expr.pos));
                }
                return Some(Type::Any);
            }
        };

        let func_call_sign = func_type.get_call_sign();
        if !func_call_sign.variadic &&
           arg_types.len() != func_type.get_call_sign().param_types.len() {
            if let Expr::Identifier(ref id) = expr.data {
                self.issues
                    .push((RuntimeError::ArgumentLength(Some(id.clone())).into(), expr.pos));
            } else {
                self.issues
                    .push((RuntimeError::ArgumentLength(None).into(), expr.pos));
            }
            return Some(Type::Any);
        } else {
            if let Err(mut e) = check_args_compat(&arg_types, args, &func_call_sign) {
                self.issues.append(&mut e);
            }

            if let FunctionType::User {
                       ref param_names,
                       ref body,
                       ref env,
                       ref already_checked_param_types,
                       ref ret_type,
                       ..
                   } = func_type {
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
                        env.borrow_mut()
                            .set(&id, Type::Function(Box::new(Some(new_func_type))));

                        let old_context = self.context.clone();
                        self.context = Context {
                            in_loop: false,
                            in_func: true,
                            func_ret_type: ret_type.clone(),
                        };
                        let mut outer_issues = self.issues.clone();
                        self.issues = Vec::new();
                        self.check_statement(body, inner_env);
                        for inner_issue in self.issues.iter() {
                            outer_issues
                                .push((
                                    TypeCheckerIssue::InsideFunctionCall(
                                        Box::new(inner_issue.clone())
                                    ),
                                    expr.pos
                                ));
                        }
                        self.issues = outer_issues;
                        self.context = old_context;
                    }
                }
            };
        }

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
        ret_type
    }

    fn check_expr_function_definition(&mut self,
                                      possible_id: &Option<String>,
                                      param_list: &[(String, Option<ConstraintType>)],
                                      body: &Box<StmtNode>,
                                      type_hint: &Option<ConstraintType>,
                                      env: Rc<RefCell<TypeEnvironment>>)
                                      -> Option<Type> {
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
        let func_type = Type::Function(Box::new(Some(func)));
        if let Some(ref id) = *possible_id {
            env.borrow_mut().declare(id, &func_type);
        }
        let function_env = TypeEnvironment::create_clone(env);

        for param in param_names {
            function_env.borrow_mut().declare(&param, &Type::Any);
        }
        let inner_env = TypeEnvironment::create_child(function_env);
        let old_context = self.context.clone();
        self.context = Context {
            in_loop: false,
            in_func: true,
            func_ret_type: type_hint.clone(),
        };
        self.check_statement(body, inner_env);
        self.context = old_context;
        Some(func_type)
    }

    fn check_expr_member_access_by_index(&mut self,
                                         expr: &ExprNode,
                                         index_expr: &ExprNode,
                                         env: Rc<RefCell<TypeEnvironment>>)
                                         -> Option<Type> {
        let object_type = self.check_expr_as_value(expr, env.clone());
        match object_type {
            Type::Tuple | Type::Any => {}
            typ => {
                self.issues
                    .push((RuntimeError::SubscriptOnNonSubscriptable(typ).into(), expr.pos));
            }
        };
        let typ = self.check_expr_as_value(index_expr, env.clone());
        match typ {
            Type::Number | Type::Any => {}
            non_integral_type => {
                self.issues
                    .push((RuntimeError::NonIntegralSubscript(non_integral_type).into(),
                           index_expr.pos));
            }
        };
        Some(Type::Any)
    }

    fn check_expr_as_value(&mut self, expr: &ExprNode, env: Rc<RefCell<TypeEnvironment>>) -> Type {
        let possible_type = self.check_expr(expr, env);
        if possible_type.is_none() {
            if let Expr::FnCall(ref f_expr, _) = expr.data {
                if let Expr::Identifier(ref id) = f_expr.data {
                    self.issues
                        .push((RuntimeError::NoneError(Some(id.clone())).into(), expr.pos));
                } else {
                    self.issues
                        .push((RuntimeError::NoneError(None).into(), expr.pos));
                }
            } else {
                unreachable!();
            }
            Type::Any
        } else {
            possible_type.clone().unwrap()
        }
    }
}

fn check_unary_minus_for_type(typ: Type) -> Result<Type, TypeCheckerIssue> {
    match typ {
        Type::Number => Ok(Type::Number),
        Type::Any => Ok(Type::Any),
        _ => Err(RuntimeError::UnaryTypeError(UnOp::Neg, typ).into()),
    }
}

fn check_add_for_types(t1: &Type, t2: &Type) -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::String, _) |
        (_, &Type::String) => Ok(Type::String),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(RuntimeError::BinaryTypeError(BinOp::Add, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_arithmetic_for_types(op: BinOp,
                                     t1: &Type,
                                     t2: &Type)
                                     -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(RuntimeError::BinaryTypeError(op, t1.clone(), t2.clone()).into()),
    }
}

fn check_binary_comparison_for_types(op: BinOp,
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

fn get_function_type_with_updated_already_checked(
    old_fn_type: &FunctionType,
    new_already_checked: LinearMap<Vec<ConstraintType>,()>)
-> FunctionType{

    if let FunctionType::User {
               ref param_names,
               ref body,
               ref env,
               ref call_sign,
               ref ret_type,
               ..
           } = *old_fn_type {
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
