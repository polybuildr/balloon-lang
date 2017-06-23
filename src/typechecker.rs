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
        call_sign: CallSign,
        param_names: Vec<String>,
        body: Box<ast::StmtNode>,
        env: Rc<RefCell<TypeEnvironment>>,
        already_checked_param_types: LinearMap<Vec<Type>, ()>,
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

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Number, &Type::Number) |
            (&Type::Bool, &Type::Bool) |
            (&Type::Function(_), &Type::Function(_)) | // TODO
            (&Type::String, &Type::String) |
            (&Type::Tuple, &Type::Tuple) |
            (&Type::Any, &Type::Any) => true,
            _ => false,
        }
    }
}

impl Type {
    fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Number, &Type::Number) |
            (&Type::Bool, &Type::Bool) |
            (&Type::Function(_), &Type::Function(_)) | // TODO
            (&Type::String, &Type::String) |
            (&Type::Tuple, &Type::Tuple) |
            (&Type::Any, _) |
            (_, &Type::Any) => true,
            _ => false,
        }
    }
}

impl Eq for Type {}

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

#[derive(Clone)]
struct Context {
    pub in_loop: bool,
    pub in_func: bool,
    // Some(None) represents a non-returning function
    pub func_ret_type: Option<Option<Type>>,
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
    FunctionReturnsMultipleTypes,
    PossibleNoneError(Option<String>),
    UnreachableCodeAfterReturn,
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
            (
                "println",
                FunctionType::NativeVoid(CallSign {
                    num_params: 0,
                    variadic: true,
                }),
            ),
            (
                "assert",
                FunctionType::NativeVoid(CallSign {
                    num_params: 1,
                    variadic: false,
                }),
            ),
            (
                "assert_eq",
                FunctionType::NativeVoid(CallSign {
                    num_params: 2,
                    variadic: false,
                }),
            ),
            (
                "len",
                FunctionType::NativeReturning(CallSign {
                    num_params: 1,
                    variadic: false,
                }),
            ),
            (
                "run_http_server",
                FunctionType::NativeVoid(CallSign {
                    num_params: 1,
                    variadic: false,
                }),
            ),
        ];
        for item in builtin_functions.iter() {
            let (name, ref func) = *item;
            env.declare(
                &name.to_string(),
                &Type::Function(Box::new(Some(func.clone()))),
            );
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

#[derive(Debug)]
pub enum StmtEffect {
    None,
    Return,
}

pub struct TypeChecker {
    context: Context,
    issues: Vec<TypeCheckerIssueWithPosition>,
    env: Rc<RefCell<TypeEnvironment>>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            context: Context::root(),
            issues: Vec::new(),
            env: TypeEnvironment::new_root(),
        }
    }

    pub fn get_issues(&self) -> Vec<TypeCheckerIssueWithPosition> {
        self.issues.clone()
    }

    pub fn check_program(&mut self, ast: &[StmtNode]) {
        self.check_statements(ast);
    }

    pub fn check_statements(&mut self, ast: &[StmtNode]) {
        for statement in ast.iter() {
            self.check_statement(statement);
        }
    }

    pub fn check_statement(&mut self, s: &StmtNode) -> StmtEffect {
        match s.data {
            Stmt::VarDecl(ref variable, ref expr) => {
                self.check_statement_variable_declaration(variable, expr);
                StmtEffect::None
            }
            Stmt::Assign(ref lhs_expr, ref expr) => {
                self.check_statement_assignment(lhs_expr, expr);
                StmtEffect::None
            }
            Stmt::AssignOp(ref lhs_expr, ref op, ref expr) => {
                self.check_statement_assignment_with_op(lhs_expr, op, expr, s);
                StmtEffect::None
            }
            Stmt::Block(ref statements) => {
                let current_env = self.env.clone();
                self.env = TypeEnvironment::create_child(current_env.clone());
                let mut last_effect = StmtEffect::None;
                for stmt in statements.iter() {
                    if let StmtEffect::Return = last_effect {
                        self.issues.push((
                            TypeCheckerIssue::UnreachableCodeAfterReturn,
                            (stmt.pos.0, statements.last().unwrap().pos.1),
                        ));
                        // unreachable code, stop checking
                        break;
                    }
                    last_effect = self.check_statement(stmt);
                }
                self.env = current_env;
                last_effect
            }
            Stmt::Expr(ref expr) => {
                self.check_expr(expr);
                StmtEffect::None
            }
            Stmt::IfThen(ref if_then_stmt) => self.check_statement_if_then_else(s, if_then_stmt),
            Stmt::Loop(ref block) => {
                let old_in_loop_value = self.context.in_loop;
                self.context.in_loop = true;
                let effect = self.check_statement(block);
                self.context.in_loop = old_in_loop_value;
                effect
            }
            Stmt::Break => {
                if self.context.in_loop != true {
                    self.issues
                        .push((RuntimeError::BreakOutsideLoop.into(), s.pos));
                }
                StmtEffect::None
            }
            Stmt::Continue => {
                if self.context.in_loop != true {
                    self.issues
                        .push((RuntimeError::ContinueOutsideLoop.into(), s.pos));
                }
                StmtEffect::None
            }
            Stmt::Empty => StmtEffect::None,
            Stmt::Return(ref possible_expr) => self.check_statement_return(possible_expr, s),
        }
    }

    fn check_expr(&mut self, expr: &ExprNode) -> Option<Type> {
        match expr.data {
            Expr::Literal(ref x) => Some(Type::from(x.data.clone())),
            Expr::Identifier(ref id) => {
                match self.env.borrow().get_type(id) {
                    Some(t) => Some(t),
                    None => {
                        self.issues
                            .push((RuntimeError::ReferenceError(id.clone()).into(), expr.pos));
                        Some(Type::Any)
                    }
                }
            }
            Expr::Tuple(ref elems) => Some(self.check_expr_tuple(elems)),
            Expr::Unary(ref op, ref expr) => Some(self.check_expr_unary_op(op, expr)),
            Expr::UnaryLogical(ref op, ref expr) => {
                Some(self.check_expr_unary_logical_op(op, expr))
            }
            Expr::Binary(ref expr1, ref op, ref expr2) => {
                Some(self.check_expr_binary_expr(expr, expr1, op, expr2))
            }
            Expr::BinaryLogical(ref expr1, ref op, ref expr2) => {
                Some(self.check_expr_binary_logical_expr(expr1, op, expr2))
            }
            Expr::FnCall(ref f_expr, ref args) => self.check_expr_function_call(expr, f_expr, args),
            Expr::FnDef(ref fn_def_expr) => Some(self.check_expr_function_definition(fn_def_expr)),
            Expr::MemberByIdx(ref expr, ref index_expr) => {
                Some(self.check_expr_member_access_by_index(expr, index_expr))
            }
        }
    }


    fn check_statement_variable_declaration(&mut self, variable: &Variable, expr: &ExprNode) {
        let checked_type = self.check_expr_as_value(expr);
        match *variable {
            Variable::Identifier(_, ref id) => {
                self.env.borrow_mut().declare(id, &checked_type);
            }
        };
    }

    fn check_statement_assignment(&mut self, lhs_expr: &LhsExprNode, expr: &ExprNode) {
        let checked_type = self.check_expr_as_value(expr);
        match lhs_expr.data {
            LhsExpr::Identifier(ref id) => {
                if !self.env.borrow_mut().set(id, checked_type) {
                    self.issues.push((
                        RuntimeError::UndeclaredAssignment(id.clone()).into(),
                        lhs_expr.pos,
                    ));
                }
            }
        };
    }

    fn check_statement_assignment_with_op(
        &mut self,
        lhs_expr: &LhsExprNode,
        op: &BinOp,
        expr: &ExprNode,
        stmt: &StmtNode,
    ) {
        let checked_type = self.check_expr_as_value(expr);
        match lhs_expr.data {
            LhsExpr::Identifier(ref id) => {
                let prev_type = match self.env.borrow_mut().get_type(id) {
                    Some(t) => t,
                    None => {
                        self.issues.push((
                            RuntimeError::ReferenceError(id.to_owned()).into(),
                            lhs_expr.pos,
                        ));
                        Type::Any
                    }
                };
                let retval = match *op {
                    BinOp::Add => check_add_for_types(&prev_type, &checked_type),
                    ref op @ BinOp::Sub |
                    ref op @ BinOp::Mul |
                    ref op @ BinOp::Div |
                    ref op @ BinOp::Mod => {
                        check_binary_arithmetic_for_types(op.clone(), &prev_type, &checked_type)
                    }
                    _ => unreachable!(),
                };
                let new_type = match retval {
                    Ok(t) => t,
                    Err(issue) => {
                        self.issues.push((issue, stmt.pos));
                        Type::Any
                    }
                };

                // if id does not exist, then error was reported above
                self.env.borrow_mut().set(id, new_type);
            }
        };
    }

    fn check_statement_if_then_else(
        &mut self,
        statement: &StmtNode,
        if_then_stmt: &IfThenStmt,
    ) -> StmtEffect {
        let &IfThenStmt {
            ref cond,
            ref then_block,
            ref maybe_else_block,
        } = if_then_stmt;

        let else_block = match *maybe_else_block {
            None => {
                StmtNode {
                    data: Stmt::Block(vec![]),
                    pos: (0, 0), // dummy span
                }
            }
            Some(ref block) => *block.clone(),
        };

        self.check_expr_as_value(cond);

        let current_env = self.env.clone();
        let then_env = TypeEnvironment::create_clone(current_env.clone());
        let else_env = TypeEnvironment::create_clone(current_env.clone());

        self.env = then_env;
        let then_effect = self.check_statement(then_block);

        let then_pairs = self.env.borrow().get_all_pairs();

        self.env = else_env;
        let else_effect = self.check_statement(&else_block);

        let else_pairs = self.env.borrow().get_all_pairs();

        self.env = current_env;

        for (then_pair, else_pair) in then_pairs.iter().zip(else_pairs.iter()) {
            let &(ref then_name, ref then_type) = then_pair;
            let &(ref else_name, ref else_type) = else_pair;
            if then_name != else_name {
                panic!("Unexpected behaviour when iterating through environments!");
            }
            if !else_type.is_compatible_with(then_type) {
                self.issues.push((
                    TypeCheckerIssue::MultipleTypesFromBranchWarning(then_name.clone()),
                    statement.pos,
                ));
                self.env.borrow_mut().set(then_name, Type::Any);
            } else {
                self.env.borrow_mut().set(then_name, then_type.clone());
            }
        }

        if let (StmtEffect::Return, StmtEffect::Return) = (then_effect, else_effect) {
            StmtEffect::Return
        } else {
            StmtEffect::None
        }
    }

    fn check_statement_return(
        &mut self,
        possible_expr: &Option<ExprNode>,
        return_statement: &StmtNode,
    ) -> StmtEffect {
        if self.context.in_func != true {
            self.issues.push((
                RuntimeError::ReturnOutsideFunction.into(),
                return_statement.pos,
            ));
            // if the return is outside a function, don't typecheck anything else wrt the return
            return StmtEffect::None;
        }
        match *possible_expr {
            // represents `return foo;`
            Some(ref expr) => {
                let actual_type = self.check_expr_as_value(expr);
                self.context.func_ret_type = match self.context.func_ret_type {
                    None => Some(Some(actual_type)),
                    Some(ref maybe_type /* : Option<Type> */) => {
                        match *maybe_type {
                            // Some(None), non-returning
                            None => {
                                // If the function didn't return a value at some point,
                                // then that's the case we'll stick with because the function
                                // does not *always* return a value.

                                // But we will complain that this time it did return a value.
                                self.issues.push((
                                    TypeCheckerIssue::FunctionReturnsMultipleTypes,
                                    return_statement.pos,
                                ));
                                None
                            }
                            // Some(Some(typ)), returning typ
                            Some(ref typ) => {
                                if !actual_type.is_compatible_with(typ) {
                                    self.issues.push((
                                        TypeCheckerIssue::FunctionReturnsMultipleTypes,
                                        return_statement.pos,
                                    ));
                                    Some(Some(Type::Any))
                                } else {
                                    Some(Some(typ.clone()))
                                }
                            }
                        }
                    }
                };
                StmtEffect::Return
            }
            // represents `return;`
            None => {
                // If the function did return a value previously, then it is returning
                // "multiple types".
                self.issues.push((
                    TypeCheckerIssue::FunctionReturnsMultipleTypes,
                    return_statement.pos,
                ));

                // No matter what the value was before, if it is ever non-returning,
                // we have to remember that
                self.context.func_ret_type = Some(None);
                StmtEffect::Return
            }
        }
    }

    fn check_expr_tuple(&mut self, elems: &[ExprNode]) -> Type {
        for elem_expr in elems {
            self.check_expr_as_value(elem_expr);
        }
        Type::Tuple
    }

    fn check_expr_unary_op(&mut self, op: &UnOp, expr: &ExprNode) -> Type {
        let typ = self.check_expr_as_value(expr);
        match *op {
            UnOp::Neg => {
                match check_unary_minus_for_type(typ) {
                    Ok(t) => t,
                    Err(e) => {
                        self.issues.push((e, expr.pos));
                        Type::Any
                    }
                }
            }
        }
    }

    fn check_expr_unary_logical_op(&mut self, op: &LogicalUnOp, expr: &ExprNode) -> Type {
        self.check_expr_as_value(expr);
        match *op {
            LogicalUnOp::Not => Type::Bool,
        }
    }

    fn check_expr_binary_expr(
        &mut self,
        binary_expr: &ExprNode,
        expr1: &ExprNode,
        op: &BinOp,
        expr2: &ExprNode,
    ) -> Type {
        let checked_type_1 = self.check_expr_as_value(expr1);
        let checked_type_2 = self.check_expr_as_value(expr2);
        use ast::BinOp::*;
        let result = match *op {
            Add => check_add_for_types(&checked_type_1, &checked_type_2),
            ref op @ Sub | ref op @ Mul | ref op @ Div | ref op @ Mod => {
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
                Type::Any
            }
            Ok(t) => t,
        }
    }

    fn check_expr_binary_logical_expr(
        &mut self,
        expr1: &ExprNode,
        op: &LogicalBinOp,
        expr2: &ExprNode,
    ) -> Type {
        match *op {
            LogicalBinOp::And | LogicalBinOp::Or => {
                self.check_expr_as_value(expr1);
                self.check_expr_as_value(expr2);
            }
        }
        Type::Bool
    }


    fn check_expr_function_call(
        &mut self,
        expr: &ExprNode,
        f_expr: &ExprNode,
        args: &[ExprNode],
    ) -> Option<Type> {
        let checked_type = self.check_expr_as_value(f_expr);

        let mut arg_types = Vec::new();
        for arg in args.iter() {
            arg_types.push(self.check_expr_as_value(arg));
        }

        let func_type = match checked_type {
            Type::Function(possible_func) => {
                match *possible_func {
                    None => unreachable!(),
                    Some(func_type) => func_type,
                }
            }
            Type::Any => {
                // Don't know anything about this type. Allow it to be called
                // as func, and then assume the return type is Any.
                return Some(Type::Any);
            }
            v => {
                self.issues.push((
                    RuntimeError::CallToNonFunction(try_get_name_of_fn(f_expr), v).into(),
                    expr.pos,
                ));
                return Some(Type::Any);
            }
        };

        let func_call_sign = func_type.get_call_sign();
        if !func_call_sign.variadic && args.len() != func_type.get_call_sign().num_params {
            self.issues.push((
                RuntimeError::ArgumentLength(try_get_name_of_fn(f_expr)).into(),
                expr.pos,
            ));
            return Some(Type::Any);
        }
        match func_type {
            FunctionType::NativeVoid(_) => None,
            FunctionType::NativeReturning(_) => Some(Type::Any),
            FunctionType::User {
                ref param_names,
                ref body,
                ref env,
                ref already_checked_param_types,
                ..
            } => {
                let function_env = TypeEnvironment::create_child(self.env.clone());
                for (param, arg) in param_names.iter().zip(arg_types.iter()) {
                    function_env.borrow_mut().declare(param, arg);
                }
                let inner_env = TypeEnvironment::create_child(function_env);

                let constraint_types = arg_types.iter().map(|typ| typ.clone().into()).collect();
                if !already_checked_param_types.contains_key(&constraint_types) {
                    if let Some(id) = try_get_name_of_fn(f_expr) {
                        let mut new_checked_param_types = already_checked_param_types.clone();
                        new_checked_param_types.insert(constraint_types, ());
                        let new_func_type = get_function_type_with_updated_already_checked(
                            &func_type,
                            new_checked_param_types,
                        );
                        env.borrow_mut()
                            .set(&id, Type::Function(Box::new(Some(new_func_type))));

                        let old_context = self.context.clone();
                        self.context = Context {
                            in_loop: false,
                            in_func: true,
                            func_ret_type: None,
                        };
                        let mut outer_issues = self.issues.clone();
                        self.issues = Vec::new();
                        let current_env = self.env.clone();
                        self.env = inner_env;
                        let fn_body_effect = self.check_statement(body);
                        self.env = current_env;
                        for inner_issue in &self.issues {
                            outer_issues.push((
                                TypeCheckerIssue::InsideFunctionCall(Box::new(inner_issue.clone())),
                                expr.pos,
                            ));
                        }
                        self.issues = outer_issues;
                        let ret_type;
                        if let StmtEffect::None = fn_body_effect {
                            self.context = old_context;
                            None
                        } else {
                            match self.context.func_ret_type {
                                // non-returning
                                None | Some(None) => {
                                    ret_type = None;
                                }
                                Some(ref typ) => {
                                    ret_type = typ.clone();
                                }
                            }

                            self.context = old_context;
                            ret_type
                        }
                    } else {
                        // TODO
                        // If the function is anonymous, no typechecking is performed,
                        // but probably could and should be.
                        Some(Type::Any)
                    }
                } else {
                    // TODO
                    // If it's been previously checked, remember that and
                    // use the return type
                    Some(Type::Any)
                }
            }
        }
    }

    fn check_expr_function_definition(&mut self, fn_def_expr: &FnDefExpr) -> Type {
        let &FnDefExpr {
            ref maybe_id,
            ref params,
            ref body,
        } = fn_def_expr;
        let func = FunctionType::User {
            call_sign: CallSign {
                num_params: params.len(),
                variadic: false,
            },
            param_names: params.to_vec(),
            body: body.clone(),
            env: self.env.clone(),
            already_checked_param_types: LinearMap::new(),
        };
        let func_type = Type::Function(Box::new(Some(func)));
        if let Some(ref id) = *maybe_id {
            self.env.borrow_mut().declare(id, &func_type);
        }
        func_type
    }

    fn check_expr_member_access_by_index(
        &mut self,
        expr: &ExprNode,
        index_expr: &ExprNode,
    ) -> Type {
        let object_type = self.check_expr_as_value(expr);
        match object_type {
            Type::Tuple | Type::Any => {}
            typ => {
                self.issues.push((
                    RuntimeError::SubscriptOnNonSubscriptable(typ).into(),
                    expr.pos,
                ));
            }
        };
        let typ = self.check_expr_as_value(index_expr);
        match typ {
            Type::Number | Type::Any => {}
            non_integral_type => {
                self.issues.push((
                    RuntimeError::NonIntegralSubscript(non_integral_type).into(),
                    index_expr.pos,
                ));
            }
        };
        Type::Any
    }

    fn check_expr_as_value(&mut self, expr: &ExprNode) -> Type {
        let possible_type = self.check_expr(expr);
        if possible_type.is_none() {
            if let Expr::FnCall(ref f_expr, _) = expr.data {
                if let Expr::Identifier(ref id) = f_expr.data {
                    self.issues.push((
                        TypeCheckerIssue::PossibleNoneError(Some(id.clone())),
                        expr.pos,
                    ));
                } else {
                    self.issues
                        .push((TypeCheckerIssue::PossibleNoneError(None), expr.pos));
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
        _ => Err(
            RuntimeError::BinaryTypeError(BinOp::Add, t1.clone(), t2.clone()).into(),
        ),
    }
}

fn check_binary_arithmetic_for_types(
    op: BinOp,
    t1: &Type,
    t2: &Type,
) -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Number),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(
            RuntimeError::BinaryTypeError(op, t1.clone(), t2.clone()).into(),
        ),
    }
}

fn check_binary_comparison_for_types(
    op: BinOp,
    t1: &Type,
    t2: &Type,
) -> Result<Type, TypeCheckerIssue> {
    match (t1, t2) {
        (&Type::Number, &Type::Number) => Ok(Type::Bool),
        (&Type::Any, _) | (_, &Type::Any) => Ok(Type::Any),
        _ => Err(
            RuntimeError::BinaryTypeError(op, t1.clone(), t2.clone()).into(),
        ),
    }
}

fn try_get_name_of_fn(expr: &ExprNode) -> Option<String> {
    if let Expr::Identifier(ref id) = expr.data {
        Some(id.to_string())
    } else {
        None
    }
}

fn get_function_type_with_updated_already_checked(
    old_fn_type: &FunctionType,
    new_already_checked: LinearMap<Vec<Type>, ()>,
) -> FunctionType {

    if let FunctionType::User {
        ref param_names,
        ref body,
        ref env,
        ref call_sign,
        ..
    } = *old_fn_type
    {
        FunctionType::User {
            param_names: param_names.clone(),
            body: body.clone(),
            env: env.clone(),
            already_checked_param_types: new_already_checked,
            call_sign: call_sign.clone(),
        }
    } else {
        panic!("Not a user function");
    }
}
