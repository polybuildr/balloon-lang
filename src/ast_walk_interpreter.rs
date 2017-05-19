use std::rc::Rc;
use std::cell::RefCell;
use std::usize;

use ast::*;
use value::*;
use operations;
use environment::Environment;
use function::*;
use runtime::*;
use typechecker::Type;

#[derive(Clone)]
struct Context {
    pub in_loop: bool,
    pub in_func: bool,
}

impl Context {
    pub fn root() -> Context {
        Context {
            in_loop: false,
            in_func: false,
        }
    }
}

pub struct AstWalkInterpreter {
    env: Rc<RefCell<Environment>>,
    context: Context,
}

impl AstWalkInterpreter {
    pub fn new() -> AstWalkInterpreter {
        AstWalkInterpreter {
            env: Environment::new_root(),
            context: Context::root(),
        }
    }

    fn with_environment_and_context(env: Rc<RefCell<Environment>>,
                                    context: Context)
                                    -> AstWalkInterpreter {
        AstWalkInterpreter {
            env: env,
            context: context,
        }
    }

    fn interpret_program(&mut self,
                         program: &[StmtNode])
                         -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        let result = self.eval_stmts(program)?;
        Ok(result)
    }

    fn eval_stmts(&mut self,
                  statements: &[StmtNode])
                  -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        let mut last_result = None;
        for statement in statements.iter() {
            last_result = Some(self.eval_stmt(statement)?);
        }
        Ok(last_result)
    }

    fn eval_stmt(&mut self, s: &StmtNode) -> Result<StmtResult, RuntimeErrorWithPosition> {
        match s.data {
            Stmt::VarDecl(ref variable, ref expr) => self.eval_stmt_var_decl(variable, expr),
            Stmt::Assign(ref lhs_expr, ref expr) => self.eval_stmt_assign(lhs_expr, expr),
            Stmt::Block(ref statements) => self.eval_stmt_block(statements),
            Stmt::Expr(ref expr) => {
                let val = self.eval_expr(expr)?;
                match val {
                    None => Ok(StmtResult::None),
                    Some(x) => Ok(StmtResult::Value(x)),
                }
            }
            Stmt::IfThen(ref if_then_stmt) => self.eval_stmt_if_then(if_then_stmt),
            Stmt::Loop(ref block) => self.eval_stmt_loop(block),
            Stmt::Return(ref possible_expr) => self.eval_stmt_return(possible_expr, s),
            Stmt::Break => self.eval_stmt_break(s),
            Stmt::Continue => self.eval_stmt_continue(s),
            Stmt::Empty => Ok(StmtResult::None),
        }
    }

    fn eval_expr_as_value(&mut self, expr: &ExprNode) -> Result<Value, RuntimeErrorWithPosition> {
        let possible_val = self.eval_expr(expr)?;
        if possible_val.is_none() {
            if let Expr::FnCall(ref f_expr, _) = expr.data {
                if let Expr::Identifier(ref id) = f_expr.data {
                    return Err((RuntimeError::NoneError(Some(id.clone())), expr.pos));
                }
                return Err((RuntimeError::NoneError(None), expr.pos));
            } else {
                unreachable!();
            }
        }
        Ok(possible_val.clone().unwrap())
    }

    fn eval_expr(&mut self, e: &ExprNode) -> Result<Option<Value>, RuntimeErrorWithPosition> {
        match e.data {
            Expr::Literal(ref x) => Ok(Some(Value::from(x.data.clone()))),
            Expr::Identifier(ref id) => wrap(self.eval_expr_identifier(id, e)),
            Expr::Tuple(ref elems) => wrap(self.eval_expr_tuple(elems)),
            Expr::Unary(ref op, ref expr) => wrap(self.eval_expr_unary(op, expr, e)),
            Expr::UnaryLogical(ref op, ref expr) => wrap(self.eval_expr_unary_logical(op, expr)),
            Expr::Binary(ref expr1, ref op, ref expr2) => {
                wrap(self.eval_expr_binary(op, expr1, expr2, e))
            }
            Expr::BinaryLogical(ref expr1, ref op, ref expr2) => {
                wrap(self.eval_expr_binary_logical(op, expr1, expr2))
            }
            Expr::MemberByIdx(ref object_expr, ref index_expr) => {
                wrap(self.eval_expr_member_by_idx(object_expr, index_expr, e))
            }
            Expr::FnDef(ref fn_def_expr) => wrap(self.eval_expr_fn_def(fn_def_expr)),
            Expr::FnCall(ref expr, ref args) => self.eval_expr_fn_call(expr, args, e),
        }
    }

    fn eval_stmt_var_decl(&mut self,
                          variable: &Variable,
                          expr: &ExprNode)
                          -> Result<StmtResult, RuntimeErrorWithPosition> {
        let val = self.eval_expr_as_value(expr)?;
        match *variable {
            Variable::Identifier(_, ref name) => {
                self.env.borrow_mut().declare(name, &val);
            }
        };
        Ok(StmtResult::None)
    }

    fn eval_stmt_assign(&mut self,
                        lhs_expr: &LhsExprNode,
                        expr: &ExprNode)
                        -> Result<StmtResult, RuntimeErrorWithPosition> {
        let val = self.eval_expr_as_value(expr)?;
        match lhs_expr.data {
            LhsExpr::Identifier(ref id) => {
                if !self.env.borrow_mut().set(id, val) {
                    return Err((RuntimeError::UndeclaredAssignment(id.clone()), lhs_expr.pos));
                }
            }
        };
        Ok(StmtResult::None)
    }

    fn eval_stmt_block(&mut self,
                       statements: &[StmtNode])
                       -> Result<StmtResult, RuntimeErrorWithPosition> {
        let child_env = Environment::create_child(self.env.clone());
        let mut last_result = Ok(StmtResult::None);
        let current_env = self.env.clone();
        self.env = child_env;
        for statement in statements.iter() {
            last_result = self.eval_stmt(statement);
            if last_result.is_err() || last_result.clone().unwrap().is_block_terminating() {
                break;
            }
        }
        self.env = current_env;
        last_result
    }

    fn eval_stmt_if_then(&mut self,
                         if_then_stmt: &IfThenStmt)
                         -> Result<StmtResult, RuntimeErrorWithPosition> {
        let &IfThenStmt {
                 ref cond,
                 ref then_block,
                 ref maybe_else_block,
             } = if_then_stmt;
        let val = self.eval_expr_as_value(cond)?;
        if val.is_truthy() {
            let result = self.eval_stmt(then_block)?;
            if let StmtResult::Break = result {
                return Ok(StmtResult::Break);
            } else if let StmtResult::Return(_) = result {
                return Ok(result);
            }
        } else if maybe_else_block.is_some() {
            let else_block = maybe_else_block.clone().unwrap();
            let result = self.eval_stmt(&else_block)?;
            if let StmtResult::Break = result {
                return Ok(StmtResult::Break);
            } else if let StmtResult::Return(_) = result {
                return Ok(result);
            }
        }
        Ok(StmtResult::None)
    }

    fn eval_stmt_loop(&mut self, block: &StmtNode) -> Result<StmtResult, RuntimeErrorWithPosition> {
        let old_in_loop = self.context.in_loop;
        self.context.in_loop = true;
        let mut last_result;
        loop {
            last_result = self.eval_stmt(block);
            if last_result.is_err() {
                break;
            }
            match last_result {
                Ok(StmtResult::None) |
                Ok(StmtResult::Value(_)) => {}
                Ok(StmtResult::Break) |
                Ok(StmtResult::Return(_)) |
                Err(_) => {
                    break;
                }
                Ok(StmtResult::Continue) => {
                    continue;
                }
            }
        }
        self.context.in_loop = old_in_loop;
        if let Ok(StmtResult::Break) = last_result {
            Ok(StmtResult::None)
        } else {
            last_result
        }
    }

    fn eval_stmt_return(&mut self,
                        possible_expr: &Option<ExprNode>,
                        return_stmt: &StmtNode)
                        -> Result<StmtResult, RuntimeErrorWithPosition> {
        if !self.context.in_func {
            return Err((RuntimeError::ReturnOutsideFunction, return_stmt.pos));
        }
        match *possible_expr {
            Some(ref expr) => {
                let val = self.eval_expr_as_value(expr)?;
                Ok(StmtResult::Return(Some(val)))
            }
            None => Ok(StmtResult::Return(None)),
        }
    }

    fn eval_stmt_break(&mut self,
                       break_stmt: &StmtNode)
                       -> Result<StmtResult, RuntimeErrorWithPosition> {
        if !self.context.in_loop {
            return Err((RuntimeError::BreakOutsideLoop, break_stmt.pos));
        }
        Ok(StmtResult::Break)
    }

    fn eval_stmt_continue(&mut self,
                          continue_stmt: &StmtNode)
                          -> Result<StmtResult, RuntimeErrorWithPosition> {
        if !self.context.in_loop {
            return Err((RuntimeError::ContinueOutsideLoop, continue_stmt.pos));
        }
        Ok(StmtResult::Continue)
    }

    fn eval_expr_identifier(&mut self,
                            id: &str,
                            id_expr: &ExprNode)
                            -> Result<Value, RuntimeErrorWithPosition> {
        match self.env.borrow_mut().get_value(id) {
            Some(v) => Ok(v),
            None => Err((RuntimeError::ReferenceError(id.to_owned()), id_expr.pos)),
        }
    }

    fn eval_expr_tuple(&mut self, elems: &[ExprNode]) -> Result<Value, RuntimeErrorWithPosition> {
        let mut values = Vec::new();
        for elem_expr in elems {
            let val = self.eval_expr_as_value(elem_expr)?;
            values.push(val);
        }
        Ok(Value::Tuple(values))
    }

    fn eval_expr_unary(&mut self,
                       op: &UnOp,
                       expr: &ExprNode,
                       unary_expr: &ExprNode)
                       -> Result<Value, RuntimeErrorWithPosition> {
        let val = self.eval_expr_as_value(expr)?;
        match *op {
            UnOp::Neg => {
                match operations::unary_minus(val) {
                    Ok(v) => Ok(v),
                    Err(err) => Err((err, unary_expr.pos)),
                }
            }
        }
    }

    fn eval_expr_unary_logical(&mut self,
                               op: &LogicalUnOp,
                               expr: &ExprNode)
                               -> Result<Value, RuntimeErrorWithPosition> {
        let val = self.eval_expr_as_value(expr)?;
        match *op {
            LogicalUnOp::Not => Ok(Value::Bool(!val.is_truthy())),
        }
    }

    fn eval_expr_binary(&mut self,
                        op: &BinOp,
                        expr1: &ExprNode,
                        expr2: &ExprNode,
                        binary_expr: &ExprNode)
                        -> Result<Value, RuntimeErrorWithPosition> {
        let val1 = self.eval_expr_as_value(expr1)?;
        let val2 = self.eval_expr_as_value(expr2)?;
        let retval = match *op {
            BinOp::Add => operations::add(val1, val2),
            BinOp::Sub => operations::subtract(val1, val2),
            BinOp::Mul => operations::multiply(val1, val2),
            BinOp::Div => operations::divide(val1, val2),
            BinOp::Mod => operations::modulo(val1, val2),
            BinOp::Lt => operations::less_than(val1, val2),
            BinOp::Lte => operations::less_than_or_equal(val1, val2),
            BinOp::Gt => operations::greater_than(val1, val2),
            BinOp::Gte => operations::greater_than_or_equal(val1, val2),
            BinOp::Eq => Ok(Value::Bool(val1 == val2)),
        };
        match retval {
            Ok(v) => Ok(v),
            Err(err) => Err((err, binary_expr.pos)),
        }
    }

    fn eval_expr_binary_logical(&mut self,
                                op: &LogicalBinOp,
                                expr1: &ExprNode,
                                expr2: &ExprNode)
                                -> Result<Value, RuntimeErrorWithPosition> {
        match *op {
            LogicalBinOp::And => {
                let val1 = self.eval_expr_as_value(expr1)?;
                if !val1.is_truthy() {
                    return Ok(Value::Bool(false));
                }
                let val2 = self.eval_expr_as_value(expr2)?;
                Ok(Value::Bool(val2.is_truthy()))
            }
            LogicalBinOp::Or => {
                let val1 = self.eval_expr_as_value(expr1)?;
                if val1.is_truthy() {
                    return Ok(Value::Bool(true));
                }
                let val2 = self.eval_expr_as_value(expr2)?;
                Ok(Value::Bool(val2.is_truthy()))
            }
        }
    }

    fn eval_expr_member_by_idx(&mut self,
                               object_expr: &ExprNode,
                               index_expr: &ExprNode,
                               member_access_expr: &ExprNode)
                               -> Result<Value, RuntimeErrorWithPosition> {
        let object = self.eval_expr_as_value(object_expr)?;
        let index = self.eval_expr_as_value(index_expr)?;
        match object {
            Value::Tuple(ref v) => {
                match index {
                    Value::Number(n) => {
                        let idx;
                        if let Number::Float(f) = n {
                            if f.fract() == 0.0 {
                                idx = f.trunc() as i64;
                            } else {
                                return Err((RuntimeError::NonIntegralSubscript(Type::Number),
                                            index_expr.pos));
                            }
                        } else if let Number::Integer(i) = n {
                            idx = i;
                        } else {
                            unreachable!();
                        }
                        if idx < 0 {
                            return Err((RuntimeError::IndexOutOfBounds(idx),
                                        member_access_expr.pos));
                        }
                        match v.get(idx as usize) {
                            Some(x) => Ok(x.clone()),
                            None => {
                                Err((RuntimeError::IndexOutOfBounds(idx), member_access_expr.pos))
                            }
                        }
                    }
                    non_num_index => {
                        Err((RuntimeError::NonIntegralSubscript(non_num_index.get_type()),
                             index_expr.pos))
                    }
                }
            }
            obj => {
                Err((RuntimeError::SubscriptOnNonSubscriptable(obj.get_type()), object_expr.pos))
            }
        }
    }

    fn eval_expr_fn_def(&mut self,
                        fn_def_expr: &FnDefExpr)
                        -> Result<Value, RuntimeErrorWithPosition> {
        let &FnDefExpr {
                 ref maybe_id,
                 ref params,
                 ref body,
             } = fn_def_expr;
        let func = Function::User {
            call_sign: CallSign {
                num_params: params.len(),
                variadic: false,
            },
            param_names: params.clone(),
            body: body.clone(),
            env: self.env.clone(),
        };
        let func_val = Value::Function(Box::new(func));
        if let Some(ref id) = *maybe_id {
            self.env.borrow_mut().declare(id, &func_val);
        }
        Ok(func_val)
    }

    fn eval_expr_fn_call(&mut self,
                         expr: &ExprNode,
                         args: &[ExprNode],
                         fn_call_expr: &ExprNode)
                         -> Result<Option<Value>, RuntimeErrorWithPosition> {
        let val = self.eval_expr_as_value(expr)?;
        let func = match val {
            Value::Function(f) => f,
            v => {
                if let Expr::Identifier(ref id) = expr.data {
                    return Err((RuntimeError::CallToNonFunction(Some(id.clone()), v.get_type()),
                                expr.pos));
                }
                return Err((RuntimeError::CallToNonFunction(None, v.get_type()), expr.pos));
            }
        };
        let mut arg_vals = Vec::new();
        for arg in args.iter() {
            let val = self.eval_expr_as_value(arg)?;
            arg_vals.push(val);
        }

        let call_sign = func.get_call_sign();
        check_args_compat(&arg_vals, &call_sign, expr, fn_call_expr)?;

        let call_func_result = call_func(&func, &arg_vals);
        match call_func_result {
            Ok(possible_val) => Ok(possible_val),
            Err(runtime_error) => Err((runtime_error, fn_call_expr.pos)),
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
        Function::User {
            ref param_names,
            ref body,
            ref env,
            ..
        } => {
            // TODO: returning
            let function_env = Environment::create_child(env.clone());
            for (param, arg) in param_names.iter().zip(arg_vals.iter()) {
                function_env.borrow_mut().declare(param, arg);
            }
            let inner_env = Environment::create_child(function_env);
            let fn_context = Context {
                in_func: true,
                in_loop: false,
            };
            let mut machine = AstWalkInterpreter::with_environment_and_context(inner_env,
                                                                               fn_context);
            let result = machine.eval_stmt(body);
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

fn check_args_compat(arg_vals: &[Value],
                     call_sign: &CallSign,
                     expr: &ExprNode,
                     full_expr: &ExprNode)
                     -> Result<(), RuntimeErrorWithPosition> {
    if !call_sign.variadic && call_sign.num_params != arg_vals.len() {
        if let Expr::Identifier(ref id) = expr.data {
            return Err((RuntimeError::ArgumentLength(Some(id.clone())), full_expr.pos));
        }
        return Err((RuntimeError::ArgumentLength(None), full_expr.pos));
    }
    Ok(())
}

fn wrap(result: Result<Value, RuntimeErrorWithPosition>)
        -> Result<Option<Value>, RuntimeErrorWithPosition> {
    match result {
        Err(err) => Err(err),
        Ok(val) => Ok(Some(val)),
    }
}

impl Interpreter for AstWalkInterpreter {
    fn run_ast_as_statements(&mut self,
                             statements: &[StmtNode])
                             -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        self.eval_stmts(statements)
    }

    fn run_ast_as_program(&mut self,
                          program: &[StmtNode])
                          -> Result<Option<StmtResult>, RuntimeErrorWithPosition> {
        self.interpret_program(program)
    }
}
