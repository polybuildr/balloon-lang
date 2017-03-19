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

pub enum StatementEffect {
    None,
    Break,
}

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { env: Environment::new() }
    }

    pub fn setup_for_repl(&mut self) {
        self.env.start_scope();
    }

    pub fn cleanup_for_repl(&mut self) {
        self.env.end_scope();
    }

    pub fn interpret_program(&mut self, program: &Vec<StatementNode>) -> Result<(), InterpreterErrorWithPosition> {
        self.env.start_scope();
        self.interpret_statements(program)?;
        self.env.end_scope();
        Ok(())
    }

    pub fn interpret_statements(&mut self,
                                statements: &Vec<StatementNode>)
                                -> Result<(), InterpreterErrorWithPosition> {
        for statement in statements.iter() {
            self.interpret_statement(statement)?;
        }
        Ok(())
    }

    fn interpret_statement(&mut self, s: &StatementNode) -> Result<StatementEffect, InterpreterErrorWithPosition> {
        match s.data {
            Statement::VariableDeclaration(ref variable, ref expr) => {
                let val = self.interpret_expr(expr)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                    }
                }
                self.env.declare(variable, &val.unwrap());
                Ok(StatementEffect::None)
            }
            Statement::Assignment(ref lhs_expr, ref expr) => {
                let val = self.interpret_expr(expr)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                    }
                }
                match lhs_expr.data {
                    LhsExpr::Identifier(ref id) => {
                        if !self.env.set(id, val.unwrap()) {
                            return Err((InterpreterError::UndeclaredAssignment(id.clone()), lhs_expr.pos));
                        }
                    }
                };
                Ok(StatementEffect::None)
            }
            Statement::Block(ref statements) => {
                self.env.start_scope();
                for statement in statements.iter() {
                    if let StatementEffect::Break = self.interpret_statement(statement)? {
                        self.env.end_scope();
                        return Ok(StatementEffect::Break);
                    }
                }
                self.env.end_scope();
                return Ok(StatementEffect::None)
            }
            Statement::Expression(ref expr) => {
                self.interpret_expr(expr)?;
                Ok(StatementEffect::None)
            }
            Statement::IfThen(ref if_expr, ref then_block) => {
                let val = self.interpret_expr(if_expr)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = if_expr.data {
                        return Err((InterpreterError::NoneError(id.clone()), if_expr.pos));
                    }
                }
                if val.unwrap().is_truthy() {
                    if let StatementEffect::Break = self.interpret_statement(then_block)? {
                        return Ok(StatementEffect::Break)
                    }
                }
                return Ok(StatementEffect::None)
            }
            Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
                let val = self.interpret_expr(if_expr)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = if_expr.data {
                        return Err((InterpreterError::NoneError(id.clone()), if_expr.pos));
                    }
                }
                if val.unwrap().is_truthy() {
                    if let StatementEffect::Break = self.interpret_statement(then_block)? {
                        return Ok(StatementEffect::Break)
                    }
                } else {
                    if let StatementEffect::Break = self.interpret_statement(else_block)? {
                        return Ok(StatementEffect::Break)
                    }
                }
                Ok(StatementEffect::None)
            },
            Statement::Loop(ref block) => {
                self.env.start_scope();
                loop {
                    if let StatementEffect::Break = self.interpret_statement(block)? {
                        break;
                    }
                }
                self.env.end_scope();
                Ok(StatementEffect::None)
            }
            Statement::Break => {
                Ok(StatementEffect::Break)
            }
            Statement::Empty => Ok(StatementEffect::None),
        }
    }
    fn interpret_expr(&mut self, e: &ExprNode) -> Result<Option<Value>, InterpreterErrorWithPosition> {
        match e.data {
            Expr::Literal(ref x) => Ok(Some(Value::from(x.clone()))),
            Expr::Identifier(ref id) => {
                match self.env.get_value(&id) {
                    Some(v) => Ok(Some(v)),
                    None => Err((InterpreterError::ReferenceError(id.clone()), e.pos))
                }
            },
            Expr::UnaryExpression(ref op, ref expr) => {
                let val = self.interpret_expr(expr)?;
                if let None = val {
                    if let Expr::FunctionCall(ref id, _) = expr.data {
                        return Err((InterpreterError::NoneError(id.clone()), expr.pos));
                    }
                }
                match *op {
                    UnaryOp::Minus => match operations::unary_minus(val.unwrap()) {
                        Ok(v) => Ok(Some(v)),
                        Err(err) => Err((err, e.pos)),
                    }
                }
            }
            Expr::UnaryLogicalExpression(ref op, ref expr) => {
                let val = self.interpret_expr(expr)?;
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
                let possible_val_1 = self.interpret_expr(expr1)?;
                if let None = possible_val_1 {
                    if let Expr::FunctionCall(ref id, _) = expr1.data {
                        return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                    }
                }
                let possible_val_2 = self.interpret_expr(expr2)?;
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
                    Err(err) => {
                        Err((err, e.pos))
                    }
                }
            }
            Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
                match *op {
                    LogicalBinaryOp::LogicalAnd => {
                        let val1 = self.interpret_expr(expr1)?;
                        if let None = val1 {
                            if let Expr::FunctionCall(ref id, _) = expr1.data {
                                return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                            }
                        }
                        if !val1.unwrap().is_truthy() {
                            return Ok(Some(Value::Bool(false)));
                        }
                        let val2 = self.interpret_expr(expr2)?;
                        if let None = val2 {
                            if let Expr::FunctionCall(ref id, _) = expr2.data {
                                return Err((InterpreterError::NoneError(id.clone()), expr2.pos));
                            }
                        }
                        Ok(Some(Value::Bool(val2.unwrap().is_truthy())))
                    }
                    LogicalBinaryOp::LogicalOr => {
                        let val1 = self.interpret_expr(expr1)?;
                        if let None = val1 {
                            if let Expr::FunctionCall(ref id, _) = expr1.data {
                                return Err((InterpreterError::NoneError(id.clone()), expr1.pos));
                            }
                        }
                        if val1.unwrap().is_truthy() {
                            return Ok(Some(Value::Bool(true)));
                        }
                        let val2 = self.interpret_expr(expr2)?;
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
                use builtins::Callable;
                // check for builtins
                let func = match id.as_ref() {
                    "println" => builtins::PrintLn {},
                    _ => {
                        return Err((InterpreterError::ReferenceError(id.clone()), e.pos));
                    },
                };
                let mut arg_vals = Vec::new();
                for arg in args.iter() {
                    let val = self.interpret_expr(arg)?;
                    if let None = val {
                        if let Expr::FunctionCall(ref id, _) = arg.data {
                            return Err((InterpreterError::NoneError(id.clone()), arg.pos));
                        }
                    }
                    arg_vals.push(val.unwrap());
                }
                match func.call(arg_vals) {
                    Some(v) => Ok(Some(v)),
                    None => Ok(None),
                }
            }
        }
    }
}
