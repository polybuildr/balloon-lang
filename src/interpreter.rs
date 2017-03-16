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
}

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

    pub fn interpret_program(&mut self, program: &Vec<Statement>) -> Result<(), InterpreterError> {
        self.env.start_scope();
        self.interpret_statements(program)?;
        self.env.end_scope();
        Ok(())
    }

    pub fn interpret_statements(&mut self,
                                statements: &Vec<Statement>)
                                -> Result<(), InterpreterError> {
        for statement in statements.iter() {
            self.interpret_statement(statement)?;
        }
        Ok(())
    }

    fn interpret_statement(&mut self, s: &Statement) -> Result<StatementEffect, InterpreterError> {
        match *s {
            Statement::VariableDeclaration(ref variable, ref expr) => {
                let val = self.interpret_expr(expr)?;
                self.env.declare(variable, &val);
                Ok(StatementEffect::None)
            }
            Statement::Assignment(ref lhs_expr, ref expr) => {
                let val = self.interpret_expr(expr)?;
                match *lhs_expr {
                    LhsExpr::Identifier(ref id) => {
                        self.env.set(id, val)?;
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
                if val.is_truthy() {
                    if let StatementEffect::Break = self.interpret_statement(then_block)? {
                        return Ok(StatementEffect::Break)
                    }
                }
                return Ok(StatementEffect::None)
            }
            Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
                let val = self.interpret_expr(if_expr)?;
                if val.is_truthy() {
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
            Statement::PrintLn(ref expr) => {
                let val = self.interpret_expr(expr)?;
                println!("{}", val);
                Ok(StatementEffect::None)
            }
            Statement::Empty => Ok(StatementEffect::None),
        }
    }
    fn interpret_expr(&mut self, e: &Expr) -> Result<Value, InterpreterError> {
        match *e {
            Expr::Literal(ref x) => Ok(Value::from(x.clone())),
            Expr::Identifier(ref id) => self.env.get_value(&id),
            Expr::UnaryExpression(ref op, ref expr) => {
                let val = self.interpret_expr(expr)?;
                match *op {
                    UnaryOp::Minus => operations::unary_minus(val),
                }
            }
            Expr::UnaryLogicalExpression(ref op, ref expr) => {
                let val = self.interpret_expr(expr)?;
                match *op {
                    LogicalUnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
                }
            }
            Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
                let val1 = self.interpret_expr(expr1)?;
                let val2 = self.interpret_expr(expr2)?;
                match *op {
                    BinaryOp::Add => Ok(operations::add(val1, val2)?),
                    BinaryOp::Sub => Ok(operations::subtract(val1, val2)?),
                    BinaryOp::Mul => Ok(operations::multiply(val1, val2)?),
                    BinaryOp::Div => Ok(operations::divide(val1, val2)?),
                    BinaryOp::FloorDiv => Ok(operations::floor_divide(val1, val2)?),
                    BinaryOp::LessThan => Ok(operations::less_than(val1, val2)?),
                    BinaryOp::LessThanOrEqual => Ok(operations::less_than_or_equal(val1, val2)?),
                    BinaryOp::GreaterThan => Ok(operations::greater_than(val1, val2)?),
                    BinaryOp::GreaterThanOrEqual => {
                        Ok(operations::greater_than_or_equal(val1, val2)?)
                    }
                    BinaryOp::StrictEquals => Ok(operations::strict_equals(val1, val2)?),
                }
            }
            Expr::BinaryLogicalExpression(ref expr1, ref op, ref expr2) => {
                match *op {
                    LogicalBinaryOp::LogicalAnd => {
                        let val1 = self.interpret_expr(expr1)?;
                        if !val1.is_truthy() {
                            return Ok(Value::Bool(false));
                        }
                        let val2 = self.interpret_expr(expr2)?;
                        Ok(Value::Bool(val2.is_truthy()))
                    }
                    LogicalBinaryOp::LogicalOr => {
                        let val1 = self.interpret_expr(expr1)?;
                        if val1.is_truthy() {
                            return Ok(Value::Bool(true));
                        }
                        let val2 = self.interpret_expr(expr2)?;
                        Ok(Value::Bool(val2.is_truthy()))
                    }
                }
            }
        }
    }
}
