use ast::*;
use value::*;
use typechecker::Type;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    /// When an undeclared identifier is used on the RHS
    ReferenceError(String),
    /// When an undeclared identifier is assigned to
    UndeclaredAssignment(String),
    /// When a binary op cannot be performed on the given types
    BinaryTypeError(BinOp, Type, Type),
    /// When a unary op cannot be performed on the given type
    UnaryTypeError(UnOp, Type),
    /// When a non-returning function's return value is used
    NoneError(Option<String>),
    /// When a call is made to a non-function value
    CallToNonFunction(Option<String>, Type),
    /// When a subscript access obj[i] is made on a non-subscriptable object
    SubscriptOnNonSubscriptable(Type),
    NonIntegralSubscript(Type),
    IndexOutOfBounds(i64),
    /// When the number of arguments don't match
    ArgumentLength(Option<String>),
    /// When nothing else suits
    GeneralRuntimeError(String),
    /// When a runtime error occurs inside a function call
    /// and is getting propagated as a plain RuntimeError
    InsideFunctionCall(Box<RuntimeErrorWithPosition>),
    BreakOutsideLoop,
    ReturnOutsideFunction,
}

pub type RuntimeErrorWithPosition = (RuntimeError, OffsetSpan);

#[derive(Debug, PartialEq)]
pub enum StmtResult {
    None,
    Break,
    Value(Value),
    Return(Option<Value>),
}

pub trait Interpreter {
    fn run_ast_as_statements(&mut self,
                             statements: &[StmtNode])
                             -> Result<Option<StmtResult>, RuntimeErrorWithPosition>;
    fn run_ast_as_program(&mut self,
                          program: &[StmtNode])
                          -> Result<Option<StmtResult>, RuntimeErrorWithPosition>;
}
