use std::fmt;

use typechecker::ConstraintType;

pub type OffsetSpan = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    StrictEquals,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalBinOp {
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalUnaryOp {
    Not,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::LessThan => write!(f, "<"),
            BinOp::LessThanOrEqual => write!(f, "<="),
            BinOp::GreaterThan => write!(f, ">"),
            BinOp::GreaterThanOrEqual => write!(f, ">="),
            BinOp::StrictEquals => write!(f, "=="),
        }
    }
}

impl fmt::Display for LogicalBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LogicalBinOp::LogicalAnd => write!(f, "and"),
            LogicalBinOp::LogicalOr => write!(f, "or"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for LogicalUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LogicalUnaryOp::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum LhsExpr {
    Identifier(String),
}

#[derive(Debug, Clone)]
pub struct LhsExprNode {
    pub pos: OffsetSpan,
    pub data: LhsExpr,
}

#[derive(Debug, Clone)]
pub enum Variable {
    Identifier(BindingType, String),
}

#[derive(Debug, Clone)]
pub enum BindingType {
    Mutable,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryExpression(Box<ExprNode>, BinOp, Box<ExprNode>),
    BinaryLogicalExpression(Box<ExprNode>, LogicalBinOp, Box<ExprNode>),
    UnaryExpression(UnaryOp, Box<ExprNode>),
    UnaryLogicalExpression(LogicalUnaryOp, Box<ExprNode>),
    // optional name, list of params, body, optional return type
    FunctionDefinition(Option<String>,
                       Vec<(String, Option<ConstraintType>)>,
                       Box<StatementNode>,
                       Option<ConstraintType>),
    FunctionCall(Box<ExprNode>, Vec<ExprNode>),
    Tuple(Vec<ExprNode>),
    MemberAccessByIndex(Box<ExprNode>, Box<ExprNode>),
}

// Only for parser convenience
pub enum ExprSuffix {
    ExprListInParens(Vec<ExprNode>),
    ExprInSquareBrackets(ExprNode),
}

#[derive(Debug, Clone)]
pub struct ExprNode {
    pub pos: OffsetSpan,
    pub data: Expr,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(LhsExprNode, ExprNode),
    VariableDeclaration(Variable, ExprNode),
    Expression(ExprNode),
    Block(Vec<StatementNode>),
    IfThen(ExprNode, Box<StatementNode>),
    IfThenElse(ExprNode, Box<StatementNode>, Box<StatementNode>),
    Loop(Box<StatementNode>),
    Return(Option<ExprNode>),
    Break,
    Empty,
}

#[derive(Debug, Clone)]
pub struct StatementNode {
    pub pos: OffsetSpan,
    pub data: Statement,
}
