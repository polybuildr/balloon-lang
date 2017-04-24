use std::fmt;

use typechecker::ConstraintType;

pub type OffsetSpan = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalBinOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalUnOp {
    Not,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Gte => write!(f, ">="),
            BinOp::Eq => write!(f, "=="),
        }
    }
}

impl fmt::Display for LogicalBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LogicalBinOp::And => write!(f, "and"),
            LogicalBinOp::Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnOp::Neg => write!(f, "-"),
        }
    }
}

impl fmt::Display for LogicalUnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LogicalUnOp::Not => write!(f, "not"),
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
    Binary(Box<ExprNode>, BinOp, Box<ExprNode>),
    BinaryLogical(Box<ExprNode>, LogicalBinOp, Box<ExprNode>),
    Unary(UnOp, Box<ExprNode>),
    UnaryLogical(LogicalUnOp, Box<ExprNode>),
    // optional name, list of params, body, optional return type
    FnDef(Option<String>,
          Vec<(String, Option<ConstraintType>)>,
          Box<StmtNode>,
          Option<ConstraintType>),
    FnCall(Box<ExprNode>, Vec<ExprNode>),
    Tuple(Vec<ExprNode>),
    MemberByIdx(Box<ExprNode>, Box<ExprNode>),
}

// Only for parser convenience
pub enum ExprSuffix {
    ListInParens(Vec<ExprNode>),
    InSquareBrackets(ExprNode),
}

#[derive(Debug, Clone)]
pub struct ExprNode {
    pub pos: OffsetSpan,
    pub data: Expr,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(LhsExprNode, ExprNode),
    VarDecl(Variable, ExprNode),
    Expr(ExprNode),
    Block(Vec<StmtNode>),
    IfThen(ExprNode, Box<StmtNode>),
    IfThenElse(ExprNode, Box<StmtNode>, Box<StmtNode>),
    Loop(Box<StmtNode>),
    Return(Option<ExprNode>),
    Break,
    Empty,
}

#[derive(Debug, Clone)]
pub struct StmtNode {
    pub pos: OffsetSpan,
    pub data: Stmt,
}
