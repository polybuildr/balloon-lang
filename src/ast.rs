use std::fmt;

pub type OffsetSpan = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub pos: OffsetSpan,
    pub data: T,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
            BinOp::Mod => write!(f, "%"),
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

pub type LiteralNode = Spanned<Literal>;

#[derive(Debug, Clone)]
pub enum LhsExpr {
    Identifier(String),
}

pub type LhsExprNode = Spanned<LhsExpr>;

#[derive(Debug, Clone)]
pub enum Variable {
    Identifier(BindingType, String),
}

#[derive(Debug, Clone)]
pub enum BindingType {
    Mutable,
}

#[derive(Debug, Clone)]
pub struct FnDefExpr {
    pub maybe_id: Option<String>,
    pub params: Vec<String>,
    pub body: Box<StmtNode>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralNode),
    Identifier(String),
    Binary(Box<ExprNode>, BinOp, Box<ExprNode>),
    BinaryLogical(Box<ExprNode>, LogicalBinOp, Box<ExprNode>),
    Unary(UnOp, Box<ExprNode>),
    UnaryLogical(LogicalUnOp, Box<ExprNode>),
    FnDef(FnDefExpr),
    FnCall(Box<ExprNode>, Vec<ExprNode>),
    Tuple(Vec<ExprNode>),
    MemberByIdx(Box<ExprNode>, Box<ExprNode>),
}

// Only for parser convenience
pub enum ExprSuffix {
    ListInParens(Vec<ExprNode>),
    InSquareBrackets(ExprNode),
}

pub type ExprNode = Spanned<Expr>;

#[derive(Debug, Clone)]
pub struct IfThenStmt {
    pub cond: ExprNode,
    pub then_block: Box<StmtNode>,
    pub maybe_else_block: Option<Box<StmtNode>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(LhsExprNode, ExprNode),
    VarDecl(Variable, ExprNode),
    Expr(ExprNode),
    Block(Vec<StmtNode>),
    IfThen(IfThenStmt),
    Loop(Box<StmtNode>),
    Return(Option<ExprNode>),
    Break,
    Continue,
    Empty,
}

pub type StmtNode = Spanned<Stmt>;
