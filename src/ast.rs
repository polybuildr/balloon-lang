use std::fmt;

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug, Clone)]
pub enum LhsExpr {
    Identifier(String),
    IdentifierWithAnnotation(String, String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Identifier(String),
    IdentifierWithType(String, String),
    BinaryExpression(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(Clone)]
pub enum Statement {
    Assignment(LhsExpr, Expr)
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Assignment (ref s, ref e) => write!(f, "Assignment {{ {:?} = {:?} }}", s, e)
        }
    }
}
