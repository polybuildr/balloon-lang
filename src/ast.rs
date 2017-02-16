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
}

#[derive(Debug, Clone)]
pub enum Variable {
    Identifier(Binding, String),
    IdentifierWithType(Binding, String, String)
}

#[derive(Debug, Clone)]
pub enum Binding {
    Mutable,
    Immutable
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Identifier(String),
    IdentifierWithType(String, String),
    BinaryExpression(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(LhsExpr, Expr),
    VariableDeclaration(Variable, Expr),
    Expression(Expr),
    Block(Vec<Statement>),
}
