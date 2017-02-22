#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum LhsExpr {
    Identifier(String),
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
    BinaryExpression(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(LhsExpr, Expr),
    VariableDeclaration(Variable, Expr),
    Expression(Expr),
    Block(Vec<Statement>),
    Empty,
}
