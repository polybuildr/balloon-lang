use std::collections::HashMap;

use ast::*;
use value::*;
use operations;

struct Environment {
    symbol_tables: Vec<HashMap<String, Value>>
}

impl Environment {
    fn new() -> Environment {
        Environment {
            symbol_tables: Vec::new()
        }
    }

    fn start_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.symbol_tables.pop();
    }

    fn declare(&mut self, variable: &Variable, value: &Value) {
        match *variable {
            Variable::Identifier(_, ref id) => {
                self.symbol_tables.last_mut().unwrap().insert(id.clone(), *value);
            }
        };
    }

    fn set(&mut self, identifier: &String, value: Value) {
        for table in self.symbol_tables.iter_mut().rev() {
            // TODO: Entry API
            if table.contains_key(identifier) {
                table.insert(identifier.clone(), value);
                return;
            }
        }
        panic!(format!("reference error: '{}' was not declared", identifier));
    }

    fn get_value(&mut self, identifier: &String) -> Value {
        for table in self.symbol_tables.iter().rev() {
            if let Some(val) = table.get(identifier) {
                return *val
            }
        }
        panic!(format!("reference error: '{}' was not declared", identifier));
    }
}

pub fn interpret_program(ast: &Vec<Statement>) {
    let mut env = Environment::new();
    env.start_scope();
    for statement in ast.iter() {
        interpret_statement(statement, &mut env);
    }
    env.end_scope();
}

fn interpret_statement(s: &Statement, env: &mut Environment) {
    match *s {
        Statement::VariableDeclaration(ref variable, ref expr) => {
            let val = interpret_expr(expr, env);
            env.declare(variable, &val);
            println!("{:?} => {}", variable, val);
        },
        Statement::Assignment(ref lhs_expr, ref expr) => {
            let val = interpret_expr(expr, env);
            match *lhs_expr {
                LhsExpr::Identifier(ref id) => env.set(id, val)
            };
        },
        Statement::Block(ref statements) => {
            env.start_scope();
            for statement in statements.iter() {
                interpret_statement(statement, env);
            }
            env.end_scope();
        },
        Statement::Expression(ref expr) => {
            let val = interpret_expr(expr, env);
            println!("Expression => {}", val);
        },
        Statement::IfThen(ref if_expr, ref then_block) => {
            let val = interpret_expr(if_expr, env);
            if val.is_truthy() {
                interpret_statement(then_block, env);
            }
        },
        Statement::IfThenElse(ref if_expr, ref then_block, ref else_block) => {
            let val = interpret_expr(if_expr, env);
            if val.is_truthy() {
                interpret_statement(then_block, env);
            } else {
                interpret_statement(else_block, env);
            }
        },
        Statement::Empty => {},
    }
}

fn interpret_expr(e: &Expr, env: &mut Environment) -> Value {
    match *e {
        Expr::Literal(ref x) => Value::from(x.clone()),
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let val1 = interpret_expr(expr1, env);
            let val2 = interpret_expr(expr2, env);
            match *op {
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
            }
        },
        Expr::Identifier(ref id) => env.get_value(&id),
    }
}
