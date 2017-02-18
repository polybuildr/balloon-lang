use std::collections::HashMap;
use ast::*;

struct Environment {
    symbol_tables: Vec<HashMap<String, i64>>
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

    fn declare(&mut self, variable: &Variable, value: i64) {
        match *variable {
            Variable::Identifier(ref binding, ref id)
            | Variable::IdentifierWithType(ref binding, ref id, _) => {
                // TODO: binding
                self.symbol_tables.last_mut().unwrap().insert(id.clone(), value);
            }
        };
    }

    fn set(&mut self, identifier: &String, value: i64) {
        for table in self.symbol_tables.iter_mut().rev() {
            // TODO: Entry API
            if table.contains_key(identifier) {
                table.insert(identifier.clone(), value);
                return;
            }
        }
        panic!(format!("reference error: '{}' was not declared", identifier));
    }

    fn get_value(&mut self, identifier: &String) -> i64 {
        for table in self.symbol_tables.iter().rev() {
            // TODO: Entry API
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
            env.declare(variable, val);
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
        // TODO: other statements
        _ => {}
    }
}

fn interpret_expr(e: &Expr, env: &mut Environment) -> i64 {
    match *e {
        Expr::Number(x) => x,
        Expr::BinaryExpression(ref expr1, ref op, ref expr2) => {
            let val1 = interpret_expr(expr1, env);
            let val2 = interpret_expr(expr2, env);
            match *op {
                BinaryOp::Add => val1 + val2,
                BinaryOp::Sub => val1 - val2,
                BinaryOp::Mul => val1 * val2,
                BinaryOp::Div => val1 / val2,
            }
        },
        Expr::Identifier(ref id) => env.get_value(&id),
        // TODO: other exprs
        _ => 0
    }
}