use crate::ast::{Ast, Expr, ExprKind, LiteralKind, Stmt, StmtKind, TypeExpr, TypeExprKind};

pub fn ast_to_sexpr(ast: &Ast) -> String {
    let mut res = "".to_string();

    for stmt in ast.statements.iter() {
        res.push_str(&convert_stmt(stmt, 0));
        res.push('\n');
    }

    res
}

fn convert_type_expr(typ_expr: &TypeExpr) -> String {
    match &typ_expr.kind {
        TypeExprKind::Builtin(name) => name.clone(),
        TypeExprKind::Void => "void".to_string(),
        TypeExprKind::Procedure {
            return_type,
            arguments,
        } => {
            let mut res = "(".to_string();

            // TODO: Fix this
            for (i, arg) in arguments.iter().enumerate() {
                res.push_str(&convert_type_expr(arg));
                if i < arguments.len() - 1 {
                    res.push(' ');
                }
            }

            res.push_str(&format!(") -> {}", convert_type_expr(return_type)));

            res
        }
    }
}

fn add_indent(indent: usize, string: &mut String) {
    for _ in 0..indent {
        string.push(' ');
    }
}

fn convert_stmt(stmt: &Stmt, indent: usize) -> String {
    match &stmt.kind {
        StmtKind::Declaration(name, type_expr, expr) => {
            let mut res = format!("(let {}", name);
            if let Some(typ) = type_expr {
                res.push_str(": ");
                res.push_str(&convert_type_expr(typ));
            }

            if let Some(ex) = expr {
                res.push_str(&format!(" {}", convert_expr(ex, indent)));
            }

            res.push(')');

            res
        }
        StmtKind::Expression(expr) => format!("({})", convert_expr(expr, indent)),
        StmtKind::Return(expr) => format!("(return {})", convert_expr(expr, indent)),
        StmtKind::Block(statements) => {
            let mut res = "(block\n".to_string();

            for statement in statements {
                add_indent(indent + 1, &mut res);
                res.push_str(&convert_stmt(statement, indent + 1));
                res.push('\n');
            }

            add_indent(indent, &mut res);
            res.push(')');
            res
        }
    }
}

fn convert_expr(expr: &Expr, indent: usize) -> String {
    match &expr.kind {
        ExprKind::Operation(operator, operands) => {
            let mut res = format!("({}", operator.get_name());

            for op in operands {
                res.push(' ');
                res.push_str(&convert_expr(op, indent));
            }

            res.push(')');

            res
        }
        ExprKind::Grouping(expr) => convert_expr(expr, indent),
        ExprKind::Variable(name) => name.clone(),
        ExprKind::Literal(kind) => convert_literal(kind),
        ExprKind::If(check, then_body, else_body_opt) => format!(
            "(if {} ({}){})",
            convert_expr(check, indent),
            convert_stmt(then_body, indent),
            if let Some(else_body) = else_body_opt {
                format!(" ({})", convert_stmt(else_body, indent))
            } else {
                "".to_string()
            }
        ),
        ExprKind::While(check, body) => format!(
            "(while {} {})",
            convert_expr(check, indent),
            convert_stmt(body, indent)
        ),
        ExprKind::Procedure(arguments, _return_type, body) => {
            let mut res = "(proc ".to_string();

            for (arg, _) in arguments {
                res.push_str(&format!("{} ", arg));
            }

            res.push_str(&format!("{})", convert_stmt(body, indent)));

            res
        }
    }
}

fn convert_literal(lit: &LiteralKind) -> String {
    match lit {
        LiteralKind::String(val) => val.clone(),
        LiteralKind::Integer32(val) => format!("{}", val),
        LiteralKind::Float32(val) => format!("{}", val),
        LiteralKind::Boolean(val) => format!("{}", val),
    }
}
