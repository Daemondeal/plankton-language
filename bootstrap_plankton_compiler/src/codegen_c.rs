use crate::{
    ast::LiteralKind,
    checked_ast::{
        CheckedAst, CheckedExpr, CheckedExprKind, CheckedStmt, CheckedStmtKind, Type, TypeId,
        TYPEID_BOOL, TYPEID_F32, TYPEID_I32, TYPEID_VOID,
    },
    PlanktonError, Res, Span,
};

fn error(message: String, span: Span) -> Res<String> {
    Err(vec![PlanktonError::CodegenError { message, span }])
}

struct CodegenC {
    // ast: CheckedAst,
    type_context: Vec<Type>,
}

impl CodegenC {
    pub fn new(type_context: Vec<Type>) -> Self {
        Self { type_context }
    }

    fn get_type(&self, typ: TypeId) -> &Type {
        &self.type_context[typ.0]
    }

    fn get_type_name(&self, span: Span, typ: TypeId) -> Res<String> {
        let typ = self.get_type(typ);

        Ok(match typ {
            Type::Builtin(id) => {
                if id == &TYPEID_VOID.0 {
                    "void".to_string()
                } else if id == &TYPEID_I32.0 {
                    "int".to_string()
                } else if id == &TYPEID_F32.0 {
                    "float".to_string()
                } else if id == &TYPEID_BOOL.0 {
                    "bool".to_string()
                } else {
                    return error(format!("Type {:?} not implemented", typ), span);
                }
            }

            Type::Procedure(_args, _return_type) => {
                return error("Function pointers not implemented".to_string(), span)
            }
        })
    }

    fn codegen_expr(&mut self, expr: &CheckedExpr) -> Res<String> {
        Ok(match &expr.kind {
            CheckedExprKind::Operation(_, _) => todo!(),
            CheckedExprKind::Grouping(expr) => self.codegen_expr(&expr)?,
            CheckedExprKind::Variable(id) => id.clone(),

            CheckedExprKind::If(_, _, _) => todo!(),
            CheckedExprKind::While(_, _) => todo!(),
            CheckedExprKind::Procedure(_, _, _) => {
                return error(
                    "Procedures not in global scope are not supported in C codegen".to_string(),
                    expr.span,
                )
            }

            // FIXME: We shouldn't rely on rust formatting correctly the numbers here
            CheckedExprKind::Literal(literal_kind) => match literal_kind {
                LiteralKind::String(val) => format!("\"{}\"", val),
                LiteralKind::Integer32(val) => format!("{}", val),
                LiteralKind::Float32(val) => format!("{}", val),
                LiteralKind::Boolean(val) => format!("{}", val),
            },
        })
    }

    fn codegen_stmt(&mut self, statement: &CheckedStmt) -> Res<String> {
        let span = statement.span;

        Ok(match &statement.kind {
            CheckedStmtKind::Declaration(id, typ, init) => {
                format!(
                    "{} {} = {};",
                    &self.get_type_name(span, *typ)?,
                    id,
                    &self.codegen_expr(&init)?
                )
            }
            CheckedStmtKind::Expression(expr) => format!("{};", &self.codegen_expr(&expr)?),
            CheckedStmtKind::Return(expr) => format!("return {};", &self.codegen_expr(&expr)?),
            CheckedStmtKind::Block(stmts) => {
                let mut res = "{\n".to_string();
                for stmt in stmts {
                    res.push_str(&self.codegen_stmt(&stmt)?);
                    res.push('\n');
                }
                res.push('}');

                res
            }
        })
    }

    fn codegen_procedure(
        &mut self,
        identifier: &str,
        arguments: &Vec<(String, TypeId)>,
        return_type: TypeId,
        body: &CheckedStmt,
        span: Span,
    ) -> Res<String> {
        let mut result = format!(
            "{} {}(",
            &self.get_type_name(span, return_type)?,
            identifier,
        );

        for (i, (id, typ)) in arguments.iter().enumerate() {
            result.push_str(&format!("{} {}", &self.get_type_name(span, *typ)?, id));

            // FIXME: deuglyfy this
            if i != arguments.len() - 1 {
                result.push(',');
            }
        }
        result.push(')');

        // FIXME: this could be done more elegantly
        match body.kind {
            CheckedStmtKind::Block(_) => result.push_str(&self.codegen_stmt(body)?),
            _ => {
                result.push('{');
                result.push_str(&self.codegen_stmt(body)?);
                result.push('}');
            }
        }

        Ok(result)
    }

    pub fn codegen(mut self, statements: Vec<CheckedStmt>) -> Res<String> {
        let mut result = "".to_string();

        for statement in statements {
            let body = match &statement.kind {
                CheckedStmtKind::Declaration(identifier, _, initializer) => match &initializer.kind
                {
                    CheckedExprKind::Procedure(args, return_type, body) => self.codegen_procedure(
                        &identifier,
                        args,
                        *return_type,
                        body,
                        statement.span,
                    ),
                    _ => self.codegen_stmt(&statement),
                },

                _ => error(
                    "Only declarations are allowed in global scope in C codegen".to_string(),
                    statement.span,
                ),
            }?;

            result.push_str(&body);
            result.push('\n');
        }

        Ok(result)
    }
}

pub fn codegen_c(ast: CheckedAst) -> Res<String> {
    CodegenC::new(ast.type_context).codegen(ast.statements)
}
