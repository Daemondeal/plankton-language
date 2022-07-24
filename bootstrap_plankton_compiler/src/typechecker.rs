use crate::{
    ast::{Ast, Expr, ExprKind, LiteralKind, Stmt, StmtKind, TypeExpr, TypeExprKind},
    checked_ast::{
        CheckedAst, CheckedExpr, CheckedExprKind, CheckedStmt, CheckedStmtKind, Type, TypeId,
        BUILTIN_TYPES, TYPEID_BOOL, TYPEID_F32, TYPEID_I32, TYPEID_STRING, TYPEID_VOID,
    },
    PlanktonError, Res,
};

#[derive(PartialEq, Eq)]
enum ContextKind {
    Procedure,
    While,
    If,
    Global,
}

struct Context {
    variables: Vec<(String, TypeId)>,
    kind: ContextKind,
}

struct Typechecker {
    type_context: Vec<Type>,
    contexts: Vec<Context>,
}

impl Typechecker {
    fn get_intrinsics() -> Vec<(String, TypeId)> {
        vec![]
    }

    pub fn new() -> Self {
        let mut type_context = vec![];

        for typ in BUILTIN_TYPES {
            type_context.push(typ.clone());
        }

        let contexts = vec![Context {
            variables: Self::get_intrinsics(),
            kind: ContextKind::Global,
        }];

        Self {
            type_context,
            contexts,
        }
    }

    fn push_context(&mut self, kind: ContextKind) {
        self.contexts.push(Context {
            variables: vec![],
            kind,
        });
    }

    fn pop_context(&mut self) {
        let popped = self.contexts.pop();
        assert!(popped.is_some(), "Should always pop some context");
        assert!(
            popped.unwrap().kind != ContextKind::Global,
            "It should never remove the global context",
        );
    }

    fn add_to_context(&mut self, id: String, typ: TypeId) {
        // We should always be in some kind of context
        self.contexts.last_mut().unwrap().variables.push((id, typ));
    }

    fn is_within_context(&self, kind: ContextKind) -> bool {
        self.contexts.iter().any(|c| c.kind == kind)
    }

    fn get_from_context(&self, id: &str) -> Option<TypeId> {
        for context in self.contexts.iter().rev() {
            for (name, typ) in context.variables.iter() {
                if name == id {
                    return Some(*typ);
                }
            }
        }

        None
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        for (i, context_typ) in self.type_context.iter().enumerate() {
            if &typ == context_typ {
                return TypeId(i);
            }
        }

        self.type_context.push(typ);
        TypeId(self.type_context.len() - 1)
    }

    fn get_type(&self, typ: TypeId) -> &Type {
        &self.type_context[typ.0]
    }

    pub fn typecheck(mut self, ast: Ast) -> Res<CheckedAst> {
        let mut checked_statements = vec![];

        // FIXME: This should support multiple errors
        for stmt in ast.statements {
            checked_statements.push(self.typecheck_stmt(stmt)?);
        }

        Ok(CheckedAst {
            statements: checked_statements,
            type_context: self.type_context,
        })
    }

    pub fn parse_type_expr(&mut self, expr: TypeExpr) -> Res<TypeId> {
        match expr.kind {
            TypeExprKind::Builtin(id) => match id.as_str() {
                "str" => Ok(TYPEID_STRING),
                "int" => Ok(TYPEID_I32),
                "float" => Ok(TYPEID_F32),
                "bool" => Ok(TYPEID_BOOL),
                _ => Err(vec![PlanktonError::TypecheckerError {
                    message: "Invalid type".to_string(),
                    span: expr.span,
                }]),
            },

            TypeExprKind::Void => Ok(TYPEID_VOID),

            TypeExprKind::Procedure {
                return_type: _,
                arguments: _,
            } => todo!(),
        }
    }

    pub fn typecheck_stmt(&mut self, stmt: Stmt) -> Res<CheckedStmt> {
        let span = stmt.span;

        let kind = match stmt.kind {
            StmtKind::Declaration(identifier, opt_typ_expr, opt_init) => {
                let mut typ = None;

                if let Some(typ_expr) = opt_typ_expr {
                    typ = Some(self.parse_type_expr(typ_expr)?)
                };

                let initializer = if let Some(init) = opt_init {
                    let checked = self.typecheck_expr(init)?;

                    if let Some(id) = typ {
                        if id != checked.typ {
                            return Err(vec![PlanktonError::TypecheckerError {
                                message: "Types don't match".to_string(),
                                span: stmt.span,
                            }]);
                        }
                    } else {
                        typ = Some(checked.typ);
                    }

                    checked
                } else if let Some(typ) = typ {
                    self.get_type(typ).get_default(stmt.span)
                } else {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message:
                            "Initializations should have either a type descriptor or an initializer"
                                .to_string(),
                        span: stmt.span,
                    }]);
                };

                if let Some(typ) = typ {
                    self.add_to_context(identifier.clone(), typ);
                    CheckedStmtKind::Declaration(identifier, typ, initializer)
                } else {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message:
                            "Initializations should have either a type descriptor or an initializer"
                                .to_string(),
                        span: stmt.span,
                    }]);
                }
            }

            StmtKind::Expression(expr) => CheckedStmtKind::Expression(self.typecheck_expr(expr)?),

            // TODO: Check if inside procedure
            StmtKind::Return(expr) => CheckedStmtKind::Return(self.typecheck_expr(expr)?),

            // TODO: Maybe join multiple errors?
            StmtKind::Block(stmts) => CheckedStmtKind::Block(
                stmts
                    .into_iter()
                    .map(|x| self.typecheck_stmt(x))
                    .collect::<Res<Vec<_>>>()?,
            ),
        };

        Ok(CheckedStmt { kind, span })
    }

    fn typecheck_expr(&mut self, expr: Expr) -> Res<CheckedExpr> {
        let span = expr.span;

        Ok(match expr.kind {
            ExprKind::Operation(_, _) => todo!(),

            ExprKind::Grouping(expr) => {
                let checked = self.typecheck_expr(*expr)?;
                CheckedExpr {
                    span,
                    typ: checked.typ,
                    kind: CheckedExprKind::Grouping(Box::new(checked)),
                }
            }

            ExprKind::Variable(id) => {
                let typ = self.get_from_context(&id);

                if let Some(typ) = typ {
                    CheckedExpr {
                        span,
                        typ,
                        kind: CheckedExprKind::Variable(id),
                    }
                } else {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message: "Variable not found".to_string(),
                        span,
                    }]);
                }
            }

            ExprKind::While(condition, body) => {
                let condition = self.typecheck_expr(*condition)?;
                if condition.typ != TYPEID_BOOL {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message: "While condition must return a boolean value.".to_string(),
                        span: condition.span,
                    }]);
                }

                self.push_context(ContextKind::While);
                let body = self.typecheck_stmt(*body)?;
                self.pop_context();

                CheckedExpr {
                    span,
                    typ: TYPEID_VOID, // TODO: Allow yields inside while loops
                    kind: CheckedExprKind::While(Box::new(condition), Box::new(body)),
                }
            }

            ExprKind::If(condition, then_body, opt_else_body) => {
                let condition = self.typecheck_expr(*condition)?;
                if condition.typ != TYPEID_BOOL {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message: "If condition must return a boolean value.".to_string(),
                        span: condition.span,
                    }]);
                }

                self.push_context(ContextKind::If);
                let then_body = self.typecheck_stmt(*then_body)?;
                self.pop_context();

                let opt_else_body = if let Some(body) = opt_else_body {
                    Some(Box::new(self.typecheck_stmt(*body)?))
                } else {
                    None
                };

                CheckedExpr {
                    span,
                    typ: TYPEID_VOID, // TODO: Allow yields inside if loops
                    kind: CheckedExprKind::If(
                        Box::new(condition),
                        Box::new(then_body),
                        opt_else_body,
                    ),
                }
            }

            ExprKind::Procedure(params, return_type, body) => {
                self.push_context(ContextKind::Procedure);

                let mut variables = vec![];

                for (id, typ_expr) in params {
                    let typ = self.parse_type_expr(typ_expr)?;
                    self.add_to_context(id.clone(), typ);
                    variables.push((id, typ));
                }

                let return_type = self.parse_type_expr(return_type)?;

                let proc_type = Type::Procedure(
                    variables.iter().map(|(_, typ_id)| *typ_id).collect(),
                    return_type,
                );

                let proc_type_id = self.add_type(proc_type);

                let body = self.typecheck_stmt(*body)?;

                self.pop_context();

                CheckedExpr {
                    span,
                    typ: proc_type_id,
                    kind: CheckedExprKind::Procedure(variables, return_type, Box::new(body)),
                }
            }

            ExprKind::Literal(literal_kind) => match literal_kind {
                LiteralKind::String(val) => CheckedExpr {
                    typ: TYPEID_STRING,
                    span,
                    kind: CheckedExprKind::Literal(LiteralKind::String(val)),
                },
                LiteralKind::Integer32(val) => CheckedExpr {
                    typ: TYPEID_I32,
                    span,
                    kind: CheckedExprKind::Literal(LiteralKind::Integer32(val)),
                },
                LiteralKind::Float32(val) => CheckedExpr {
                    typ: TYPEID_F32,
                    span,
                    kind: CheckedExprKind::Literal(LiteralKind::Float32(val)),
                },
                LiteralKind::Boolean(val) => CheckedExpr {
                    typ: TYPEID_BOOL,
                    span,
                    kind: CheckedExprKind::Literal(LiteralKind::Boolean(val)),
                },
            },
        })
    }
}

pub fn typecheck(ast: Ast) -> Res<CheckedAst> {
    Typechecker::new().typecheck(ast)
}
