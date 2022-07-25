use crate::{
    ast::{Ast, Expr, ExprKind, LiteralKind, Operator, Stmt, StmtKind, TypeExpr, TypeExprKind},
    checked_ast::{
        CheckedAst, CheckedExpr, CheckedExprKind, CheckedIntrinsic, CheckedStmt, CheckedStmtKind,
        Type, TypeId, TYPEID_BOOL, TYPEID_F32, TYPEID_I32, TYPEID_STRING, TYPEID_VOID,
        TYPES_BUILTIN, TYPES_NUMERIC,
    },
    PlanktonError, Res, Span,
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

    fn error<T>(message: String, span: Span) -> Res<T> {
        Err(vec![PlanktonError::TypecheckerError { message, span }])
    }

    pub fn new() -> Self {
        let mut type_context = vec![];

        for typ in TYPES_BUILTIN {
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

    fn exists_in_latest_context(&mut self, id: &str) -> bool {
        self.contexts
            .last_mut()
            .unwrap()
            .variables
            .iter()
            .any(|(name, _)| name == id)
    }

    #[allow(dead_code)]
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

        // Add to context
        for stmt in &ast.statements {
            match &stmt.kind {
                StmtKind::Declaration(id, _, Some(expr)) => match &expr.kind {
                    ExprKind::Procedure(args, return_type, _) => {
                        let mut checked_args = vec![];
                        for (_, typ) in args {
                            checked_args.push(self.parse_type_expr(typ.clone())?);
                        }

                        let return_type = self.parse_type_expr(return_type.clone())?;

                        let proc_type = self.add_type(Type::Procedure(checked_args, return_type));
                        self.add_to_context(id.clone(), proc_type);
                    }
                    _ => {}
                },
                _ => {}
            }
        }

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

            // TODO: Ensure that type can indeed be pointed to
            TypeExprKind::Pointer(pointed) => {
                let typ = Type::Pointer(self.parse_type_expr(*pointed)?);
                Ok(self.add_type(typ))
            }

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
                    self.get_type(typ).get_default(stmt.span)?
                } else {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message:
                            "Initializations should have either a type descriptor or an initializer"
                                .to_string(),
                        span: stmt.span,
                    }]);
                };

                if let Some(typ) = typ {
                    match self.get_type(typ) {
                        Type::Procedure(_, _) => {
                            if !self.exists_in_latest_context(&identifier) {
                                self.add_to_context(identifier.clone(), typ);
                            }
                        }
                        _ => {
                            self.add_to_context(identifier.clone(), typ);
                        }
                    }

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

    fn get_binary_args(&mut self, mut exprs: Vec<Expr>) -> Res<(CheckedExpr, CheckedExpr)> {
        assert!(
            exprs.len() == 2,
            "Operator has more than two arguments, this is a parser bug"
        );

        let b = exprs.pop().unwrap();
        let a = exprs.pop().unwrap();

        let a = self.typecheck_expr(a)?;
        let b = self.typecheck_expr(b)?;

        Ok((a, b))
    }

    fn get_same_binary_args(
        &mut self,
        exprs: Vec<Expr>,
        span: Span,
    ) -> Res<(CheckedExpr, CheckedExpr)> {
        let (a, b) = self.get_binary_args(exprs)?;

        if a.typ != b.typ {
            return Self::error("Operators should have the same type".to_string(), span);
        }

        Ok((a, b))
    }

    fn get_same_binary_args_with_types(
        &mut self,
        exprs: Vec<Expr>,
        legal_types: &[TypeId],
        span: Span,
    ) -> Res<(CheckedExpr, CheckedExpr)> {
        let (a, b) = self.get_binary_args(exprs)?;

        if a.typ != b.typ {
            return Self::error("Operators should have the same type".to_string(), span);
        } else if !legal_types.contains(&a.typ) {
            return Self::error(format!("Invalid type {:?}", self.get_type(a.typ)), a.span);
        }

        Ok((a, b))
    }

    fn get_unary(&mut self, mut exprs: Vec<Expr>) -> Res<CheckedExpr> {
        Ok(self.typecheck_expr(exprs.pop().unwrap())?)
    }

    fn typecheck_operator(
        &mut self,
        operator: Operator,
        mut exprs: Vec<Expr>,
        span: Span,
    ) -> Res<CheckedExpr> {
        let (typ, kind) = match operator {
            Operator::Sum => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (a.typ, CheckedExprKind::Operation(operator, vec![a, b]))
            }

            Operator::Subtract => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (a.typ, CheckedExprKind::Operation(operator, vec![a, b]))
            }
            Operator::Divide => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (a.typ, CheckedExprKind::Operation(operator, vec![a, b]))
            }
            Operator::Multiply => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (a.typ, CheckedExprKind::Operation(operator, vec![a, b]))
            }
            Operator::LogicAnd => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, &[TYPEID_BOOL], span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::LogicOr => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, &[TYPEID_BOOL], span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::LessThan => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::LessThanEqual => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::GreaterThan => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::GreaterThanEqual => {
                let (a, b) = self.get_same_binary_args_with_types(exprs, TYPES_NUMERIC, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }

            Operator::LogicNot => {
                let rhs = self.get_unary(exprs)?;

                if rhs.typ != TYPEID_BOOL {
                    return Self::error("Only booleans are valid for logic not".to_string(), span);
                }

                (TYPEID_BOOL, CheckedExprKind::Operation(operator, vec![rhs]))
            }
            Operator::Negate => {
                let rhs = self.get_unary(exprs)?;

                (rhs.typ, CheckedExprKind::Operation(operator, vec![rhs]))
            }

            Operator::Equals => {
                let (a, b) = self.get_same_binary_args(exprs, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }
            Operator::NotEquals => {
                let (a, b) = self.get_same_binary_args(exprs, span)?;

                (
                    TYPEID_BOOL,
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }

            Operator::Assign => {
                let (a, b) = self.get_same_binary_args(exprs, span)?;

                (
                    TYPEID_VOID, // Assign with a return type always looks weird
                    CheckedExprKind::Operation(operator, vec![a, b]),
                )
            }

            // TODO: Implement pointers
            Operator::GetAddress => {
                let rhs = self.get_unary(exprs)?;

                let pointer = self.add_type(Type::Pointer(rhs.typ));

                (pointer, CheckedExprKind::Operation(operator, vec![rhs]))
            }
            Operator::Dereference => {
                let rhs = self.get_unary(exprs)?;

                match self.get_type(rhs.typ) {
                    &Type::Pointer(pointed) => {
                        (pointed, CheckedExprKind::Operation(operator, vec![rhs]))
                    }
                    _ => {
                        return Self::error("Can only dereference pointers".to_string(), rhs.span);
                    }
                }
            }

            Operator::Get => todo!(),
            Operator::IndexAccess => todo!(),

            Operator::ProcedureCall => {
                let name = match &exprs[0].kind {
                    ExprKind::Variable(id) => id,
                    _ => unreachable!("First parameter of procedure call is not a variable, this is a parser error,")
                }.clone();

                // Hardcoded intrinsic
                if name == "println" {
                    if exprs.len() != 2 {
                        return Err(vec![PlanktonError::TypecheckerError {
                            message: "Too many arguments for intrinsic println".to_string(),
                            span,
                        }]);
                    }

                    let expr = exprs.swap_remove(1);

                    return self.typecheck_println(expr);
                }

                let mut checked_exprs = vec![];

                for expr in exprs {
                    checked_exprs.push(self.typecheck_expr(expr)?);
                }

                let proc_type = if let Some(typ_id) = self.get_from_context(&name) {
                    self.get_type(typ_id)
                } else {
                    return Err(vec![PlanktonError::TypecheckerError {
                        message: format!("Procedure {} does not exist", name),
                        span,
                    }]);
                };

                match proc_type {
                    Type::Procedure(args, return_type) => {
                        for (arg_typ, expr) in args.iter().zip(checked_exprs.iter().skip(1)) {
                            if *arg_typ != expr.typ {
                                return Err(vec![PlanktonError::TypecheckerError {
                                    message: "Invalid argument type".to_string(),
                                    span: expr.span,
                                }]);
                            }
                        }

                        (
                            *return_type,
                            CheckedExprKind::Operation(Operator::ProcedureCall, checked_exprs),
                        )
                    }
                    _ => {
                        return Err(vec![PlanktonError::TypecheckerError {
                            message: format!("{} is not a procedure", name),
                            span,
                        }])
                    }
                }
            }
        };

        Ok(CheckedExpr { typ, span, kind })
    }

    fn typecheck_println(&mut self, arg: Expr) -> Res<CheckedExpr> {
        let span = arg.span;
        let checked = self.typecheck_expr(arg)?;

        if [TYPEID_I32, TYPEID_F32, TYPEID_BOOL].contains(&checked.typ) {
            let intrinsic = CheckedIntrinsic::Println(checked);

            Ok(CheckedExpr {
                typ: TYPEID_VOID,
                kind: CheckedExprKind::Intrinsic(Box::new(intrinsic)),
                span,
            })
        } else {
            Err(vec![PlanktonError::TypecheckerError {
                message: format!("Invalid type {:?} for print", self.get_type(checked.typ)),
                span,
            }])
        }
    }

    fn typecheck_expr(&mut self, expr: Expr) -> Res<CheckedExpr> {
        let span = expr.span;

        Ok(match expr.kind {
            ExprKind::Operation(op, args) => self.typecheck_operator(op, args, span)?,

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
                        // TODO: Find a better error message
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

                // FIXME: Procedure types are parsed twice
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
