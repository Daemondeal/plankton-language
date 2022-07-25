use crate::{
    ast::LiteralKind,
    checked_ast::{
        CheckedAst, CheckedExpr, CheckedExprKind, CheckedIntrinsic, CheckedStmt, CheckedStmtKind,
        Type, TypeId, TYPEID_BOOL, TYPEID_F32, TYPEID_I32, TYPEID_STRING, TYPEID_VOID,
    },
    PlanktonError, Res, Span,
};

struct CType(String);

struct CVariable {
    pub identifier: String,
    pub typ: CType,
}

impl CVariable {
    pub fn new(identifier: String, typ: CType) -> Self {
        Self { identifier, typ }
    }
}

struct CFunction {
    name: String,
    return_type: CType,
    arguments: Vec<CVariable>,
    body: CBlock,
}

#[derive(Default)]
struct CBlock {
    variables: Vec<CVariable>,
    body: Vec<CStatement>,
}

enum CStatement {
    Expression(CExpr),
    Return(CExpr),
    If(CExpr, Box<CBlock>, Option<Box<CBlock>>),

    Block(CBlock),

    #[allow(dead_code)] // TODO
    While(CExpr, Box<CBlock>),
}

enum CExpr {
    CLiteral(String),
    CVariable(String),
    Assignment(String, Box<CExpr>),
    Printf(String, Box<CExpr>),
}

struct CAst {
    pub imports: Vec<String>,
    pub functions: Vec<CFunction>,

    #[allow(dead_code)]
    pub globals: Vec<CStatement>,
}

impl CExpr {
    pub fn literal(kind: &LiteralKind) -> Self {
        Self::CLiteral(match kind {
            LiteralKind::String(val) => format!("\"{}\"", val),
            LiteralKind::Integer32(val) => format!("{}", val),
            LiteralKind::Float32(val) => format!("{}f", val),
            LiteralKind::Boolean(val) => (if *val { "true" } else { "false" }).to_string(),
        })
    }
}

struct Codifier<'a> {
    type_context: &'a [Type],

    coded_ast: CAst,

    current_var_number: usize,
}

impl<'a> Codifier<'a> {
    fn error<T>(message: String, span: Span) -> Res<T> {
        Err(vec![PlanktonError::CodegenError { message, span }])
    }

    fn get_type(&self, typ: TypeId) -> &Type {
        &self.type_context[typ.0]
    }

    fn get_type_name(&self, span: Span, typ: TypeId) -> Res<CType> {
        let typ = self.get_type(typ);

        let typename = match typ {
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
                    return Self::error(format!("Type {:?} not implemented", typ), span);
                }
            }

            Type::Procedure(_args, _return_type) => {
                return Self::error("Function pointers not implemented".to_string(), span)
            }
        };

        Ok(CType(typename))
    }

    pub fn codify(ast: &'a CheckedAst) -> Res<CAst> {
        let mut codifier = Self {
            type_context: &ast.type_context,
            coded_ast: CAst {
                imports: vec!["stdio.h".to_string(), "stdbool.h".to_string()],
                functions: vec![],
                globals: vec![],
            },
            current_var_number: 0,
        };
        codifier.codify_ast(ast)?;

        Ok(codifier.coded_ast)
    }

    fn codify_procedure(
        &mut self,
        name: &str,
        args: &[(String, TypeId)],
        return_type: TypeId,
        body: &CheckedStmt,
        span: Span,
    ) -> Res<CFunction> {
        let mut c_args = vec![];
        for (id, typ) in args {
            c_args.push(CVariable::new(id.clone(), self.get_type_name(span, *typ)?));
        }

        let return_type = self.get_type_name(span, return_type)?;

        let mut c_body = CBlock::default();
        self.codify_statement(body, &mut c_body)?;

        Ok(CFunction {
            name: name.to_string(),
            return_type,
            arguments: c_args,
            body: c_body,
        })
    }

    fn codify_statement(&mut self, statement: &CheckedStmt, parent_block: &mut CBlock) -> Res<()> {
        match &statement.kind {
            CheckedStmtKind::Declaration(identifier, typ, initializer) => {
                parent_block.variables.push(CVariable::new(
                    identifier.clone(),
                    self.get_type_name(statement.span, *typ)?,
                ));
                let init_expr = self.codify_expression(initializer, parent_block)?;

                // FIXME: There must be a better way
                parent_block
                    .body
                    .push(CStatement::Expression(CExpr::Assignment(
                        identifier.clone(),
                        Box::new(init_expr),
                    )));
            }

            CheckedStmtKind::Expression(expr) => {
                self.codify_expression(expr, parent_block)?;
            }

            CheckedStmtKind::Return(expr) => {
                let return_name = self.codify_expression(expr, parent_block)?;
                parent_block.body.push(CStatement::Return(return_name));
            }
            CheckedStmtKind::Block(body) => {
                if parent_block.body.is_empty() {
                    for statement in body {
                        self.codify_statement(statement, parent_block)?;
                    }
                } else {
                    let mut new_block = CBlock::default();
                    for statement in body {
                        self.codify_statement(statement, &mut new_block)?;
                    }
                    parent_block.body.push(CStatement::Block(new_block))
                }
            }
        };

        Ok(())
    }

    // TODO: this needs to be better
    fn generate_name(&mut self) -> String {
        let num = self.current_var_number;
        self.current_var_number += 1;

        format!("___temp_000{}", num)
    }

    fn codify_intrinsic(
        &mut self,
        intrinsic: &CheckedIntrinsic,
        parent_block: &mut CBlock,
        _span: Span,
    ) -> Res<CExpr> {
        match intrinsic {
            CheckedIntrinsic::Println(expr) => {
                let val = self.codify_expression(expr, parent_block)?;

                let printf_format = if expr.typ == TYPEID_I32 {
                    "%d"
                } else if expr.typ == TYPEID_F32 {
                    "%f"
                } else if expr.typ == TYPEID_STRING {
                    "%s"
                } else if expr.typ == TYPEID_BOOL {
                    // FIXME: This should print either 'true' or 'false', not '0' or '1'
                    "%d"
                } else {
                    unreachable!("Invalid type for print intrinsic. This is a typechecker error")
                };

                parent_block.body.push(CStatement::Expression(CExpr::Printf(
                    printf_format.to_string(),
                    Box::new(val),
                )));

                Ok(CExpr::CVariable("".to_string()))
            }
        }
    }

    fn codify_expression(&mut self, expr: &CheckedExpr, parent_block: &mut CBlock) -> Res<CExpr> {
        match &expr.kind {
            CheckedExprKind::Operation(_, _) => todo!(),
            CheckedExprKind::Grouping(expr) => self.codify_expression(expr, parent_block),
            CheckedExprKind::Variable(id) => Ok(CExpr::CVariable(id.clone())),
            CheckedExprKind::Literal(lit) => {
                Ok(CExpr::literal(lit))
                // let name = self.generate_name();

                // parent_block.variables.push(CVariable::new(
                //     name.clone(),
                //     self.get_type_name(expr.span, expr.typ)?,
                // ));

                // parent_block
                //     .body
                //     .push(CStatement::Expression(CExpr::Assignment(
                //         name.clone(),
                //         Box::new(CExpr::literal(lit)),
                //     )));

                // Ok(name)
            }

            CheckedExprKind::If(condition, then_block, else_block) => {
                let condition = self.codify_expression(condition, parent_block)?;

                let mut c_then_block = CBlock::default();
                self.codify_statement(then_block, &mut c_then_block)?;

                let c_else_block = if let Some(else_block) = else_block {
                    let mut c_else_block = CBlock::default();
                    self.codify_statement(else_block, &mut c_else_block)?;
                    Some(Box::new(c_else_block))
                } else {
                    None
                };

                parent_block.body.push(CStatement::If(
                    condition,
                    Box::new(c_then_block),
                    c_else_block,
                ));

                Ok(CExpr::CVariable("".to_string())) // TODO: Implement yield
            }

            CheckedExprKind::While(_condition, _block) => todo!(),

            CheckedExprKind::Procedure(_, _, _) => Self::error(
                "Cannot declare a procedure inside another procedure in C codegen".to_string(),
                expr.span,
            ),

            CheckedExprKind::Intrinsic(intrinsic) => {
                self.codify_intrinsic(intrinsic, parent_block, expr.span)
            }
        }
    }

    pub fn codify_ast(&mut self, ast: &'a CheckedAst) -> Res<()> {
        for statement in &ast.statements {
            match &statement.kind {
                CheckedStmtKind::Declaration(id, _typ, initializer) => match &initializer.kind {
                    CheckedExprKind::Procedure(args, return_type, body) => {
                        let proc =
                            self.codify_procedure(id, args, *return_type, body, statement.span)?;

                        self.coded_ast.functions.push(proc);
                    }
                    _ => todo!("TODO: Implement global declarations"),
                },

                _ => {
                    return Self::error(
                        "Only declarations and procedures are allowed in global context"
                            .to_string(),
                        statement.span,
                    )
                }
            }
        }

        Ok(())
    }
}

fn codegen_function(function: CFunction) -> String {
    let mut result = format!("{} {}(", function.return_type.0, function.name);

    // TODO: Find a better way
    for (i, var) in function.arguments.iter().enumerate() {
        result.push_str(&format!("{} {}", var.typ.0, var.identifier));

        if i != function.arguments.len() - 1 {
            result.push(',');
        }
    }

    result.push_str(") ");

    result.push_str(&codegen_block(function.body));

    result
}

fn codegen_statement(c_statement: CStatement) -> String {
    match c_statement {
        CStatement::Expression(expr) => format!("{};\n", codegen_expr(expr)),
        CStatement::Return(expr) => format!("return {};\n", codegen_expr(expr)),

        CStatement::If(condition, then_body, else_body) => {
            if let Some(else_body) = else_body {
                format!(
                    "if ({}) {} else {}\n",
                    codegen_expr(condition),
                    codegen_block(*then_body),
                    codegen_block(*else_body)
                )
            } else {
                format!(
                    "if ({}) {}\n",
                    codegen_expr(condition),
                    codegen_block(*then_body)
                )
            }
        }
        CStatement::While(condition, body) => format!(
            "while ({}) {}\n",
            codegen_expr(condition),
            codegen_block(*body)
        ),
        CStatement::Block(block) => codegen_block(block),
    }
}

fn codegen_expr(c_expr: CExpr) -> String {
    match c_expr {
        CExpr::CLiteral(lit) => lit,
        CExpr::CVariable(name) => name,
        CExpr::Assignment(id, expr) => format!("{} = {}", id, codegen_expr(*expr)),
        CExpr::Printf(format, content) => {
            format!("printf(\"{}\\n\", {})", format, codegen_expr(*content))
        }
    }
}

fn codegen_block(c_block: CBlock) -> String {
    let mut result = "{\n".to_string();

    for variable in c_block.variables {
        result.push_str(&format!("{} {};\n", variable.typ.0, variable.identifier));
    }

    result.push('\n');

    for statement in c_block.body {
        result.push_str(&codegen_statement(statement));
    }

    result.push_str("}\n");
    result
}

fn codegen_ast(c_ast: CAst) -> String {
    let mut result = "".to_string();
    for import in c_ast.imports {
        result.push_str(&format!("#include <{}>\n", import));
    }

    result.push('\n');

    // TODO: Handle global declarations

    for function in c_ast.functions {
        result.push_str(&codegen_function(function));
        result.push('\n');
    }

    result
}

pub fn codegen_c(ast: CheckedAst) -> Res<String> {
    let coded_ast = Codifier::codify(&ast)?;
    Ok(codegen_ast(coded_ast))
}
