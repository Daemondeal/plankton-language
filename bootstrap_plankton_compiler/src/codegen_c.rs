use crate::{
    ast::{Ast, Expr, LiteralKind},
    checked_ast::{
        CheckedAst, CheckedExpr, CheckedExprKind, CheckedStmt, CheckedStmtKind, Type, TypeId,
        TYPEID_BOOL, TYPEID_F32, TYPEID_I32, TYPEID_VOID,
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
    Return(String),
    If(String, Box<CBlock>, Option<Box<CBlock>>),
    While(CExpr, Box<CBlock>),
    Block(CBlock),
}

enum CExpr {
    CLiteral(String),
    CVariable(String),
    Assignment(String, Box<CExpr>),
}

struct CAst {
    pub imports: Vec<String>,
    pub functions: Vec<CFunction>,
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
                imports: vec!["stdio".to_string()],
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
        args: &[(String, TypeId)],
        return_type: TypeId,
        body: &CheckedStmt,
    ) -> Res<CFunction> {
        todo!()
    }

    fn codify_statement(&mut self, statement: &CheckedStmt, parent_block: &mut CBlock) -> Res<()> {
        match &statement.kind {
            CheckedStmtKind::Declaration(identifier, typ, initializer) => {
                parent_block.variables.push(CVariable::new(
                    identifier.clone(),
                    self.get_type_name(statement.span, *typ)?,
                ));
                let temp_name = self.codify_expression(&initializer, parent_block)?;

                // FIXME: There must be a better way
                parent_block
                    .body
                    .push(CStatement::Expression(CExpr::Assignment(
                        identifier.clone(),
                        Box::new(CExpr::CVariable(temp_name)),
                    )));
            }

            CheckedStmtKind::Expression(expr) => {
                self.codify_expression(&expr, parent_block)?;
            }

            CheckedStmtKind::Return(expr) => {
                let return_name = self.codify_expression(&expr, parent_block)?;
                parent_block.body.push(CStatement::Return(return_name));
            }
            CheckedStmtKind::Block(body) => {
                if parent_block.body.len() == 0 {
                    for statement in body {
                        self.codify_statement(&statement, parent_block)?;
                    }
                } else {
                    let mut new_block = CBlock::default();
                    for statement in body {
                        self.codify_statement(&statement, &mut new_block)?;
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

        format!("temp_000{}", num)
    }

    fn codify_expression(&mut self, expr: &CheckedExpr, parent_block: &mut CBlock) -> Res<String> {
        match &expr.kind {
            CheckedExprKind::Operation(_, _) => todo!(),
            CheckedExprKind::Grouping(expr) => self.codify_expression(expr, parent_block),
            CheckedExprKind::Variable(id) => Ok(id.clone()),
            CheckedExprKind::Literal(lit) => {
                let name = self.generate_name();

                parent_block.variables.push(CVariable::new(
                    name.clone(),
                    self.get_type_name(expr.span, expr.typ)?,
                ));

                parent_block
                    .body
                    .push(CStatement::Expression(CExpr::Assignment(
                        name.clone(),
                        Box::new(CExpr::literal(lit)),
                    )));

                Ok(name)
            }

            CheckedExprKind::If(condition, then_block, else_block) => {
                let cond_name = self.codify_expression(condition, parent_block)?;

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
                    cond_name,
                    Box::new(c_then_block),
                    c_else_block,
                ));

                Ok("".to_string()) // TODO: Implement yield
            }

            CheckedExprKind::While(condition, block) => todo!(),

            CheckedExprKind::Procedure(_, _, _) => Self::error(
                "Cannot declare a procedure inside another procedure in C codegen".to_string(),
                expr.span,
            ),
        }
    }

    pub fn codify_ast(&mut self, ast: &'a CheckedAst) -> Res<()> {
        for statement in &ast.statements {
            match &statement.kind {
                CheckedStmtKind::Declaration(_, _, initializer) => match &initializer.kind {
                    CheckedExprKind::Procedure(args, return_type, body) => {
                        let proc = self.codify_procedure(args, *return_type, body)?;

                        self.coded_ast.functions.push(proc);
                    }
                    _ => todo!("TODO: Implement global declarations"), //c_globals.push(self.codify_statement(&statement)),
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

struct CodegenC {
    // ast: CheckedAst,
    type_context: Vec<Type>,
}

impl CodegenC {
    pub fn new(type_context: Vec<Type>) -> Self {
        Self { type_context }
    }

    pub fn codegen(type_context: Vec<Type>, c_ast: CAst) -> Res<String> {
        let mut this = Self { type_context };
        this.codegen_ast(c_ast)
    }

    fn codegen_function(&mut self, function: CFunction) -> Res<String> {
        let mut result = format!("{} {}(", function.return_type.0, function.name);

        // TODO: Find a better way
        for (i, var) in function.arguments.iter().enumerate() {
            result.push_str(&format!("{} {}", var.typ.0, var.identifier));

            if i != function.arguments.len() - 1 {
                result.push(',');
            }
        }

        result.push_str(") ");

        Ok(result)
    }

    fn codegen_statement(&mut self, c_statement: CStatement) -> Res<String> {
        match c_statement {
            CStatement::Expression(expr) => Ok(format!("{};\n", self.codegen_expr(expr)?)),
            CStatement::Return(id) => Ok(format!("return {};\n", id)),

            CStatement::If(condition, then_body, else_body) => todo!(),
            CStatement::While(condtion, body) => todo!(),
            CStatement::Block(block) => self.codegen_block(block),
        }
    }

    fn codegen_expr(&mut self, c_expr: CExpr) -> Res<String> {
        match c_expr {
            CExpr::CLiteral(lit) => Ok(lit),
            CExpr::CVariable(name) => Ok(name),
            CExpr::Assignment(id, expr) => Ok(format!("{} = {}", id, self.codegen_expr(*expr)?)),
        }
    }

    fn codegen_block(&mut self, c_block: CBlock) -> Res<String> {
        let mut result = "{\n".to_string();

        for variable in c_block.variables {
            result.push_str(&format!("{} {};\n", variable.typ.0, variable.identifier));
        }

        result.push('\n');

        for statement in c_block.body {
            result.push_str(&self.codegen_statement(statement)?);
        }

        result.push_str("}\n");
        Ok(result)
    }

    fn codegen_ast(&mut self, c_ast: CAst) -> Res<String> {
        let mut result = "".to_string();
        for import in c_ast.imports {
            result.push_str(&format!("#include <{}>\n", import));
        }

        result.push('\n');

        // TODO: Handle global declarations

        for function in c_ast.functions {
            result.push_str(&self.codegen_function(function)?);
            result.push('\n');
        }

        Ok(result)
    }
}

pub fn codegen_c(ast: CheckedAst) -> Res<String> {
    let coded_ast = Codifier::codify(&ast)?;
    CodegenC::codegen(ast.type_context, coded_ast)
}
