use log::info;

use crate::{
    ast::Ast, checked_ast::CheckedAst, codegen_c::codegen_c, lexer::tokenize, parser::parse_tokens,
    typechecker::typecheck, PlanktonError, Res,
};

pub type FileId = usize;

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: FileId, start: usize, end: usize) -> Self {
        Self { file, start, end }
    }
}

#[derive(Clone)]
pub struct Source {
    pub name: String,
    pub content: String,
}

impl Source {
    pub fn new(name: String, content: String) -> Self {
        Self { name, content }
    }
}

pub struct Compiler {
    sources: Vec<Source>,
}

pub struct CompilerArgs {
    pub input_sources: Vec<Source>,
}

#[derive(Clone, Copy, Debug)]
pub enum CompilerTarget {
    C_LANGUAGE,
}

impl Compiler {
    pub fn new(args: CompilerArgs) -> Self {
        Self {
            sources: args.input_sources,
        }
    }

    pub fn get_source(&self, file: FileId) -> Option<&Source> {
        if file < self.sources.len() {
            Some(&self.sources[file])
        } else {
            None
        }
    }

    fn generate_checked_ast(&mut self) -> Result<CheckedAst, Vec<PlanktonError>> {
        info!(target: "compiler", "Started lexing...");
        let lexed_sources = self
            .sources
            .iter()
            .enumerate()
            .map(|(i, x)| tokenize(&x.content, i as FileId))
            .collect::<Result<Vec<_>, Vec<_>>>()?;
        info!(target: "compiler", "Finished Lexing!");

        info!(target: "compiler", "Started Parsing...");
        let mut asts = lexed_sources
            .into_iter()
            .enumerate()
            .map(|(i, source)| parse_tokens(source, i))
            .collect::<Result<Vec<_>, Vec<_>>>()?;
        info!(target: "compiler", "Finished Parsing!");

        // FIXME: There's surely a better way to do this
        let mut combined_statements = vec![];

        for ast in asts.iter_mut() {
            combined_statements.append(&mut ast.statements);
        }

        let combined_ast = Ast {
            statements: combined_statements,
        };

        info!(target: "compiler", "Started Typechecking...");
        let checked_ast = typecheck(combined_ast)?;
        info!(target: "compiler", "Finished Typechecking!");

        Ok(checked_ast)
    }

    pub fn compile_target(&mut self, target: CompilerTarget) -> Result<String, Vec<PlanktonError>> {
        info!(target: "compiler", "Compiling for target {:?}...", target);

        let checked_ast = self.generate_checked_ast()?;

        info!(target: "compiler", "Starting codegen...");
        let code = match target {
            CompilerTarget::C_LANGUAGE => codegen_c(checked_ast)?,
        };

        info!(target: "compiler", "Finished codegen!");

        Ok(code)
    }
}
