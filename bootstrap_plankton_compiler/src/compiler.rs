use log::info;

use crate::{PlanktonError, lexer::tokenize, checked_ast::CheckedAst};

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
    sources: Vec<Source>
}

pub struct CompilerArgs {
    pub input_sources: Vec<Source>,
}

#[derive(Clone, Copy, Debug)]
pub enum CompilerTarget {
    MIPS,
}

impl Compiler {
    pub fn new(args: CompilerArgs) -> Self {
        Self { sources: args.input_sources }
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

        for (i, source) in lexed_sources.iter().enumerate() {
            for token in source {
                return Err(vec![
                    PlanktonError::ParserError { message: "Test".to_string(), span: token.span }
                ]);
            }
        }

        todo!()
    }

    pub fn compile_target(&mut self, target: CompilerTarget) -> Result<String, Vec<PlanktonError>> {
        info!(target: "compiler", "Compiling for target {:?}...", target);

        let _checked_ast = self.generate_checked_ast()?;
        
        
            

        todo!()
    }
}
