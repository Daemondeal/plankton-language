use log::info;

use crate::{PlanktonError, lexer::tokenize, checked_ast::CheckedAst};

pub type FileId = usize;

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

pub struct Compiler {
    sources: Vec<String>
}

pub struct CompilerArgs {
    pub input_sources: Vec<String>,
}

#[derive(Clone, Copy, Debug)]
pub enum CompilerTarget {
    MIPS,
}

impl Compiler {
    pub fn new(args: CompilerArgs) -> Self {
        Self { sources: args.input_sources }
    }

    fn generate_checked_ast(&mut self) -> Result<CheckedAst, PlanktonError> {
        info!(target: "compiler", "Started lexing...");
        let lexed_sources = self
            .sources
            .iter()
            .enumerate()
            .map(|(i, x)| tokenize(x, i as FileId))
            .collect::<Result<Vec<_>, _>>()?;

        
        info!(target: "compiler", "Finished Lexing!");
        for (i, source) in lexed_sources.iter().enumerate() {
            println!("File {}:", i);
            for token in source {
                println!("{:?}", token);
            }
        }

        todo!()
    }

    pub fn compile_target(&mut self, target: CompilerTarget) -> Result<String, PlanktonError> {
        info!(target: "compiler", "Compiling for target {:?}...", target);

        let _checked_ast = self.generate_checked_ast()?;
        
        
            

        todo!()
    }
}
