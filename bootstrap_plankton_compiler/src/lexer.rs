use crate::{PlanktonError, token::Token, compiler::FileId};

pub fn tokenize(file: &str, id: FileId) -> Result<Vec<Token>, PlanktonError> {
    println!("{}: {}", id, file);
    todo!()
}
