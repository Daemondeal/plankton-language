use crate::Span;

#[derive(Debug)]
pub enum PlanktonError {
    IOError(std::io::Error),
    LexerError { message: String, span: Span },
    ParserError { message: String, span: Span },
}

impl From<std::io::Error> for PlanktonError {
    fn from(err: std::io::Error) -> Self {
        PlanktonError::IOError(err)
    }
}