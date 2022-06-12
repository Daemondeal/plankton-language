

pub enum PlanktonError {
    IOError(std::io::Error),
}

impl From<std::io::Error> for PlanktonError {
    fn from(err: std::io::Error) -> Self {
        PlanktonError::IOError(err)
    }
}