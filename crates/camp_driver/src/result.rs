use camp_files::{FileError, FileId};
use camp_import_resolve::ResolveError;
use camp_lex::LexError;
use camp_parse::ParseError;
use codespan_derive::IntoDiagnostic;

pub type DriverResult<T> = std::result::Result<T, DriverError>;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum DriverError {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),

    #[render(FileError::into_diagnostic)]
    FileError(FileError),

    #[render(LexError::into_diagnostic)]
    LexError(LexError),

    #[render(ParseError::into_diagnostic)]
    ParseError(ParseError),

    #[render(ResolveError::into_diagnostic)]
    ResolveError(ResolveError),
}

impl From<FileError> for DriverError {
    fn from(e: FileError) -> Self {
        DriverError::FileError(e)
    }
}

impl From<LexError> for DriverError {
    fn from(e: LexError) -> Self {
        DriverError::LexError(e)
    }
}

impl From<ParseError> for DriverError {
    fn from(e: ParseError) -> Self {
        DriverError::ParseError(e)
    }
}

impl From<ResolveError> for DriverError {
    fn from(e: ResolveError) -> Self {
        DriverError::ResolveError(e)
    }
}
