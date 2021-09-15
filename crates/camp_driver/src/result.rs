use camp_files::FileId;
use camp_parse::ParseError;
use codespan_derive::IntoDiagnostic;

pub type DriverResult<T> = std::result::Result<T, DriverError>;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum DriverError {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),

    #[render(ParseError::into_diagnostic)]
    ParseError(ParseError),
}

impl From<ParseError> for DriverError {
    fn from(e: ParseError) -> Self {
        DriverError::ParseError(e)
    }
}
