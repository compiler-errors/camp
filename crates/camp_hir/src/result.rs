use camp_parse::{FileId, ParseError};
use codespan_derive::IntoDiagnostic;

pub type LoweringResult<T> = std::result::Result<T, LoweringError>;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub enum LoweringError {
    #[render(ParseError::into_diagnostic)]
    ParseError(ParseError),
}

impl From<ParseError> for LoweringError {
    fn from(e: ParseError) -> LoweringError {
        LoweringError::ParseError(e)
    }
}
