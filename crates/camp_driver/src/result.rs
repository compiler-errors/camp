use camp_files::FileId;
use camp_parse::AstError;
use codespan_derive::IntoDiagnostic;

pub type DriverResult<T> = std::result::Result<T, DriverError>;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum DriverError {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),

    #[render(AstError::into_diagnostic)]
    AstError(AstError),
}

impl From<AstError> for DriverError {
    fn from(e: AstError) -> Self {
        DriverError::AstError(e)
    }
}
