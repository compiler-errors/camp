use camp_files::FileId;
use camp_util::IntoCampError;
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub enum DriverError {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),
}

impl IntoCampError for DriverError {
    type Id = FileId;
}
