use camp_util::FileId;
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum DriverError {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),
}
