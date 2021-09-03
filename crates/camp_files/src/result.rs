use camino::{FromPathBufError, Utf8PathBuf};
use codespan_derive::IntoDiagnostic;

use crate::{FileId, Span};

pub type Result<T, E = FileError> = std::result::Result<T, E>;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum FileError {
    #[message = "IO Error: {0}"]
    Io(std::io::Error),
    #[message = "Path is not UTF-8: {0}"]
    NotUtf8(FromPathBufError),
    #[message = "Path does not exist or is not a file: {0}"]
    NotFile(Utf8PathBuf),
    #[message = "Module {mod_name} is declared in two different files: {file1} and {file2}"]
    DuplicateModuleFile {
        #[primary = "Module declared here"]
        span: Span,
        mod_name: String,
        file1: Utf8PathBuf,
        file2: Utf8PathBuf,
    },
    #[message = "File cannot be found for module {mod_name}"]
    NoSuchModule {
        #[primary = "Module declared here"]
        span: Span,
        mod_name: String,
    },
}
