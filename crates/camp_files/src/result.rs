use std::io::{Error as IoError, ErrorKind as IoErrorKind};

use camino::{FromPathBufError, Utf8PathBuf};
use camp_util::IntoCampError;
use codespan_derive::{Diagnostic, IntoDiagnostic};

use crate::FileId;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub(crate) enum FileError {
    #[message = "IO Error: {1}"]
    Io(IoErrorKind, String),
    #[message = "Path is not UTF-8: {0}"]
    NotUtf8(FromPathBufError),
    #[message = "Path does not exist or is not a file: {0}"]
    NotFile(Utf8PathBuf),
    #[message = "Path is not a camp file (incorrect file extension): {0}"]
    NotCampFile(Utf8PathBuf),

    /// Used by codespan
    #[render(unexpected_line_too_large)]
    LineTooLarge { given: usize, max: usize },

    #[message = "Duplicate campsite name provided in arguments"]
    #[note = "First campsite is at {path1}"]
    #[note = "Second campsite is at {path2}"]
    DuplicateCampsite { name: String, path1: Utf8PathBuf, path2: Utf8PathBuf },
}

impl IntoCampError for FileError {
    type Id = FileId;
}

fn unexpected_line_too_large(_: &usize, _: &usize) -> Diagnostic<FileId> {
    panic!("This is an codespan-internal error, and we don't expect to see it ever")
}

impl From<IoError> for FileError {
    fn from(e: IoError) -> Self {
        FileError::Io(e.kind(), e.to_string())
    }
}
