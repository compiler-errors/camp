use std::io::{Error as IoError, ErrorKind as IoErrorKind};

use camino::{FromPathBufError, Utf8PathBuf};
use codespan_derive::{Diagnostic, IntoDiagnostic};
use codespan_reporting::files::Error as CodespanError;

use crate::FileId;

pub type FileResult<T> = std::result::Result<T, FileError>;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub enum FileError {
    #[message = "IO Error: {1}"]
    Io(IoErrorKind, String),
    #[message = "Path is not UTF-8: {0}"]
    NotUtf8(FromPathBufError),
    #[message = "Path does not exist or is not a file: {0}"]
    NotFile(Utf8PathBuf),

    /// Used by codespan
    #[render(unexpected_line_too_large)]
    LineTooLarge { given: usize, max: usize },
}

fn unexpected_line_too_large(_: &usize, _: &usize) -> Diagnostic<FileId> {
    panic!("This is an codespan-internal error, and we don't expect to see it ever")
}

impl From<IoError> for FileError {
    fn from(e: IoError) -> Self {
        FileError::Io(e.kind(), e.to_string())
    }
}

impl FileError {
    pub fn into_codespan_error(self) -> CodespanError {
        match self {
            FileError::LineTooLarge { given, max } => CodespanError::LineTooLarge { given, max },
            FileError::Io(kind, msg) => CodespanError::Io(IoError::new(kind, msg)),
            e => CodespanError::Io(std::io::Error::new(IoErrorKind::Other, format!("{:?}", e))),
        }
    }
}
