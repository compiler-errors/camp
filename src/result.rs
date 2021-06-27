use camino::{FromPathBufError, Utf8PathBuf};
use codespan_derive::IntoDiagnostic;

use crate::{files::FileId, parser::Span};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum Error {
    #[message = "Unsupported compiler mode: {0}"]
    UnsupportedMode(&'static str),

    #[message = "IO Error: {0}"]
    Io(std::io::Error),
    #[message = "Path is not UTF-8: {0}"]
    NotUtf8(FromPathBufError),
    #[message = "Path does not exist or is not a file: {0}"]
    NotFile(Utf8PathBuf),
    #[message = "File {0} does not have an enclosing directory"]
    NoParent(Utf8PathBuf),

    #[message = "Could not parse token"]
    InvalidToken(#[primary] Span),

    #[message = "Unrecognized token {token}"]
    ExtraToken {
        token: String,
        #[primary]
        span: Span,
    },

    #[message = "Unrecognized token {token}, expected {expected}"]
    UnrecognizedToken {
        token: String,
        expected: String,
        #[primary]
        span: Span,
    },

    #[message = "Two files provide module \"{mod_name}\": {file1} and {file2}"]
    DuplicateModuleFile {
        file1: Utf8PathBuf,
        file2: Utf8PathBuf,
        mod_name: String,
        #[primary = "Module declared here"]
        span: Span,
    },

    #[message = "There is no file that provides module \"{mod_name}\""]
    NoSuchModule {
        mod_name: String,
        #[primary = "Module declared here"]
        span: Span,
    },
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<FromPathBufError> for Error {
    fn from(e: FromPathBufError) -> Self {
        Error::NotUtf8(e)
    }
}
