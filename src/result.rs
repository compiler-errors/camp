use camino::{FromPathBufError, Utf8PathBuf};
use codespan_derive::IntoDiagnostic;

use crate::{
    files::FileId,
    lexer::{tok::TokenDelim, Span},
};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(IntoDiagnostic, Debug)]
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

    #[message = "Reached end of the file while parsing block comment"]
    EofInComment(#[primary] Span),
    #[message = "Reached end of the file while parsing character literal"]
    EofInChar(#[primary] Span),
    #[message = "Reached end of the file while parsing character escape"]
    EofInEscape(#[primary] Span),
    #[message = "Reached end of the file while parsing string literal"]
    EofInString(#[primary] Span),
    #[message = "Reached end of the line while parsing string literal"]
    EolInString(#[primary] Span),
    #[message = "Unexpected {1}"]
    UnexpectedDelimiter(#[primary] Span, TokenDelim),
    #[message = "Unmatched {1}"]
    ExpectedDelimiter(
        #[secondary = "Begins here"] Span,
        TokenDelim,
        #[primary = "Insert closing {1} here"] Span,
    ),
    #[message = "Mismatched {1}, found {3} instead"]
    MismatchedDelimiter(
        #[secondary = "Expected to match this {1}"] Span,
        TokenDelim,
        #[primary = "Encountered {3} instead"] Span,
        TokenDelim,
    ),
    #[message = "Invalid numeric literal `{1}`"]
    InvalidNumericLiteral(#[primary] Span, String),
    #[message = "Expected quote `'` symbol to end character literal"]
    ExpectedQuote(#[primary] Span),
    #[message = "Unexpected escape sequence '\\{1}'"]
    UnexpectedEscape(#[primary] Span, char),
    #[message = "Unrecognized symbol `{1}`"]
    UnrecognizedCharacter(#[primary] Span, char),

    #[message = "Expected {1}"]
    ExpectedTokens(#[primary] Span, String),
    #[message = "Visiblity `pub` not expected here"]
    UnexpectedPub(#[primary] Span),
    #[message = "Where clause cannot precede tuple members"]
    ImproperWhere(#[primary] Span),
    #[message = "This type is not a trait, cannot be implemented"]
    NotATrait(#[primary] Span),
    #[message = "This expression is not callable"]
    #[note = "Enclose the expression in parenthesis to make it callable"]
    NotCallable(#[primary] Span),
    #[message = "Let statement is not allowed in expression position"]
    DisallowLet(#[primary] Span),
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
