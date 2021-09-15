use camp_files::{FileId, Span};
use codespan_derive::IntoDiagnostic;

use crate::tok::TokenDelim;

pub type LexResult<T> = std::result::Result<T, LexError>;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub enum LexError {
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
}
