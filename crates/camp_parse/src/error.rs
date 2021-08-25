use camp_util::{FileId, Span};
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum ParseError {
    #[message = "Expected {1}"]
    ExpectedTokens(#[primary] Span, String),
}
