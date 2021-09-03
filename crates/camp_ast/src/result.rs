use camp_files::{FileId, Span};
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic)]
#[file_id(FileId)]
pub enum AstError {
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
