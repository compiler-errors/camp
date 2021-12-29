use camp_parse::{FileId, Span};
use camp_util::IntoCampError;
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub enum LoweringError {
    #[message = "{what} is defined twice with name `{name}`"]
    Duplicate {
        what: &'static str,
        name: String,
        #[primary = "first encountered here"]
        first: Span,
        #[primary = "redefined here"]
        second: Span,
    },

    #[message = "elaborated type not allowed here"]
    DenyElaborated {
        #[primary]
        span: Span,
        #[secondary = "try just using this type"]
        inner: Span,
    },

    #[message = "associated type `{name}` not allowed to be specified here"]
    DenyAssociated {
        #[primary]
        span: Span,
        name: String,
    },

    #[message = "no such language item {1}"]
    NoSuchLangItem(#[primary] Span, String),
}

impl IntoCampError for LoweringError {
    type Id = FileId;
}
