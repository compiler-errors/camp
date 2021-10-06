use camp_files::{FileError, FileId, Span};
use camp_parse::ParseError;
use codespan_derive::IntoDiagnostic;

use crate::Visibility;

pub type ResolveResult<T> = std::result::Result<T, ResolveError>;

#[derive(IntoDiagnostic, Debug, Eq, PartialEq, Clone)]
#[file_id(FileId)]
pub enum ResolveError {
    #[message = "No such campsite `{module}`"]
    #[note = "Did you forget a `--lib {module}=/path/to/site/mod.camp` flag?"]
    MissingCampsite {
        #[primary = "Extern campsite `{module}` declared here"]
        span: Span,
        module: String,
    },

    #[message = "An `extern::` path must be followed by a campsite name"]
    ExternNeedsCampsite(#[primary] Span),

    #[message = "Unrecognized path segment, expected identifier"]
    UnrecognizedPathSegment(#[primary] Span),

    #[message = "A glob `::*` must be at the end of a path"]
    StarTrailing(#[primary] Span),

    #[message = "A glob `::*` path cannot be renamed"]
    CannotRenameGlob(#[primary] Span, #[secondary = "Remove this rename"] Span),

    #[message = "Duplicate items declared with name `{0}`"]
    Duplicate(
        String,
        #[primary = "First occurrence is a {2}"] Span,
        &'static str,
        #[primary = "Second occurrence is a {4}"] Span,
        &'static str,
    ),

    #[message = "The module `{module}` has no parent module"]
    NoParent {
        #[primary = "`super` is referenced here"]
        span: Span,
        module: String,
    },

    #[message = "Cannot reference {kind} `{name}`, because it is {visibility}"]
    #[note = "We can only reference items that are {allowed_visibility} in module `{mod_name}`"]
    Visibility {
        name: String,
        mod_name: String,
        kind: &'static str,
        visibility: Visibility,
        allowed_visibility: Visibility,
        span: Span,
    },

    #[message = "No such item `{name}` in module `{module}`"]
    Missing {
        name: String,
        module: String,
        #[primary = "Referenced here"]
        span: Span,
    },

    #[render(FileError::into_diagnostic)]
    FileError(FileError),
    #[render(ParseError::into_diagnostic)]
    ParseError(ParseError),
}

impl From<FileError> for ResolveError {
    fn from(e: FileError) -> Self {
        ResolveError::FileError(e)
    }
}

impl From<ParseError> for ResolveError {
    fn from(e: ParseError) -> Self {
        ResolveError::ParseError(e)
    }
}
