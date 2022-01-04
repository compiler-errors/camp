use camp_ast::{CampError, FileId, Span};
use camp_util::IntoCampError;
use codespan_derive::IntoDiagnostic;

use crate::Visibility;

pub type UnspannedResolveResult<T> = std::result::Result<T, UnspannedResolveError>;

#[derive(IntoDiagnostic, Debug, Eq, PartialEq, Clone)]
#[file_id(FileId)]
pub(crate) enum ResolveError {
    #[message = "No such campsite `{module}`"]
    #[note = "Did you forget a `--lib {module}=/path/to/site/mod.camp` flag?"]
    MissingCampsite {
        #[primary = "Extern campsite `{module}` declared here"]
        span: Span,
        module: String,
    },

    #[message = "An `extern::` path must be followed by a campsite name"]
    ExternNeedsCampsite(#[primary] Span),

    #[message = "Invalid path segment, expected identifier"]
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
    #[note = "We can only reference items that are at least {allowed_visibility} in module \
              `{mod_name}`"]
    Visibility {
        name: String,
        mod_name: String,
        kind: &'static str,
        visibility: Visibility,
        allowed_visibility: Visibility,
        #[primary]
        span: Span,
    },

    #[message = "No such item `{name}` in module `{module}`"]
    Missing {
        name: String,
        module: String,
        #[primary = "Referenced here"]
        span: Span,
    },

    #[message = "Cannot glob-import from {kind} `{name}`"]
    #[note = "The {kind} `{name}` is not a module or enum"]
    NotAGlob {
        kind: &'static str,
        name: String,
        #[primary]
        span: Span,
    },

    #[message = "Cannot import `{child}` from {kind} `{name}`"]
    #[note = "The {kind} `{name}` is not a module or enum"]
    NotASource {
        kind: &'static str,
        name: String,
        child: String,
        #[primary]
        span: Span,
    },

    #[message = "Expected `{expected}`, found `{found}`"]
    ExpectedFound {
        #[primary]
        span: Span,
        expected: &'static str,
        found: &'static str,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnspannedResolveError {
    Visibility {
        name: String,
        mod_name: String,
        kind: &'static str,
        visibility: Visibility,
        allowed_visibility: Visibility,
    },
    Missing {
        name: String,
        module: String,
    },
    Other(CampError),
}

impl UnspannedResolveError {
    pub fn with_span(self, span: Span) -> CampError {
        match self {
            UnspannedResolveError::Visibility {
                name,
                mod_name,
                kind,
                visibility,
                allowed_visibility,
            } => ResolveError::Visibility {
                name,
                mod_name,
                kind,
                visibility,
                allowed_visibility,
                span,
            }
            .into(),
            UnspannedResolveError::Missing { name, module } => {
                ResolveError::Missing { name, module, span }.into()
            }
            UnspannedResolveError::Other(o) => o,
        }
    }
}

impl IntoCampError for ResolveError {
    type Id = FileId;
}
