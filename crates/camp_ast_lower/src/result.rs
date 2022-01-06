use camp_ast::{FileId, Span};
use camp_hir::LangItem;
use camp_util::IntoCampError;
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub(crate) enum LoweringError {
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
        #[secondary = "try just using the inner type"]
        inner: Span,
    },

    #[message = "{what} not allowed here"]
    Deny {
        what: &'static str,
        #[primary]
        span: Span,
    },

    #[message = "language item `{1}` is undefined"]
    LangItemNotFound(#[primary] Span, LangItem),

    #[message = "invalid language item `{1}`"]
    InvalidLangItem(#[primary] Span, String),

    #[message = "missing {what} `{name}` in {parent} `{parent_name}`"]
    Missing {
        what: &'static str,
        name: String,
        parent: &'static str,
        parent_name: String,
        #[primary]
        span: Span,
    },

    #[message = "Expected {expected}, found {found}"]
    Unexpected {
        expected: &'static str,
        found: &'static str,
        #[primary = "when resolving this path"]
        span: Span,
    },

    #[message = "Invalid path segment, expected {1}"]
    UnrecognizedPathSegment(#[primary] Span, &'static str),

    #[message = "Unexpected generics provided for {what} `{name}`"]
    UnexpectedGenerics {
        what: &'static str,
        name: String,
        #[primary]
        span: Span,
    },

    #[message = "duplicate generics provided for item"]
    DuplicateGenerics {
        #[primary = "generics first provided here"]
        old: Span,
        #[secondary = "generics provided again here"]
        new: Span,
    },

    #[message = "Unexpected associated item or method following {what}"]
    UnexpectedPathSegment {
        what: &'static str,
        #[primary]
        span: Span,
    },

    #[message = "invalid generic argument ordering, all {this}s must precede any {other}s"]
    MustPrecede {
        other: &'static str,
        #[primary = "this {other} must be placed after ..."]
        other_span: Span,
        this: &'static str,
        #[primary = "... this {this} in the generic arguments"]
        this_span: Span,
    },

    #[message = "mismatched number of {what}: expected {expected}, found {found}"]
    GenericsMismatch {
        what: &'static str,
        found: usize,
        expected: usize,
        #[primary]
        span: Span,
    },

    #[message = "unexpected associated type binding `{binding_name}` on {what} `{name}`"]
    UnexpectedBinding {
        what: &'static str,
        name: String,
        binding_name: String,
        #[primary]
        span: Span,
    },

    #[message = "{1} is not a type name"]
    #[note = "expected a struct or an enum"]
    NotAType(#[primary] Span, &'static str),

    #[message = "lifetime must be named here"]
    MustNameLifetime(#[primary = "add a `'named` lifetime here"] Span),

    #[message = "lifetime `'{name}` not found"]
    MissingLifetime {
        #[primary]
        span: Span,
        name: String,
    },

    #[message = "lifetime can only outlive other lifetime"]
    #[note = "this is a trait, not a lifetime"]
    InvalidLifetimeBound(#[primary] Span),
}

impl IntoCampError for LoweringError {
    type Id = FileId;
}
