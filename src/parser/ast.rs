use codespan_derive::IntoLabel;
use codespan_reporting::diagnostic::{Label, LabelStyle};

use crate::files::FileId;

#[derive(Debug, Copy, Clone)]
pub struct Span(pub FileId, pub usize, pub usize);

impl IntoLabel for Span {
    type FileId = FileId;

    fn into_label(&self, style: LabelStyle) -> Label<Self::FileId> {
        Label::new(style, self.0, self.1..self.2)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    name: String,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct Typename {
    name: String,
    span: Span,
}

#[derive(Debug)]
pub struct Module {
    pub mod_name: Identifier,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug)]
pub enum ModuleItem {
    /// This item corresponds to an unparsed module and will be expanded into a `ModuleItem::Module` before the parser has returned the full AST.
    ModuleDeclaration(Identifier),
    Module(Module),
    Function(Function),
    Struct(Struct),
    Use(Path),
}

#[derive(Debug)]
pub struct Struct {
    pub name: Identifier,
    pub generics: Vec<Typename>,
    pub restrictions: Vec<(Ty, Vec<String>)>,
    pub members: Vec<Member>,
}

#[derive(Debug)]
pub struct Member {
    pub name: Identifier,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Function {
    pub sig: Signature,
    pub body: Spanned<Expression>,
}

#[derive(Debug)]
pub struct Signature {
    pub name: Identifier,
    pub generics: Vec<Typename>,
    pub restrictions: Vec<(Ty, Vec<String>)>,
    /// Marks whether "self" is one of the parameters in `params`. Used to check
    /// whether this is an ill-formed signature (e.g. top-level fn with `self`).
    pub has_self: Option<Span>,
    pub params: Vec<Param>,
}

#[derive(Debug)]
pub struct Param {
    pub name: Identifier,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Ty {
    kind: TyKind,
    span: Span,
}

#[derive(Debug)]
pub enum TyKind {

}

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    
}
