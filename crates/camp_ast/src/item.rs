use std::sync::Arc;

pub use camp_lex::tok::Token as RawToken;
use camp_util::{id_type, wrapper_id_type};
use derivative::Derivative;

use crate::{
    punctuated::Punctuated, tok, CampsiteId, Expr, ExprLiteral, Function, Path, Signature, Span,
    Supertraits, TraitTy, Ty, Visibility,
};

id_type!(pub ModId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ModDecl {
    CampsiteRoot(CampsiteId),
    Submod(Arc<SubmodDecl>),
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct SubmodDecl {
    pub id: ItemId,
    pub viz: Visibility,
    pub mod_token: tok::Mod,
    pub name: tok::Ident,
}

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]

pub struct Mod {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ModId,
    pub items: Vec<ModuleItem>,
}

id_type!(pub ItemId);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ItemDecl {
    pub mod_id: ModId,
    pub idx: usize,
}

#[derive(Clone, Hash, PartialEq, Eq, Derivative)]
#[derivative(Debug)]
pub enum ModuleItem {
    #[derivative(Debug = "transparent")]
    Mod(Arc<Mod>),
    #[derivative(Debug = "transparent")]
    Extern(Arc<Extern>),
    #[derivative(Debug = "transparent")]
    Use(Arc<Use>),
    #[derivative(Debug = "transparent")]
    Struct(Arc<Struct>),
    #[derivative(Debug = "transparent")]
    Enum(Arc<Enum>),
    #[derivative(Debug = "transparent")]
    Function(Arc<Function>),
    #[derivative(Debug = "transparent")]
    Trait(Arc<Trait>),
    #[derivative(Debug = "transparent")]
    Impl(Arc<Impl>),
}

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Attribute {
    pub hash_tok: tok::Hash,
    pub lsq_tok: tok::LSq,
    pub inner: AttributeInner,
    pub rsq_tok: tok::RSq,
}

impl Attribute {
    pub fn span(&self) -> Span {
        self.hash_tok.span.until(self.rsq_tok.span)
    }

    pub fn name(&self) -> &str {
        match &self.inner {
            AttributeInner::NameValue { ident, .. } | AttributeInner::List { ident, .. } => {
                &ident.ident
            }
        }
    }
}

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub enum AttributeInner {
    NameValue {
        ident: tok::Ident,
        eq_tok: tok::Eq,
        lit: ExprLiteral,
    },
    List {
        ident: tok::Ident,
        lparen_tok: tok::LParen,
        contents: Vec<RawToken>,
        rparen_tok: tok::RParen,
    },
}

wrapper_id_type!(pub ExternId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Extern {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ExternId,
    pub viz: Visibility,
    pub extern_tok: tok::Extern,
    pub site_tok: tok::Site,
    pub name: tok::Ident,
    pub rename: Option<UseRename>,
    pub semi_tok: tok::Semicolon,
}

wrapper_id_type!(pub UseId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Use {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: UseId,
    pub viz: Visibility,
    pub use_tok: tok::Use,
    pub path: Path,
    pub rename: Option<UseRename>,
    pub semi_tok: tok::Semicolon,
}

impl Use {
    pub fn span(&self) -> Span {
        self.use_tok.span.until(self.semi_tok.span)
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct UseRename {
    pub as_tok: tok::As,
    pub ident: tok::Ident,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericsDecl {
    pub lt_tok: tok::Lt,
    pub generics: Punctuated<GenericDecl, tok::Comma>,
    pub gt_tok: tok::Gt,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum GenericDecl {
    Lifetime(GenericLifetime),
    Ident(GenericType),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericLifetime {
    pub lifetime: tok::Lifetime,
    pub maybe_bounds: Option<Supertraits>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub ident: tok::Ident,
    pub maybe_bounds: Option<Supertraits>,
}

wrapper_id_type!(pub StructId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Struct {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: StructId,
    pub attrs: Vec<Attribute>,
    pub viz: Visibility,
    pub struct_tok: tok::Struct,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub fields: Fields,
    pub where_clause: Option<WhereClause>,
    pub semi_tok: Option<tok::Semicolon>,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Fields {
    #[derivative(Debug = "transparent")]
    Named(FieldsNamed),
    #[derivative(Debug = "transparent")]
    Positional(FieldsPositional),
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<FieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldNamed {
    pub viz: Visibility,
    pub ident: tok::Ident,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<FieldPositional, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldPositional {
    pub viz: Visibility,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub where_tok: tok::Where,
    pub restrictions: Punctuated<TypeRestriction, tok::Comma>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeRestriction {
    pub subject: TyOrLifetime,
    pub trailing_traits: Supertraits,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TyOrLifetime {
    Ty(Ty),
    Lifetime(tok::Lifetime),
}

wrapper_id_type!(pub EnumId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Enum {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: EnumId,
    pub attrs: Vec<Attribute>,
    pub viz: Visibility,
    pub enum_tok: tok::Enum,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub where_clause: Option<WhereClause>,
    pub lcurly_tok: tok::LCurly,
    pub variants: Punctuated<EnumVariant, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub ident: tok::Ident,
    pub fields: Fields,
}

wrapper_id_type!(pub TraitId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Trait {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: TraitId,
    pub attrs: Vec<Attribute>,
    pub viz: Visibility,
    pub fn_tok: tok::Trait,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub supertraits: Option<Supertraits>,
    pub where_clause: Option<WhereClause>,
    pub lcurly_tok: tok::LCurly,
    pub trait_items: Vec<TraitItem>,
    pub rcurly_tok: tok::RCurly,
}

id_type!(pub TraitItemId);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TraitItemDecl {
    pub trait_id: TraitId,
    pub idx: usize,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum TraitItem {
    #[derivative(Debug = "transparent")]
    Fn(Arc<TraitFunction>),
    #[derivative(Debug = "transparent")]
    Type(Arc<TraitType>),
}

wrapper_id_type!(pub TraitFnId => TraitItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct TraitFunction {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: TraitFnId,
    pub signature: Signature,
    pub semi_tok: tok::Semicolon,
}

wrapper_id_type!(pub TraitTypeId => TraitItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct TraitType {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: TraitTypeId,
    pub type_tok: tok::Type,
    pub ident: tok::Ident,
    pub supertraits: Option<Supertraits>,
    pub semi_tok: tok::Semicolon,
}

wrapper_id_type!(pub ImplId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Impl {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ImplId,
    pub attrs: Vec<Attribute>,
    pub impl_tok: tok::Impl,
    pub generics: Option<GenericsDecl>,
    pub impl_trait: Option<ImplTrait>,
    pub ty: Ty,
    pub lcurly_tok: tok::LCurly,
    pub impl_items: Vec<ImplItem>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    pub trait_ty: TraitTy,
    pub for_tok: tok::For,
}

id_type!(pub ImplItemId);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ImplItemDecl {
    pub impl_id: ImplId,
    pub idx: usize,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum ImplItem {
    #[derivative(Debug = "transparent")]
    Function(Arc<ImplFunction>),
    #[derivative(Debug = "transparent")]
    Type(Arc<ImplType>),
}

wrapper_id_type!(pub ImplFnId => ImplItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct ImplFunction {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ImplFnId,
    pub signature: Signature,
    pub body: Expr,
}

wrapper_id_type!(pub ImplTypeId => ImplItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct ImplType {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ImplTypeId,
    pub type_tok: tok::Type,
    pub ident: tok::Ident,
    pub eq_tok: tok::Eq,
    pub ty: Ty,
    pub semi_tok: tok::Semicolon,
}
