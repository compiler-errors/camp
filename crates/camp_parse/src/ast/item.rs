use std::sync::Arc;

use camp_files::{CampsiteId, Span};
use camp_util::{id_type, wrapper_id_type};
use derivative::Derivative;

use super::ReferencePrefix;
use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{
    tok, Expr, ExprContext, GenericsDecl, ParseDb, ParseError, ParseResult, Pat, PathSegment,
    ReturnTy, Supertraits, TraitGenerics, TraitTy, TraitTyPath, Ty, TyPath, Visibility,
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

impl Parse for SubmodDecl {
    type Context = ItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ItemId) -> ParseResult<Self> {
        Ok(SubmodDecl {
            id: ctx,
            viz: input.parse()?,
            mod_token: input.parse()?,
            name: input.parse()?,
        })
    }
}

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]

pub struct Mod {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ModId,
    pub items: Vec<ModuleItem>,
}

impl Mod {
    pub fn mod_ast_from_file(db: &dyn ParseDb, id: ModId) -> ParseResult<Arc<Mod>> {
        let file_id = db.mod_file(id)?;

        let contents = db.open_file(file_id)?;
        let buf = camp_lex::lex(file_id, &contents)?;

        let mut input = ParseBuffer::new(db, &buf);
        let m = input.parse_with(id)?;
        assert!(input.is_empty());

        Ok(m)
    }
}

impl Parse for Mod {
    type Context = ModId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ModId) -> ParseResult<Self> {
        let mut items = vec![];

        while !input.is_empty() {
            let decl = ItemDecl {
                mod_id: ctx,
                idx: items.len(),
            };
            let id = input.db.item_decl(decl);

            items.push(input.parse_with(id)?);
        }

        Ok(Mod { id: ctx, items })
    }
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
    Fn(Arc<Fun>),
    #[derivative(Debug = "transparent")]
    Trait(Arc<Trait>),
    #[derivative(Debug = "transparent")]
    Impl(Arc<Impl>),
}

impl Parse for ModuleItem {
    type Context = ItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ItemId) -> ParseResult<Self> {
        let mut lookahead = input.clone();
        let _viz: Visibility = lookahead.parse()?;

        Ok(if lookahead.peek::<tok::Mod>() {
            let mod_decl = input.parse_with(ctx)?;
            let id = input.db.mod_decl(ModDecl::Submod(mod_decl));

            if input.peek::<tok::LCurly>() {
                let (_, mut contents, rcurly_tok) = input.parse_between_curlys()?;

                // Parse the mod fully
                let m = contents.parse_with(id)?;
                contents.expect_empty(rcurly_tok)?;

                ModuleItem::Mod(m)
            } else if input.peek::<tok::Semicolon>() {
                let _semi: tok::Semicolon = input.parse()?;
                ModuleItem::Mod(input.db.mod_ast_from_file(id)?)
            } else {
                lookahead.error_exhausted()?;
            }
        } else if lookahead.peek::<tok::Extern>() {
            ModuleItem::Extern(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Struct>() {
            ModuleItem::Struct(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Use>() {
            ModuleItem::Use(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Enum>() {
            ModuleItem::Enum(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Fn>() {
            ModuleItem::Fn(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Trait>() {
            ModuleItem::Trait(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Impl>() {
            ModuleItem::Impl(input.parse_with(ctx.into())?)
        } else {
            lookahead.error_exhausted()?;
        })
    }
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

impl Parse for Extern {
    type Context = ExternId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ExternId) -> ParseResult<Self> {
        Ok(Extern {
            id: ctx,
            viz: input.parse()?,
            extern_tok: input.parse()?,
            site_tok: input.parse()?,
            name: input.parse()?,
            rename: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

wrapper_id_type!(pub UseId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Use {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: UseId,
    pub viz: Visibility,
    pub use_tok: tok::Use,
    pub path: Punctuated<PathSegment, tok::ColonColon>,
    pub rename: Option<UseRename>,
    pub semi_tok: tok::Semicolon,
}

impl Parse for Use {
    type Context = UseId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: UseId) -> ParseResult<Self> {
        let viz = input.parse()?;
        let use_tok = input.parse()?;
        let mut path = Punctuated::new();

        path.push(input.parse()?);

        while input.peek::<tok::ColonColon>() {
            path.push_punct(input.parse()?);

            path.push(input.parse()?);
        }

        Ok(Use {
            id: ctx,
            viz,
            use_tok,
            path,
            rename: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

impl Use {
    pub fn span(&self) -> Span {
        self.use_tok.span.until(self.path.last().unwrap().span())
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct UseRename {
    pub as_tok: tok::As,
    pub ident: tok::Ident,
}

impl Parse for UseRename {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(UseRename {
            as_tok: input.parse()?,
            ident: input.parse()?,
        })
    }
}

impl ShouldParse for UseRename {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::As>()
    }
}

wrapper_id_type!(pub StructId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Struct {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: StructId,
    pub viz: Visibility,
    pub struct_tok: tok::Struct,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub fields: Fields,
    pub where_clause: Option<WhereClause>,
    pub semi_tok: Option<tok::Semicolon>,
}

impl Parse for Struct {
    type Context = StructId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: StructId) -> ParseResult<Self> {
        let viz = input.parse()?;
        let struct_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause: Option<WhereClause> = input.parse()?;
        let fields = input.parse()?;

        let (where_clause, semi_tok) = match fields {
            Fields::Named(_) => (where_clause, None),
            Fields::None => (where_clause, Some(input.parse()?)),
            Fields::Positional(_) => {
                if let Some(where_clause) = where_clause {
                    return Err(ParseError::ImproperWhere(where_clause.where_tok.span));
                }
                // Parse optional trailing where clause and semicolon token
                (input.parse()?, Some(input.parse()?))
            },
        };

        Ok(Struct {
            id: ctx,
            viz,
            struct_tok,
            ident,
            generics,
            where_clause,
            fields,
            semi_tok,
        })
    }
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

impl Parse for Fields {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(if input.peek::<tok::LCurly>() {
            Fields::Named(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            Fields::Positional(input.parse()?)
        } else {
            Fields::None
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<FieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for FieldsNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(FieldsNamed {
            lcurly_tok,
            fields: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldNamed {
    pub viz: Visibility,
    pub ident: tok::Ident,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

impl Parse for FieldNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(FieldNamed {
            viz: input.parse()?,
            ident: input.parse()?,
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<FieldPositional, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

impl Parse for FieldsPositional {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(FieldsPositional {
            lparen_tok,
            fields: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldPositional {
    pub viz: Visibility,
    pub ty: Ty,
}

impl Parse for FieldPositional {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(FieldPositional {
            viz: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhereClause {
    pub where_tok: tok::Where,
    pub restrictions: Punctuated<TypeRestriction, tok::Comma>,
}

impl Parse for WhereClause {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let where_tok = input.parse()?;
        let mut restrictions = Punctuated::new();

        loop {
            restrictions.push(input.parse()?);

            if input.peek::<tok::Semicolon>() || input.peek::<tok::RCurly>() {
                break;
            }

            restrictions.push_punct(input.parse()?);

            if input.peek::<tok::Semicolon>() || input.peek::<tok::RCurly>() {
                break;
            }
        }

        Ok(WhereClause {
            where_tok,
            restrictions,
        })
    }
}

impl ShouldParse for WhereClause {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Where>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeRestriction {
    pub subject: TyOrLifetime,
    pub trailing_traits: Supertraits,
}

impl Parse for TypeRestriction {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(TypeRestriction {
            subject: input.parse()?,
            trailing_traits: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TyOrLifetime {
    Ty(Ty),
    Lifetime(tok::Lifetime),
}

impl Parse for TyOrLifetime {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            TyOrLifetime::Lifetime(input.parse()?)
        } else {
            TyOrLifetime::Ty(input.parse()?)
        })
    }
}

wrapper_id_type!(pub EnumId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Enum {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: EnumId,
    pub viz: Visibility,
    pub enum_tok: tok::Enum,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub where_clause: Option<WhereClause>,
    pub lcurly_tok: tok::LCurly,
    pub variants: Punctuated<EnumVariant, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for Enum {
    type Context = EnumId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: EnumId) -> ParseResult<Self> {
        let viz = input.parse()?;
        let enum_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(Enum {
            id: ctx,
            viz,
            enum_tok,
            ident,
            generics,
            where_clause,
            lcurly_tok,
            variants: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub ident: tok::Ident,
    pub fields: Fields,
}

impl Parse for EnumVariant {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(EnumVariant {
            ident: input.parse()?,
            fields: input.parse()?,
        })
    }
}

wrapper_id_type!(pub FnId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Fun {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: FnId,
    pub sig: Signature,
    pub body: Expr,
}

impl Parse for Fun {
    type Context = FnId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: FnId) -> ParseResult<Self> {
        Ok(Fun {
            id: ctx,
            sig: input.parse()?,
            body: Expr::expr_block(input, ExprContext::any_expr())?,
        })
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    pub viz: Visibility,
    pub fn_tok: tok::Fn,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub where_clause: Option<WhereClause>,
    pub lparen_tok: tok::LParen,
    pub parameters: Punctuated<Parameter, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

impl Parse for Signature {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let viz = input.parse()?;
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause = input.parse()?;
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Signature {
            viz,
            fn_tok,
            ident,
            generics,
            where_clause,
            lparen_tok,
            parameters: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
            return_ty: input.parse()?,
        })
    }
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Parameter {
    #[derivative(Debug = "transparent")]
    Named(ParameterNamed),
    #[derivative(Debug = "transparent")]
    LSelf(ParameterSelf),
    #[derivative(Debug = "transparent")]
    SelfRef(ParameterSelfRef),
}

impl Parse for Parameter {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        if input.peek::<tok::Mut>() || input.peek::<tok::LSelf>() {
            let mut fork = input.clone();
            if let Ok(param) = fork.parse::<ParameterSelf>() {
                *input = fork;
                return Ok(Parameter::LSelf(param));
            }
        }

        if input.peek::<tok::Amp>() {
            let mut fork = input.clone();
            if let Ok(param) = fork.parse::<ParameterSelfRef>() {
                *input = fork;
                return Ok(Parameter::SelfRef(param));
            }
        }

        Ok(Parameter::Named(input.parse()?))
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterNamed {
    pub pat: Pat,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

impl Parse for ParameterNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(ParameterNamed {
            pat: input.parse()?,
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelf {
    pub mut_tok: Option<tok::Mut>,
    pub self_tok: tok::LSelf,
}

impl Parse for ParameterSelf {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(ParameterSelf {
            mut_tok: input.parse()?,
            self_tok: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelfRef {
    pub prefix: ReferencePrefix,
    pub self_tok: tok::LSelf,
}

impl Parse for ParameterSelfRef {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(ParameterSelfRef {
            prefix: input.parse()?,
            self_tok: input.parse()?,
        })
    }
}

wrapper_id_type!(pub TraitId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Trait {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: TraitId,
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

impl Parse for Trait {
    type Context = TraitId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: TraitId) -> ParseResult<Self> {
        let viz = input.parse()?;
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let supertraits = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut trait_items = vec![];

        while !contents.is_empty() {
            let decl = TraitItemDecl {
                trait_id: ctx,
                idx: trait_items.len(),
            };
            let id = input.db.trait_decl(decl);

            trait_items.push(contents.parse_with(id)?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Trait {
            id: ctx,
            viz,
            fn_tok,
            ident,
            generics,
            supertraits,
            where_clause,
            lcurly_tok,
            trait_items,
            rcurly_tok,
        })
    }
}

id_type!(pub TraitItemId);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TraitItemDecl {
    trait_id: TraitId,
    idx: usize,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum TraitItem {
    #[derivative(Debug = "transparent")]
    Fn(Arc<TraitFn>),
    #[derivative(Debug = "transparent")]
    Type(Arc<TraitType>),
}

impl Parse for TraitItem {
    type Context = TraitItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: TraitItemId) -> ParseResult<Self> {
        Visibility::do_not_expect(input)?;

        Ok(if input.peek::<tok::Fn>() {
            TraitItem::Fn(input.parse_with(ctx.into())?)
        } else if input.peek::<tok::Type>() {
            TraitItem::Type(input.parse_with(ctx.into())?)
        } else {
            input.error_exhausted()?;
        })
    }
}

wrapper_id_type!(pub TraitFnId => TraitItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct TraitFn {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: TraitFnId,
    pub signature: Signature,
    pub semi_tok: tok::Semicolon,
}

impl Parse for TraitFn {
    type Context = TraitFnId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: TraitFnId) -> ParseResult<Self> {
        Ok(TraitFn {
            id: ctx,
            signature: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
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

impl Parse for TraitType {
    type Context = TraitTypeId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: TraitTypeId) -> ParseResult<Self> {
        Ok(TraitType {
            id: ctx,
            type_tok: input.parse()?,
            ident: input.parse()?,
            supertraits: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

wrapper_id_type!(pub ImplId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Impl {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ImplId,
    pub impl_tok: tok::Impl,
    pub generics: Option<GenericsDecl>,
    pub impl_trait: Option<ImplTrait>,
    pub ty: Ty,
    pub lcurly_tok: tok::LCurly,
    pub impl_items: Vec<ImplItem>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for Impl {
    type Context = ImplId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ImplId) -> ParseResult<Self> {
        Visibility::do_not_expect(input)?;

        let impl_tok = input.parse()?;
        let generics = input.parse()?;

        let ty_or_trait: Ty = input.parse()?;

        let impl_trait;
        let ty;

        if let Some(for_tok) = input.parse()? {
            match ty_or_trait {
                Ty::Path(TyPath { path, generics }) => {
                    impl_trait = Some(ImplTrait {
                        trait_ty: TraitTy::Path(TraitTyPath {
                            path,
                            generics: generics.map(TraitGenerics::from),
                        }),
                        for_tok,
                    });
                    ty = input.parse()?;
                },
                ty => return Err(ParseError::NotATrait(ty.span())),
            }
        } else {
            impl_trait = None;
            ty = ty_or_trait;
        }

        let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut impl_items = vec![];

        while !contents.is_empty() {
            let decl = ImplItemDecl {
                impl_id: ctx,
                idx: impl_items.len(),
            };
            let id = input.db.impl_decl(decl);

            impl_items.push(contents.parse_with(id)?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Impl {
            id: ctx,
            impl_tok,
            generics,
            impl_trait,
            ty,
            lcurly_tok,
            impl_items,
            rcurly_tok,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    trait_ty: TraitTy,
    for_tok: tok::For,
}

id_type!(pub ImplItemId);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ImplItemDecl {
    impl_id: ImplId,
    idx: usize,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum ImplItem {
    #[derivative(Debug = "transparent")]
    Fn(Arc<ImplFn>),
    #[derivative(Debug = "transparent")]
    Type(Arc<ImplType>),
}

impl Parse for ImplItem {
    type Context = ImplItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ImplItemId) -> ParseResult<Self> {
        let mut lookahead = input.clone();
        let _viz: Visibility = lookahead.parse()?;

        Ok(if lookahead.peek::<tok::Fn>() {
            ImplItem::Fn(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Type>() {
            ImplItem::Type(input.parse_with(ctx.into())?)
        } else {
            input.error_exhausted()?;
        })
    }
}

wrapper_id_type!(pub ImplFnId => ImplItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct ImplFn {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: ImplFnId,
    pub signature: Signature,
    pub body: Expr,
}

impl Parse for ImplFn {
    type Context = ImplFnId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ImplFnId) -> ParseResult<Self> {
        Ok(ImplFn {
            id: ctx,
            signature: input.parse()?,
            body: Expr::expr_block(input, ExprContext::any_expr())?,
        })
    }
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

impl Parse for ImplType {
    type Context = ImplTypeId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ImplTypeId) -> ParseResult<Self> {
        Ok(ImplType {
            id: ctx,
            type_tok: input.parse()?,
            ident: input.parse()?,
            eq_tok: input.parse()?,
            ty: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}
