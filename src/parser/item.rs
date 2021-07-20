use camino::Utf8Path;
use derivative::Derivative;

use super::expr::Expr;
use super::misc::{GenericsDecl, Punctuated, Visibility};
use super::misc::{ReturnTy, Supertraits};
use super::pat::Pat;
use super::ty::{TraitGenerics, TraitTy, Ty, TyPath};
use super::{tok, Parse, ParseContext, ShouldParse};
use crate::files::{calculate_submod_path, Files};
use crate::lexer::lex_file;
use crate::parser::ty::TraitTyPath;
use crate::result::{Error, Result};

#[derive(Debug)]
pub struct Mod {
    mod_decl: Option<ModDecl>,
    items: Vec<ModuleItem>,
}

impl Mod {
    pub fn parse_file(
        mod_decl: Option<ModDecl>,
        files: &mut Files,
        file_path: &Utf8Path,
        submod_path: &Utf8Path,
    ) -> Result<Mod> {
        let buf = lex_file(files, &file_path)?;
        let input = ParseContext::new(&buf);

        Ok(Mod {
            mod_decl,
            items: Mod::parse_items(input, files, submod_path)?,
        })
    }

    pub fn parse_items(
        mut input: ParseContext<'_>,
        files: &mut Files,
        submod_path: &Utf8Path,
    ) -> Result<Vec<ModuleItem>> {
        let mut items = vec![];

        while !input.is_empty() {
            let mut lookahead = input.clone();
            let _viz: Visibility = lookahead.parse()?;

            let item = if lookahead.peek::<tok::Mod>() {
                ModuleItem::Mod(Mod::parse_submod(&mut input, files, submod_path)?)
            } else if lookahead.peek::<tok::Struct>() {
                ModuleItem::Struct(input.parse()?)
            } else if lookahead.peek::<tok::Use>() {
                ModuleItem::Use(input.parse()?)
            } else if lookahead.peek::<tok::Enum>() {
                ModuleItem::Enum(input.parse()?)
            } else if lookahead.peek::<tok::Fn>() {
                ModuleItem::Fn(input.parse()?)
            } else if lookahead.peek::<tok::Trait>() {
                ModuleItem::Trait(input.parse()?)
            } else if lookahead.peek::<tok::Impl>() {
                ModuleItem::Impl(input.parse()?)
            } else {
                lookahead.error_exhausted()?;
            };

            items.push(item);
        }

        Ok(items)
    }

    pub fn parse_submod(
        input: &mut ParseContext<'_>,
        files: &mut Files,
        submod_path: &Utf8Path,
    ) -> Result<Mod> {
        let mod_decl = input.parse()?;

        if input.peek::<tok::LCurly>() {
            let (_, contents, _) = input.parse_between_curlys()?;

            Ok(Mod {
                mod_decl: Some(mod_decl),
                items: Mod::parse_items(contents, files, submod_path)?,
            })
        } else {
            input.parse::<tok::Semicolon>()?;

            // Calculate the path of the submodule, then lex it.
            let (submod_path, file_path) =
                calculate_submod_path(submod_path, &mod_decl.name.ident, mod_decl.name.span)?;

            Mod::parse_file(Some(mod_decl), files, &file_path, &submod_path)
        }
    }
}

#[derive(Debug)]
pub struct ModDecl {
    viz: Visibility,
    mod_token: tok::Mod,
    name: tok::Ident,
}

impl Parse for ModDecl {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ModDecl {
            viz: input.parse()?,
            mod_token: input.parse()?,
            name: input.parse()?,
        })
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum ModuleItem {
    #[derivative(Debug = "transparent")]
    Mod(Mod),
    #[derivative(Debug = "transparent")]
    Use(Use),
    #[derivative(Debug = "transparent")]
    Struct(Struct),
    #[derivative(Debug = "transparent")]
    Enum(Enum),
    #[derivative(Debug = "transparent")]
    Fn(Fun),
    #[derivative(Debug = "transparent")]
    Trait(Trait),
    #[derivative(Debug = "transparent")]
    Impl(Impl),
}

#[derive(Debug)]
pub struct Use {
    use_tok: tok::Use,
    path: Punctuated<tok::Ident, tok::ColonColon>,
    ending: UseEnding,
    semi_tok: tok::Semicolon,
}

impl Parse for Use {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let use_tok = input.parse()?;
        let mut path = Punctuated::new();
        let ending;

        path.push(input.parse()?);

        loop {
            path.push_punct(input.parse()?);

            if input.peek::<tok::Ident>() && input.peek2::<tok::ColonColon>() {
                path.push(input.parse()?);
            } else {
                ending = input.parse()?;
                break;
            }
        }

        Ok(Use {
            use_tok,
            path,
            ending,
            semi_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
enum UseEnding {
    Item(tok::Ident, Option<UseRename>),
    Star(tok::Star),
}

impl Parse for UseEnding {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::Ident>() {
            Ok(UseEnding::Item(input.parse()?, input.parse()?))
        } else if input.peek::<tok::Star>() {
            Ok(UseEnding::Star(input.parse()?))
        } else {
            input.error_exhausted()?;
        }
    }
}

#[derive(Debug)]
struct UseRename {
    as_tok: tok::As,
    ident: tok::Ident,
}

impl Parse for UseRename {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(UseRename {
            as_tok: input.parse()?,
            ident: input.parse()?,
        })
    }
}

impl ShouldParse for UseRename {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::As>()
    }
}

#[derive(Debug)]
pub struct Struct {
    pub viz: Visibility,
    pub struct_tok: tok::Struct,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub fields: Fields,
    pub where_clause: Option<WhereClause>,
    pub semi_tok: Option<tok::Semicolon>,
}

impl Parse for Struct {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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
                    return Err(Error::ImproperWhere(where_clause.where_tok.span));
                }
                // Parse optional trailing where clause and semicolon token
                (input.parse()?, Some(input.parse()?))
            },
        };

        Ok(Struct {
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

#[derive(Derivative)]
#[derivative(Debug)]
pub enum Fields {
    #[derivative(Debug = "transparent")]
    Named(FieldsNamed),
    #[derivative(Debug = "transparent")]
    Positional(FieldsPositional),
    None,
}

impl Parse for Fields {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::LCurly>() {
            Fields::Named(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            Fields::Positional(input.parse()?)
        } else {
            Fields::None
        })
    }
}

#[derive(Debug)]
pub struct FieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<FieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for FieldsNamed {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(FieldsNamed {
            lcurly_tok,
            fields: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        })
    }
}

#[derive(Debug)]
pub struct FieldNamed {
    pub viz: Visibility,
    pub ident: tok::Ident,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

impl Parse for FieldNamed {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(FieldNamed {
            viz: input.parse()?,
            ident: input.parse()?,
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct FieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<FieldPositional, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

impl Parse for FieldsPositional {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(FieldsPositional {
            lparen_tok,
            fields: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

#[derive(Debug)]
pub struct FieldPositional {
    pub viz: Visibility,
    pub ty: Ty,
}

impl Parse for FieldPositional {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(FieldPositional {
            viz: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct WhereClause {
    pub where_tok: tok::Where,
    pub restrictions: Punctuated<TypeRestriction, tok::Comma>,
}

impl Parse for WhereClause {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Where>()
    }
}

#[derive(Debug)]
pub struct TypeRestriction {
    ty: Ty,
    trailing_traits: Supertraits,
}

impl Parse for TypeRestriction {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TypeRestriction {
            ty: input.parse()?,
            trailing_traits: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Enum {
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let viz = input.parse()?;
        let enum_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(Enum {
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

#[derive(Debug)]
pub struct EnumVariant {
    ident: tok::Ident,
    fields: Fields,
}

impl Parse for EnumVariant {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(EnumVariant {
            ident: input.parse()?,
            fields: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Fun {
    pub sig: Signature,
    pub body: Expr,
}

impl Parse for Fun {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(Fun {
            sig: input.parse()?,
            body: Expr::expr_block(input)?,
        })
    }
}

#[derive(Debug)]
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

#[derive(Debug)]
pub struct Parameter {
    pat: Pat,
    colon_tok: tok::Colon,
    ty: Ty,
}

impl Parse for Parameter {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(Parameter {
            pat: input.parse()?,
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Trait {
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let viz = input.parse()?;
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let supertraits = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut trait_items = vec![];

        while !contents.is_empty() {
            trait_items.push(input.parse()?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Trait {
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

#[derive(Derivative)]
#[derivative(Debug)]
pub enum TraitItem {
    #[derivative(Debug = "transparent")]
    Fn(TraitFn),
    #[derivative(Debug = "transparent")]
    Type(TraitType),
}

impl Parse for TraitItem {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Visibility::do_not_expect(input)?;

        Ok(if input.peek::<tok::Fn>() {
            TraitItem::Fn(input.parse()?)
        } else if input.peek::<tok::Type>() {
            TraitItem::Type(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

#[derive(Debug)]
pub struct TraitFn {
    pub signature: Signature,
    pub semi_tok: tok::Semicolon,
}

impl Parse for TraitFn {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TraitFn {
            signature: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TraitType {
    pub type_tok: tok::Type,
    pub ident: tok::Ident,
    pub supertraits: Option<Supertraits>,
    pub semi_tok: tok::Semicolon,
}

impl Parse for TraitType {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TraitType {
            type_tok: input.parse()?,
            ident: input.parse()?,
            supertraits: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Impl {
    pub impl_tok: tok::Impl,
    pub generics: Option<GenericsDecl>,
    pub impl_trait: Option<ImplTrait>,
    pub ty: Ty,
    pub lcurly_tok: tok::LCurly,
    pub impl_items: Vec<ImplItem>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for Impl {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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
                ty => return Err(Error::NotATrait(ty.span())),
            }
        } else {
            impl_trait = None;
            ty = ty_or_trait;
        }

        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut impl_items = vec![];

        while !contents.is_empty() {
            impl_items.push(input.parse()?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Impl {
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

#[derive(Debug)]
pub struct ImplTrait {
    trait_ty: TraitTy,
    for_tok: tok::For,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum ImplItem {
    #[derivative(Debug = "transparent")]
    Fn(ImplFn),
    #[derivative(Debug = "transparent")]
    Type(ImplType),
}

impl Parse for ImplItem {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let mut lookahead = input.clone();
        let _viz: Visibility = lookahead.parse()?;

        Ok(if lookahead.peek::<tok::Fn>() {
            ImplItem::Fn(input.parse()?)
        } else if lookahead.peek::<tok::Type>() {
            ImplItem::Type(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

#[derive(Debug)]
pub struct ImplFn {
    pub signature: Signature,
    pub body: Expr,
}

impl Parse for ImplFn {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ImplFn {
            signature: input.parse()?,
            body: Expr::expr_block(input)?,
        })
    }
}

#[derive(Debug)]
pub struct ImplType {
    pub type_tok: tok::Type,
    pub ident: tok::Ident,
    pub eq_tok: tok::Eq,
    pub ty: Ty,
    pub semi_tok: tok::Semicolon,
}

impl Parse for ImplType {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ImplType {
            type_tok: input.parse()?,
            ident: input.parse()?,
            eq_tok: input.parse()?,
            ty: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}
