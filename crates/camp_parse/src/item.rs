use std::sync::Arc;

use camp_ast::{punctuated::Punctuated, *};
pub use camp_lex::tok::Token as RawToken;
use camp_util::bail;

use crate::{
    do_not_expect_visibility, expr_block,
    parser::{Parse, ParseBuffer, ShouldParse},
    result::ParseError,
    ExprContext, ParseDb,
};

impl Parse for SubmodDecl {
    type Context = ItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ItemId) -> CampResult<Self> {
        do_not_expect_attribute(input)?;

        Ok(SubmodDecl {
            id: ctx,
            viz: input.parse()?,
            mod_token: input.parse()?,
            name: input.parse()?,
        })
    }
}

pub fn mod_ast_from_file(db: &dyn ParseDb, id: ModId) -> CampResult<Arc<Mod>> {
    let file_id = db.mod_file(id)?;

    let contents = db.open_file(file_id)?;
    let buf = camp_lex::lex(file_id, &contents)?;

    let mut input = ParseBuffer::new(db, &buf);
    let m = input.parse_with(id)?;
    assert!(input.is_empty());

    Ok(m)
}

impl Parse for Mod {
    type Context = ModId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ModId) -> CampResult<Self> {
        let mut items = vec![];

        while !input.is_empty() {
            let decl = ItemDecl { parent: ItemParent::Mod(ctx), idx: items.len() };
            let id = input.db.item_decl(decl);

            items.push(input.parse_with(id)?);
        }

        Ok(Mod { id: ctx, items })
    }
}

impl Parse for Item {
    type Context = ItemId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ItemId) -> CampResult<Self> {
        let mut lookahead = input.clone();
        let _attrs = parse_many_attribute(&mut lookahead)?;
        let _viz: Visibility = lookahead.parse()?;

        Ok(if lookahead.peek::<tok::Mod>() {
            let mod_decl = input.parse_with(ctx)?;
            let id = input.db.mod_decl(ModDecl::Submod(mod_decl));

            if input.peek::<tok::LCurly>() {
                let (_, mut contents, rcurly_tok) = input.parse_between_curlys()?;

                // Parse the mod fully
                let m = contents.parse_with(id)?;
                contents.expect_empty(rcurly_tok)?;

                Item::Mod(m)
            } else if input.peek::<tok::Semicolon>() {
                let _semi: tok::Semicolon = input.parse()?;
                Item::Mod(input.db.mod_ast_from_file(id)?)
            } else {
                lookahead.error_exhausted()?;
            }
        } else if lookahead.peek::<tok::Extern>() {
            Item::Extern(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Struct>() {
            Item::Struct(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Use>() {
            Item::Use(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Enum>() {
            Item::Enum(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Fn>() {
            Item::Function(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Trait>() {
            Item::Trait(input.parse_with(ctx.into())?)
        } else if lookahead.peek::<tok::Impl>() {
            Item::Impl(input.parse_with(ctx.into())?)
        } else {
            lookahead.error_exhausted()?;
        })
    }
}

pub fn parse_many_attribute(input: &mut ParseBuffer<'_>) -> CampResult<Vec<Attribute>> {
    let mut attrs = vec![];
    while let Some(attr) = input.parse()? {
        attrs.push(attr);
    }
    Ok(attrs)
}

pub fn do_not_expect_attribute(input: &mut ParseBuffer<'_>) -> CampResult<()> {
    if let Some(attr) = input.parse::<Option<Attribute>>()? {
        bail!(ParseError::UnexpectedAttr { name: attr.name().to_owned(), span: attr.span() });
    } else {
        Ok(())
    }
}

impl Parse for Attribute {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let hash_tok = input.parse()?;
        let (lsq_tok, mut buf, rsq_tok) = input.parse_between_sqs()?;
        let inner = buf.parse()?;
        buf.expect_empty(rsq_tok)?;
        Ok(Attribute { hash_tok, lsq_tok, inner, rsq_tok })
    }
}

impl ShouldParse for Attribute {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Hash>()
    }
}

impl Parse for AttributeInner {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(AttributeInner::NameValue {
            ident: input.parse()?,
            eq_tok: input.parse()?,
            lit: input.parse()?,
        })
    }
}

impl Parse for Extern {
    type Context = ExternId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ExternId) -> CampResult<Self> {
        do_not_expect_attribute(input)?;

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

impl Parse for Use {
    type Context = UseId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: UseId) -> CampResult<Self> {
        do_not_expect_attribute(input)?;

        Ok(Use {
            id: ctx,
            viz: input.parse()?,
            use_tok: input.parse()?,
            path: input.parse()?,
            rename: input.parse()?,
            semi_tok: input.parse()?,
        })
    }
}

impl Parse for UseRename {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(UseRename { as_tok: input.parse()?, ident: input.parse()? })
    }
}

impl ShouldParse for UseRename {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::As>()
    }
}

impl Parse for GenericsDecl {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let lt_tok = input.parse()?;
        let mut generics = Punctuated::new();

        loop {
            if input.peek::<tok::Gt>() {
                break;
            }

            if input.peek::<tok::Lifetime>() {
                generics.push(GenericDecl::Lifetime(input.parse()?));
            } else if input.peek::<tok::Ident>() {
                generics.push(GenericDecl::Ident(input.parse()?));
            } else {
                input.error_exhausted()?;
            }

            if input.peek::<tok::Gt>() {
                break;
            }

            generics.push_punct(input.parse()?);
        }

        Ok(GenericsDecl { lt_tok, generics, gt_tok: input.parse()? })
    }
}

impl ShouldParse for GenericsDecl {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

impl Parse for GenericDecl {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            GenericDecl::Lifetime(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            GenericDecl::Ident(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

impl Parse for GenericLifetime {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(GenericLifetime { lifetime: input.parse()?, maybe_bounds: input.parse()? })
    }
}

impl Parse for GenericType {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(GenericType { ident: input.parse()?, maybe_bounds: input.parse()? })
    }
}

impl Parse for Struct {
    type Context = StructId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: StructId) -> CampResult<Self> {
        let attrs = parse_many_attribute(input)?;
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
                    bail!(ParseError::ImproperWhere(where_clause.where_tok.span));
                }
                // Parse optional trailing where clause and semicolon token
                (input.parse()?, Some(input.parse()?))
            }
        };

        Ok(Struct {
            id: ctx,
            attrs,
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

impl Parse for Fields {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::LCurly>() {
            Fields::Named(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            Fields::Positional(input.parse()?)
        } else {
            Fields::None
        })
    }
}

impl Parse for FieldsNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(FieldsNamed { lcurly_tok, fields: contents.parse_punctuated(rcurly_tok)?, rcurly_tok })
    }
}

impl Parse for FieldNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(FieldNamed {
            viz: input.parse()?,
            ident: input.parse()?,
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl Parse for FieldsPositional {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(FieldsPositional {
            lparen_tok,
            fields: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

impl Parse for FieldPositional {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(FieldPositional { viz: input.parse()?, ty: input.parse()? })
    }
}

impl Parse for WhereClause {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
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

        Ok(WhereClause { where_tok, restrictions })
    }
}

impl ShouldParse for WhereClause {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Where>()
    }
}

impl Parse for TypeRestriction {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TypeRestriction { subject: input.parse()?, trailing_traits: input.parse()? })
    }
}

impl Parse for TyOrLifetime {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            TyOrLifetime::Lifetime(input.parse()?)
        } else {
            TyOrLifetime::Ty(input.parse()?)
        })
    }
}

impl Parse for Enum {
    type Context = EnumId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: EnumId) -> CampResult<Self> {
        let attrs = parse_many_attribute(input)?;
        let viz = input.parse()?;
        let enum_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(Enum {
            id: ctx,
            attrs,
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

impl Parse for EnumVariant {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(EnumVariant { ident: input.parse()?, fields: input.parse()? })
    }
}

impl Parse for Trait {
    type Context = TraitId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: TraitId) -> CampResult<Self> {
        let attrs = parse_many_attribute(input)?;
        let viz = input.parse()?;
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let supertraits = input.parse()?;
        let where_clause = input.parse()?;

        let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut items = vec![];

        while !contents.is_empty() {
            let decl = ItemDecl { parent: ItemParent::Trait(ctx), idx: items.len() };
            let id = input.db.item_decl(decl);

            items.push(contents.parse_with(id)?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Trait {
            id: ctx,
            attrs,
            viz,
            fn_tok,
            ident,
            generics,
            supertraits,
            where_clause,
            lcurly_tok,
            items,
            rcurly_tok,
        })
    }
}

impl Parse for Impl {
    type Context = ImplId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ImplId) -> CampResult<Self> {
        do_not_expect_visibility(input)?;

        let attrs = parse_many_attribute(input)?;
        let impl_tok = input.parse()?;
        let generics = input.parse()?;

        let ty_or_trait: Ty = input.parse()?;

        let impl_trait;
        let ty;

        if let Some(for_tok) = input.parse()? {
            match ty_or_trait {
                Ty::Path(path) => {
                    impl_trait = Some(ImplTrait { trait_ty: TraitTy::Path(path), for_tok });
                    ty = input.parse()?;
                }
                ty => bail!(ParseError::NotATrait(ty.span())),
            }
        } else {
            impl_trait = None;
            ty = ty_or_trait;
        }

        let where_clause: Option<WhereClause> = input.parse()?;
        let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut items = vec![];

        while !contents.is_empty() {
            let decl = ItemDecl { parent: ItemParent::Impl(ctx), idx: items.len() };
            let id = input.db.item_decl(decl);

            items.push(contents.parse_with(id)?);
        }

        contents.expect_empty(rcurly_tok)?;

        Ok(Impl {
            id: ctx,
            attrs,
            impl_tok,
            generics,
            impl_trait,
            ty,
            where_clause,
            lcurly_tok,
            items,
            rcurly_tok,
        })
    }
}
