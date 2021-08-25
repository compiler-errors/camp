use crate::{
    lexer::Span,
    parser::{
        tok::{self, Lifetime},
        ty::{Generics, TraitTy, Ty},
        Parse,
        ParseContext,
        ShouldParse,
    },
    result::{Error, Result},
};

#[derive(Debug)]
pub struct Punctuated<T, S> {
    items: Vec<(T, S)>,
    last: Option<Box<T>>,
}

impl<T, S> Punctuated<T, S> {
    pub fn new() -> Punctuated<T, S> {
        Punctuated {
            items: vec![],
            last: None,
        }
    }

    pub fn push(&mut self, t: T) {
        assert!(self.last.is_none());
        self.last = Some(Box::new(t));
    }

    pub fn push_punct(&mut self, s: S) {
        let t = self.last.take().unwrap();
        self.items.push((*t, s))
    }

    pub fn pop_punct(&mut self) -> S {
        assert!(self.last.is_none());
        let (t, s) = self.items.pop().unwrap();
        self.last = Some(Box::new(t));
        s
    }

    pub fn len(&self) -> usize {
        self.items.len() + (self.last.is_some() as usize)
    }

    pub fn trailing(&self) -> bool {
        // If there's at least one item, and not a item at the end
        !self.items.is_empty() && self.last.is_none()
    }

    pub fn unwrap_one(self) -> T {
        assert!(self.items.is_empty());
        *self.last.unwrap()
    }

    pub fn map<T2>(self, mut f: impl FnMut(T) -> T2) -> Punctuated<T2, S> {
        Punctuated {
            items: self.items.into_iter().map(|(t, s)| (f(t), s)).collect(),
            last: self.last.map(|t| Box::new(f(*t))),
        }
    }

    pub fn first(&self) -> Option<&T> {
        self.items
            .first()
            .map(|(t, _)| t)
            .or_else(|| self.last.as_deref())
    }

    pub fn last(&self) -> Option<&T> {
        self.last
            .as_deref()
            .or_else(|| self.items.last().map(|(t, _)| t))
    }
}

#[derive(Debug)]
pub enum Visibility {
    Pub(tok::Pub),
    Private,
}

impl Visibility {
    pub fn do_not_expect(input: &mut ParseContext<'_>) -> Result<()> {
        if input.peek::<tok::Pub>() {
            Err(Error::UnexpectedPub(input.next_span()))
        } else {
            Ok(())
        }
    }
}

impl Parse for Visibility {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::Pub>() {
            Ok(Visibility::Pub(input.parse()?))
        } else {
            Ok(Visibility::Private)
        }
    }
}

#[derive(Debug)]
pub enum Mutability {
    Mut(tok::Mut),
    Const,
}

impl Parse for Mutability {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::Mut>() {
            Ok(Mutability::Mut(input.parse()?))
        } else {
            Ok(Mutability::Const)
        }
    }
}

#[derive(Debug)]
pub struct GenericsDecl {
    lt_tok: tok::Lt,
    generics: Punctuated<GenericDecl, tok::Comma>,
    gt_tok: tok::Gt,
}

impl Parse for GenericsDecl {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

        Ok(GenericsDecl {
            lt_tok,
            generics,
            gt_tok: input.parse()?,
        })
    }
}

impl ShouldParse for GenericsDecl {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

#[derive(Debug)]
enum GenericDecl {
    Lifetime(GenericLifetime),
    Ident(GenericType),
}

impl Parse for GenericDecl {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            GenericDecl::Lifetime(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            GenericDecl::Ident(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

#[derive(Debug)]
struct GenericLifetime {
    lifetime: tok::Lifetime,
    maybe_bounds: Option<Supertraits>,
}

impl Parse for GenericLifetime {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(GenericLifetime {
            lifetime: input.parse()?,
            maybe_bounds: input.parse()?,
        })
    }
}

#[derive(Debug)]
struct GenericType {
    ident: tok::Ident,
    maybe_bounds: Option<Supertraits>,
}

impl Parse for GenericType {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(GenericType {
            ident: input.parse()?,
            maybe_bounds: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Supertraits {
    colon_tok: tok::Colon,
    traits: Punctuated<Supertrait, tok::Plus>,
}

impl Supertraits {
    pub fn parse_supertrait_list(
        input: &mut ParseContext<'_>,
    ) -> Result<Punctuated<Supertrait, tok::Plus>> {
        let mut traits = Punctuated::new();

        loop {
            traits.push(input.parse()?);

            if input.peek::<tok::Plus>() {
                traits.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        Ok(traits)
    }
}

impl Parse for Supertraits {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let colon_tok = input.parse()?;
        let traits = Supertraits::parse_supertrait_list(input)?;

        Ok(Supertraits { colon_tok, traits })
    }
}

impl ShouldParse for Supertraits {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

#[derive(Debug)]
pub enum Supertrait {
    Trait(TraitTy),
    Lifetime(Lifetime),
}

impl Supertrait {
    pub fn span(&self) -> Span {
        match self {
            Supertrait::Trait(t) => t.span(),
            Supertrait::Lifetime(t) => t.span,
        }
    }
}

impl Parse for Supertrait {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            Supertrait::Lifetime(input.parse()?)
        } else {
            Supertrait::Trait(input.parse()?)
        })
    }
}

#[derive(Debug)]
pub struct ReturnTy {
    pub arrow_tok: tok::Arrow,
    pub ty: Box<Ty>,
}

impl Parse for ReturnTy {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ReturnTy {
            arrow_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ShouldParse for ReturnTy {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Arrow>()
    }
}

#[derive(Debug)]
pub enum PathSegment {
    Root(tok::Root),
    Ident(tok::Ident),
    Generics(Generics),
}

impl Parse for PathSegment {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::Ident>() {
            PathSegment::Ident(input.parse()?)
        } else if input.peek::<tok::Root>() {
            PathSegment::Root(input.parse()?)
        } else if input.peek::<tok::Lt>() {
            PathSegment::Generics(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}
