use camp_ast::punctuated::Punctuated;
use camp_ast::{tok, Path, PathSegment};

use crate::parser::{Parse, ParseBuffer};
use crate::CampResult;

impl Parse for Path {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let mut path = Punctuated::new();

        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        Ok(Path { path })
    }
}

impl Parse for PathSegment {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Site>() {
            PathSegment::Site(input.parse()?)
        } else if input.peek::<tok::Super>() {
            PathSegment::Super(input.parse()?)
        } else if input.peek::<tok::Mod>() {
            PathSegment::Mod(input.parse()?)
        } else if input.peek::<tok::Extern>() {
            PathSegment::Extern(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            PathSegment::Ident(input.parse()?)
        } else if input.peek::<tok::Lt>() {
            PathSegment::Generics(input.parse()?)
        } else if input.peek::<tok::Star>() {
            PathSegment::Star(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

pub fn parse_typelike_path(input: &mut ParseBuffer<'_>) -> CampResult<Path> {
    let mut path: Path = input.parse()?;

    if let Some(generics) = input.parse()? {
        path.path.push_punct(tok::ColonColon { span: path.span().shrink_to_hi() });
        path.path.push(PathSegment::Generics(generics));
    }

    Ok(path)
}
