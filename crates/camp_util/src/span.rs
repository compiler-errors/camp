use codespan_derive::IntoLabel;

use crate::FileId;

#[derive(Debug, Copy, Clone)]
pub struct Span(pub FileId, pub usize, pub usize);

impl Span {
    pub fn until(self, other: Span) -> Span {
        assert_eq!(self.0, other.0, "Can only unify spans of the same file!");
        Span(self.0, self.1.min(other.1), self.2.max(other.2))
    }
}

impl IntoLabel for Span {
    type FileId = FileId;

    fn into_label(&self, style: codespan_derive::LabelStyle) -> codespan_derive::Label<FileId> {
        codespan_derive::Label::new(style, self.0, self.1..self.2)
    }
}
