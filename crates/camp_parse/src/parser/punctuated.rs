#[derive(Debug, Hash, Eq, PartialEq)]
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
        assert_eq!(self.len(), 1);
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

    pub fn iter_items(&self) -> impl Iterator<Item = &'_ T> {
        self.items
            .iter()
            .map(|(t, _)| t)
            .chain(self.last.as_deref())
    }

    pub fn into_items(self) -> impl Iterator<Item = T> {
        self.items
            .into_iter()
            .map(|(t, _)| t)
            .chain(self.last.map(|t| *t))
    }
}
