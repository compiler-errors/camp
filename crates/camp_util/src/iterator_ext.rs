pub trait IteratorExt: Iterator + Sized {
    fn try_collect_vec<T, E>(self) -> Result<Vec<T>, E>
    where
        Self: Iterator<Item = Result<T, E>>,
    {
        self.collect()
    }
}

impl<T> IteratorExt for T where T: Iterator {}
