#[derive(Debug, Clone, Copy)]
pub struct Location<'a> {
    pub start: usize,
    pub end: usize,
    pub filename: &'a str,
}

impl<'a> Location<'a> {
    pub fn with_end(&self, end: usize) -> Self {
        return Location {
            start: self.start,
            end,
            filename: self.filename,
        };
    }
}
