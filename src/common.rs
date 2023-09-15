use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub filename: Arc<str>,
}

impl Location {
    pub fn with_end(&self, end: usize) -> Self {
        return Location {
            start: self.start,
            end,
            filename: self.filename.clone(),
        };
    }
}
