use super::Value;
use crate::gc::{Trace, Tracer};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Default)]
pub struct Table<'a>(pub FxHashMap<Value<'a>, Value<'a>>);

impl<'a> From<FxHashMap<Value<'a>, Value<'a>>> for Table<'a> {
    fn from(x: FxHashMap<Value<'a>, Value<'a>>) -> Self {
        Self(x)
    }
}

unsafe impl Trace for Table<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

impl Table<'_> {
    pub fn new() -> Self {
        Default::default()
    }
}
