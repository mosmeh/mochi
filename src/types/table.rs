use super::{Integer, Value};
use crate::gc::{Trace, Tracer};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Default)]
pub struct Table<'a>(FxHashMap<Value<'a>, Value<'a>>);

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

impl<'a> Table<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn get<K>(&self, key: K) -> Value<'a>
    where
        K: Into<Value<'a>>,
    {
        self.0.get(&key.into()).cloned().unwrap_or_default()
    }

    #[inline]
    pub fn set<K, V>(&mut self, key: K, value: V)
    where
        K: Into<Value<'a>>,
        V: Into<Value<'a>>,
    {
        let key = key.into();
        let value = value.into();
        if value == Value::Nil {
            self.0.remove(&key);
        } else {
            self.0.insert(key, value);
        }
    }
}
