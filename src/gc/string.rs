use super::{Finalizer, GarbageCollect, GcBind, GcPtr, Trace};
use bstr::ByteSlice;
use hashbrown::HashMap;
use rustc_hash::FxHasher;
use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

pub(super) type StringPool = HashMap<GcPtr<BoxedString>, (), ()>;

#[derive(Hash, PartialEq, Eq)]
pub struct BoxedString(pub(super) Box<[u8]>);

impl std::fmt::Debug for BoxedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("BoxedString")
            .field(&self.0.as_bstr())
            .finish()
    }
}

impl AsRef<[u8]> for BoxedString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Deref for BoxedString {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

unsafe impl Trace for BoxedString {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl GarbageCollect for BoxedString {
    fn finalize(&self, finalizer: &mut Finalizer) {
        let hash = calc_str_hash(&self.0);
        let table = finalizer.string_pool.raw_table();
        let bucket = table
            .find(hash, |(k, _)| {
                let gc_box = unsafe { k.as_ref() };
                gc_box.value.as_bytes() == self.0.as_ref()
            })
            .unwrap();
        unsafe { table.remove(bucket) };
    }
}

unsafe impl<'gc, 'a> GcBind<'gc, 'a> for BoxedString {
    type Bound = Self;
}

impl BoxedString {
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

pub(super) fn calc_str_hash<T: AsRef<[u8]>>(s: T) -> u64 {
    let mut state = FxHasher::default();
    s.as_ref().hash(&mut state);
    state.finish()
}
