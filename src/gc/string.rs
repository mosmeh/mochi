use super::{Finalizer, GarbageCollect, GcPtr};
use hashbrown::HashMap;
use rustc_hash::FxHasher;
use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

pub(super) type StringPool = HashMap<GcPtr<BoxedString>, (), ()>;

#[derive(Hash, PartialEq, Eq)]
pub struct BoxedString(pub(super) Box<[u8]>);

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

unsafe impl GarbageCollect for BoxedString {
    fn needs_trace() -> bool {
        false
    }

    fn finalize(&self, finalizer: &mut Finalizer) {
        let hash = calc_str_hash(&self.0);
        let table = finalizer.string_pool.raw_table_mut();
        let bucket = table
            .find(hash, |(k, _)| {
                let gc_box = unsafe { k.as_ref() };
                gc_box.value.as_bytes() == self.0.as_ref()
            })
            .unwrap();
        unsafe { table.remove(bucket) };
    }
}

impl BoxedString {
    pub const fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

pub(super) fn calc_str_hash<T: AsRef<[u8]>>(s: T) -> u64 {
    let mut state = FxHasher::default();
    s.as_ref().hash(&mut state);
    state.finish()
}
