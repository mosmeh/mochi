use super::GcPtr;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    hash::BuildHasher,
    ops::Deref,
};

pub struct Tracer<'a> {
    pub(super) gray: &'a mut Vec<GcPtr<dyn GarbageCollect>>,
}

/// # Safety
/// `trace` must trace every `Gc` or `GcCell` inside a struct.
pub unsafe trait GarbageCollect {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, _: &mut Tracer) {}
}

unsafe impl GarbageCollect for u8 {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl GarbageCollect for i32 {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl GarbageCollect for usize {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for &[T] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for &mut [T] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for Box<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.deref().trace(tracer);
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for Box<[T]> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl GarbageCollect for String {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for RefCell<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.borrow().trace(tracer);
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for Vec<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self {
            x.trace(tracer);
        }
    }
}

unsafe impl<K: GarbageCollect, V: GarbageCollect, S: BuildHasher> GarbageCollect
    for HashMap<K, V, S>
{
    fn needs_trace() -> bool {
        K::needs_trace() || V::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for (k, v) in self {
            k.trace(tracer);
            v.trace(tracer);
        }
    }
}

unsafe impl<K: GarbageCollect, V: GarbageCollect> GarbageCollect for BTreeMap<K, V> {
    fn needs_trace() -> bool {
        K::needs_trace() || V::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for (k, v) in self {
            k.trace(tracer);
            v.trace(tracer);
        }
    }
}
