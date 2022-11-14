use super::{GcPtr, StringPool};
use std::{collections::BTreeMap, hash::BuildHasher, ops::Deref};

pub struct Tracer<'a> {
    pub(super) gray: &'a mut Vec<GcPtr<dyn GarbageCollect>>,
}

pub struct Finalizer<'a> {
    pub(super) string_pool: &'a mut StringPool,
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

    #[allow(unused_variables)]
    fn trace(&self, tracer: &mut Tracer) {}

    #[allow(unused_variables)]
    fn finalize(&self, finalizer: &mut Finalizer) {}
}

unsafe impl<T: GarbageCollect> GarbageCollect for &T {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        (**self).trace(tracer);
    }
}

unsafe impl GarbageCollect for () {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T1, T2> GarbageCollect for (T1, T2)
where
    T1: GarbageCollect,
    T2: GarbageCollect,
{
    fn needs_trace() -> bool {
        T1::needs_trace() || T2::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
        self.1.trace(tracer);
    }
}

unsafe impl<T1, T2, T3> GarbageCollect for (T1, T2, T3)
where
    T1: GarbageCollect,
    T2: GarbageCollect,
    T3: GarbageCollect,
{
    fn needs_trace() -> bool {
        T1::needs_trace() || T2::needs_trace() || T3::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
        self.1.trace(tracer);
        self.2.trace(tracer);
    }
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

unsafe impl<T: GarbageCollect, const N: usize> GarbageCollect for [T; N] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self {
            x.trace(tracer)
        }
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for Option<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        if let Some(x) = self {
            x.trace(tracer);
        }
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

unsafe impl<T: ?Sized + GarbageCollect> GarbageCollect for Box<T> {
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
    for std::collections::HashMap<K, V, S>
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

unsafe impl<K: GarbageCollect, V: GarbageCollect, S: BuildHasher> GarbageCollect
    for hashbrown::HashMap<K, V, S>
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

/// # Safety
/// `Aged` must be an identical type to `Self` but with a GC lifetime `'a`.
pub unsafe trait GcLifetime<'gc, 'a>: GarbageCollect {
    type Aged: GcLifetime<'gc, 'a> + 'a;
}

unsafe impl<'gc, 'a, T: GcLifetime<'gc, 'a>> GcLifetime<'gc, 'a> for Vec<T> {
    type Aged = Vec<T::Aged>;
}
