use super::{GcPtr, StringPool};
use std::{collections::BTreeMap, hash::BuildHasher, ops::Deref};

pub struct Tracer<'a> {
    pub(super) gray: &'a mut Vec<GcPtr<dyn GarbageCollect>>,
}

/// # Safety
/// `trace` must trace every `Gc` or `GcCell` inside a struct.
pub unsafe trait Trace {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    #[allow(unused_variables)]
    fn trace(&self, tracer: &mut Tracer) {}
}

unsafe impl<T: Trace> Trace for &T {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        (**self).trace(tracer);
    }
}

unsafe impl Trace for () {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T1, T2> Trace for (T1, T2)
where
    T1: Trace,
    T2: Trace,
{
    fn needs_trace() -> bool {
        T1::needs_trace() || T2::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
        self.1.trace(tracer);
    }
}

unsafe impl<T1, T2, T3> Trace for (T1, T2, T3)
where
    T1: Trace,
    T2: Trace,
    T3: Trace,
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

unsafe impl Trace for u8 {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl Trace for i32 {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl Trace for usize {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: Trace, const N: usize> Trace for [T; N] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self {
            x.trace(tracer)
        }
    }
}

unsafe impl<T: Trace> Trace for Option<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        if let Some(x) = self {
            x.trace(tracer);
        }
    }
}

unsafe impl<T: Trace> Trace for &[T] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl<T: Trace> Trace for &mut [T] {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl<T: ?Sized + Trace> Trace for Box<T> {
    fn trace(&self, tracer: &mut Tracer) {
        self.deref().trace(tracer);
    }
}

unsafe impl<T: Trace> Trace for Box<[T]> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self.iter() {
            x.trace(tracer);
        }
    }
}

unsafe impl Trace for String {
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: Trace> Trace for Vec<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        for x in self {
            x.trace(tracer);
        }
    }
}

unsafe impl<K: Trace, V: Trace, S: BuildHasher + 'static> Trace
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

unsafe impl<K: Trace, V: Trace, S: BuildHasher + 'static> Trace for hashbrown::HashMap<K, V, S> {
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

unsafe impl<K: Trace, V: Trace> Trace for BTreeMap<K, V> {
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

pub struct Finalizer<'a> {
    pub(super) string_pool: &'a mut StringPool,
}

#[allow(clippy::missing_safety_doc)]
pub unsafe trait GarbageCollect: Trace + std::fmt::Debug {
    #[allow(unused_variables)]
    fn finalize(&self, finalizer: &mut Finalizer) {}
}

#[allow(clippy::missing_safety_doc)]
pub unsafe trait GcBind<'gc, 'a> {
    type Bound: GcBind<'gc, 'a> + 'a;
}

unsafe impl<'gc, 'a, T: GcBind<'gc, 'a>> GcBind<'gc, 'a> for Vec<T> {
    type Bound = Vec<T::Bound>;
}

unsafe impl<'gc, 'a, T: GcBind<'gc, 'a>> GcBind<'gc, 'a> for &'a [T] {
    type Bound = &'a [T::Bound];
}
