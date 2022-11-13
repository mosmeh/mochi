use super::{GarbageCollect, GcContext, GcLifetime};
use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

pub struct RootSet(pub(super) RefCell<Vec<Option<NonNull<dyn GarbageCollect>>>>);

pub struct ShadowedRoot<'roots, T> {
    roots: &'roots RootSet,
    index: usize,
    ptr: Option<NonNull<T>>,
}

impl<T> Drop for ShadowedRoot<'_, T> {
    fn drop(&mut self) {
        self.roots.0.borrow_mut().pop().unwrap();
        debug_assert_eq!(self.roots.0.borrow().len(), self.index);
        if let Some(ptr) = self.ptr {
            unsafe { Box::from_raw(ptr.as_ptr()) };
        }
    }
}

impl<'roots, T> ShadowedRoot<'roots, T> {
    #[doc(hidden)]
    pub fn new(roots: &'roots RootSet) -> Self {
        let mut roots_ref = roots.0.borrow_mut();
        let index = roots_ref.len();
        roots_ref.push(None);
        Self {
            roots,
            index,
            ptr: None,
        }
    }
}

pub struct Root<'roots, 'root, T>(Pin<&'root mut ShadowedRoot<'roots, T>>);

impl<'roots, 'root, T> Root<'roots, 'root, T> {
    #[doc(hidden)]
    pub fn new(shadowed: &'root mut ShadowedRoot<'roots, T>) -> Self {
        Self(Pin::new(shadowed))
    }
}

impl<'roots, 'root, T> Root<'roots, 'root, T> {
    pub fn bind<U>(mut self, x: U) -> Rooted<'roots, 'root, T>
    where
        T: GarbageCollect,
        U: GcLifetime<'roots, Aged = T>,
    {
        let ptr = Box::into_raw(Box::new(x)) as *mut T;
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        self.0.ptr = Some(ptr);

        fn to_static<'a>(ptr: NonNull<dyn GarbageCollect + 'a>) -> NonNull<dyn GarbageCollect> {
            unsafe { std::mem::transmute(ptr) }
        }
        self.0.roots.0.borrow_mut()[self.0.index] = Some(to_static(ptr));

        Rooted(self.0)
    }
}

pub struct Rooted<'roots, 'root, T>(Pin<&'root mut ShadowedRoot<'roots, T>>);

impl<'root, T: GcLifetime<'root>> Deref for Rooted<'_, 'root, T> {
    type Target = T::Aged;

    fn deref(&self) -> &Self::Target {
        let ptr = self.0.ptr.unwrap().as_ptr() as *mut T::Aged;
        unsafe { &*ptr }
    }
}

impl<'a, T: GcLifetime<'a>> Rooted<'_, '_, T> {
    #[allow(unused_variables)]
    pub fn into_inner(mut self, gc: &'a GcContext) -> T::Aged {
        let shadowed = self.0.deref_mut();
        shadowed.roots.0.borrow_mut()[shadowed.index] = None;
        let ptr = shadowed.ptr.take().unwrap().as_ptr() as *mut T::Aged;
        *unsafe { Box::from_raw(ptr) }
    }
}

#[macro_export]
macro_rules! new_root {
    ($roots:expr, $($root:ident),*) => {$(
        let mut $root = $crate::gc::ShadowedRoot::new($roots);
        let $root = $crate::gc::Root::new(&mut $root);
    )*}
}

#[macro_export]
macro_rules! to_rooted {
    ($roots:expr, $($value:ident),*) => {$(
        new_root!($roots, r);
        let $value = r.bind($value);
    )*}
}
