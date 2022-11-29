use super::{GcBind, GcContext, Trace};
use std::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
};

pub struct RootSet<'gc> {
    pub(super) stack: RefCell<Vec<Option<NonNull<dyn Trace>>>>,
    pub(super) invariant: PhantomData<Cell<&'gc ()>>,
}

#[doc(hidden)]
pub struct ShadowedRoot<'gc, 'roots, T> {
    roots: &'roots RootSet<'gc>,
    index: usize,
    value: Option<T>,
}

impl<T> Drop for ShadowedRoot<'_, '_, T> {
    fn drop(&mut self) {
        self.roots.stack.borrow_mut().pop().unwrap();
        debug_assert_eq!(self.roots.stack.borrow().len(), self.index);
    }
}

impl<'gc, 'roots, T> ShadowedRoot<'gc, 'roots, T> {
    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn new(roots: &'roots RootSet<'gc>) -> Self {
        let mut roots_ref = roots.stack.borrow_mut();
        let index = roots_ref.len();
        roots_ref.push(None);
        Self {
            roots,
            index,
            value: None,
        }
    }
}

pub struct Root<'gc, 'roots, 'root, T>(&'root mut ShadowedRoot<'gc, 'roots, T>);

impl<'gc, 'roots, 'root, T> Root<'gc, 'roots, 'root, T> {
    #[doc(hidden)]
    pub unsafe fn new(shadowed: &'root mut ShadowedRoot<'gc, 'roots, T>) -> Self {
        Self(shadowed)
    }
}

impl<'gc, 'roots, 'root, T> Root<'gc, 'roots, 'root, T> {
    pub fn bind<U>(self, x: U) -> Rooted<'gc, 'roots, 'root, T>
    where
        T: Trace,
        U: GcBind<'gc, 'roots, Bound = T>,
    {
        self.0.value = Some(unsafe { super::bind_value(x) });

        let ptr = self.0.value.as_mut().unwrap() as *mut T;
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        fn to_static<'a>(ptr: NonNull<dyn Trace + 'a>) -> NonNull<dyn Trace> {
            unsafe { std::mem::transmute(ptr) }
        }
        self.0.roots.stack.borrow_mut()[self.0.index] = Some(to_static(ptr));

        Rooted(self.0)
    }
}

pub struct Rooted<'gc, 'roots, 'root, T>(&'root mut ShadowedRoot<'gc, 'roots, T>);

impl<'gc, 'root, T: GcBind<'gc, 'root>> Deref for Rooted<'gc, '_, 'root, T> {
    type Target = T::Bound;

    fn deref(&self) -> &Self::Target {
        let ptr = self.0.value.as_ref().unwrap() as *const T;
        let ptr = ptr as *const T::Bound;
        unsafe { &*ptr }
    }
}

impl<'gc, 'root, T: GcBind<'gc, 'root>> AsRef<T::Bound> for Rooted<'gc, '_, 'root, T> {
    fn as_ref(&self) -> &T::Bound {
        self.deref()
    }
}

impl<'gc, 'a, 'roots, 'root, T: GcBind<'gc, 'a>> Rooted<'gc, 'roots, 'root, T> {
    #[allow(unused_variables)]
    pub fn into_inner(self, gc: &'a GcContext<'gc>) -> T::Bound {
        self.0.roots.stack.borrow_mut()[self.0.index] = None;
        unsafe { super::bind_value(self.0.value.take().unwrap()) }
    }
}

#[macro_export]
macro_rules! new_root {
    ($roots:expr, $($root:ident),*) => {$(
        let mut $root = unsafe { $crate::gc::ShadowedRoot::new($roots) };
        #[allow(unused_mut)]
        let mut $root = unsafe { $crate::gc::Root::new(&mut $root) };
    )*}
}

#[macro_export]
macro_rules! to_rooted {
    ($roots:expr, $($value:ident),*) => {$(
        $crate::new_root!($roots, r);
        let $value = r.bind($value);
    )*}
}
