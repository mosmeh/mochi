mod root;
mod string;
mod traits;

pub use root::{Root, RootSet, Rooted, ShadowedRoot};
pub(crate) use string::BoxedString;
pub use traits::{Finalizer, GarbageCollect, GcBind, Trace, Tracer};

use crate::{
    runtime::Vm,
    types::{LuaString, Value},
};
use hashbrown::hash_map::RawEntryMut;
use std::{
    borrow::Cow,
    cell::{Cell, Ref, RefCell, RefMut},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::Deref,
    ptr::NonNull,
};
use string::StringPool;

pub struct GcHeap {
    vm: GcCell<'static, 'static, Vm<'static, 'static>>,
    roots: RootSet<'static>,
    gc: GcContext<'static>,
}

impl Default for GcHeap {
    fn default() -> Self {
        let gc = GcContext {
            pause: Cell::new(200),
            step_multiplier: Cell::new(100),
            step_size: Cell::new(13),

            is_running: Cell::new(true),
            phase: Phase::Pause,
            current_white: Default::default(),
            allocated_bytes: Default::default(),
            debt: Default::default(),
            estimate: Default::default(),

            all: Default::default(),
            sweep: Default::default(),
            prev_sweep: Default::default(),
            gray: Default::default(),
            gray_again: Default::default(),

            string_pool: Default::default(),

            invariant: Default::default(),
        };

        fn to_static<'gc, T: GcBind<'gc, 'static>>(
            ptr: GcCell<T>,
        ) -> GcCell<'gc, 'static, T::Bound> {
            unsafe { std::mem::transmute(ptr) }
        }
        let vm = to_static(gc.allocate_cell(Vm::new(&gc)));
        let ptr = Box::into_raw(Box::new(vm));

        let roots = RootSet {
            stack: Default::default(),
            invariant: PhantomData,
        };
        roots.stack.borrow_mut().push(NonNull::new(ptr));

        Self { gc, roots, vm }
    }
}

impl GcHeap {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(&mut GcContext<'gc>, &RootSet<'gc>, GcCell<'gc, '_, Vm<'gc, '_>>) -> R,
    {
        f(&mut self.gc, &self.roots, self.vm)
    }
}

const GCSWEEPMAX: i32 = 100;
const PAUSEADJ: usize = 100;
const WORK2MEM: usize = std::mem::size_of::<GcBox<Value>>();

type GcPtr<T> = NonNull<GcBox<T>>;

pub struct GcContext<'gc> {
    pause: Cell<usize>,
    step_multiplier: Cell<usize>,
    step_size: Cell<usize>,

    is_running: Cell<bool>,
    phase: Phase,
    current_white: bool,
    allocated_bytes: Cell<usize>,
    debt: Cell<isize>,
    estimate: usize,

    all: Cell<Option<GcPtr<dyn GarbageCollect>>>,
    sweep: Option<GcPtr<dyn GarbageCollect>>,
    prev_sweep: Option<GcPtr<dyn GarbageCollect>>,
    gray: Vec<GcPtr<dyn GarbageCollect>>,
    gray_again: RefCell<Vec<GcPtr<dyn GarbageCollect>>>,

    string_pool: RefCell<StringPool>,

    invariant: PhantomData<Cell<&'gc ()>>,
}

impl Drop for GcContext<'_> {
    fn drop(&mut self) {
        let mut it = self.all.get();
        while let Some(ptr) = it {
            let gc_box = unsafe { ptr.as_ref() };
            it = gc_box.next;
            unsafe { Box::from_raw(ptr.as_ptr()) };
        }
    }
}

impl<'gc> GcContext<'gc> {
    pub fn is_running(&self) -> bool {
        self.is_running.get()
    }

    pub fn stop(&self) {
        self.is_running.set(false);
    }

    pub fn restart(&self) {
        self.is_running.set(true);
        self.set_debt(0);
    }

    pub fn total_bytes(&self) -> usize {
        (self.allocated_bytes.get() as isize + self.debt.get()) as usize
    }

    pub fn debt(&self) -> isize {
        self.debt.get()
    }

    pub fn pause(&self) -> usize {
        self.pause.get()
    }

    pub fn set_pause(&self, pause: usize) {
        self.pause.set(pause);
    }

    pub fn step_multiplier(&self) -> usize {
        self.step_multiplier.get()
    }

    pub fn set_step_multiplier(&self, step_multiplier: usize) {
        self.step_multiplier.set(step_multiplier);
    }

    pub fn step_size(&self) -> usize {
        self.step_size.get()
    }

    pub fn set_step_size(&self, step_size: usize) {
        self.step_size.set(step_size);
    }

    pub fn should_perform_gc(&self) -> bool {
        self.is_running() && self.debt() > 0
    }

    pub fn allocate<'a, T>(&'a self, value: T) -> Gc<'gc, 'a, T::Bound>
    where
        T: GarbageCollect + GcBind<'gc, 'a>,
        T::Bound: GarbageCollect,
    {
        debug_assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<T::Bound>());

        let color = Color::White(self.current_white);
        let mut gc_box = Box::new(std::mem::MaybeUninit::uninit());
        gc_box.write(GcBox {
            color: Cell::new(color),
            next: self.all.get(),
            value,
        });
        let ptr = Box::into_raw(gc_box) as *mut GcBox<T::Bound>;
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        self.all.set(Some(unsafe { gc_ptr_to_static(ptr) }));
        self.debt
            .set(self.debt.get() + std::mem::size_of::<GcBox<T>>() as isize);
        Gc::new(ptr)
    }

    pub fn allocate_cell<'a, T>(&'a self, value: T) -> GcCell<'gc, 'a, T::Bound>
    where
        T: GarbageCollect + GcBind<'gc, 'a>,
        T::Bound: GarbageCollect,
    {
        GcCell(self.allocate(GcRefCell::new(value)))
    }

    pub fn allocate_string<'a, 'b, T>(&'a self, string: T) -> LuaString<'gc, 'a>
    where
        T: Into<Cow<'b, [u8]>>,
    {
        let string = string.into();
        let hash = string::calc_str_hash(&string);
        let mut pool = self.string_pool.borrow_mut();
        let entry = pool.raw_entry_mut().from_hash(hash, |k| {
            let gc_box = unsafe { k.as_ref() };
            gc_box.value.as_bytes() == string.as_ref()
        });
        let interned = match entry {
            RawEntryMut::Occupied(entry) => *entry.key(),
            RawEntryMut::Vacant(entry) => {
                let gc = self.allocate(BoxedString(string.into()));
                entry.insert_with_hasher(hash, gc.ptr, (), |k| {
                    let gc_box = unsafe { k.as_ref() };
                    string::calc_str_hash(&gc_box.value)
                });
                gc.ptr
            }
        };
        LuaString(Gc::new(interned))
    }

    pub fn step(&mut self, roots: &RootSet) {
        if self.is_running() && self.debt() > 0 {
            self.step_inner(roots);
        }
    }

    pub fn force_step(&mut self, roots: &RootSet, kbytes: isize) -> bool {
        let did_step = if kbytes == 0 {
            self.set_debt(0);
            self.step(roots);
            true
        } else {
            let debt = kbytes * 1024 + self.debt();
            self.set_debt(debt);
            if debt > 0 {
                self.step(roots);
                true
            } else {
                false
            }
        };
        did_step && self.phase == Phase::Pause
    }

    pub fn full_gc(&mut self, roots: &RootSet) {
        if matches!(self.phase, Phase::Propagate | Phase::Atomic) {
            self.phase = Phase::Sweep;
            self.sweep = self.all.get();
            self.prev_sweep.take();
        }
        while self.phase != Phase::Pause {
            self.do_single_step(roots);
        }
        loop {
            self.do_single_step(roots);
            if self.phase == Phase::Pause {
                break;
            }
        }
        debug_assert_eq!(self.estimate, self.total_bytes());
        loop {
            self.do_single_step(roots);
            if self.phase == Phase::Pause {
                break;
            }
        }
        self.set_debt_for_pause_phase();
    }

    fn step_inner(&mut self, roots: &RootSet) {
        let mut debt = self.debt.get();
        let step_size = 1 << self.step_size.get();
        let step_multiplier = self.step_multiplier.get() | 1; // avoid division by zero
        loop {
            let work = self.do_single_step(roots) / step_multiplier;
            debt -= work as isize;
            if self.phase == Phase::Pause {
                self.set_debt_for_pause_phase();
                return;
            }
            if debt <= -step_size {
                self.set_debt(debt);
                return;
            }
        }
    }

    fn set_debt(&self, debt: isize) {
        self.allocated_bytes
            .set((self.allocated_bytes.get() as isize + self.debt.get() - debt) as usize);
        self.debt.set(debt);
    }

    fn set_debt_for_pause_phase(&self) {
        let estimate = self.estimate / PAUSEADJ;
        let threshold = estimate * self.pause.get();
        let debt = self.allocated_bytes.get() as isize + self.debt.get() - threshold as isize;
        self.set_debt(if debt > 0 { 0 } else { debt });
    }

    fn write_barrier<T: GarbageCollect>(&self, ptr: GcPtr<T>) {
        if self.phase != Phase::Propagate {
            return;
        }

        let gc_box = unsafe { ptr.as_ref() };
        if gc_box.color.get() == Color::Black {
            gc_box.color.set(Color::Gray);
            self.gray_again
                .borrow_mut()
                .push(unsafe { gc_ptr_to_static(ptr) });
        }
    }

    fn propagate_gray(&mut self, ptr: GcPtr<dyn GarbageCollect>) -> usize {
        let gc_box = unsafe { ptr.as_ref() };
        debug_assert_eq!(gc_box.color.get(), Color::Gray);
        gc_box.value.trace(&mut Tracer {
            gray: &mut self.gray,
        });
        gc_box.color.set(Color::Black);
        std::mem::size_of_val(gc_box)
    }

    fn do_single_step(&mut self, roots: &RootSet) -> usize {
        match self.phase {
            Phase::Pause => {
                self.do_pause(roots);
                self.phase = Phase::Propagate;
                WORK2MEM
            }
            Phase::Propagate => {
                let work = self.do_propagate();
                if work == 0 {
                    self.phase = Phase::Atomic;
                }
                work
            }
            Phase::Atomic => {
                let work = self.do_atomic(roots);
                self.phase = Phase::Sweep;
                self.sweep = self.all.get();
                self.prev_sweep.take();
                self.estimate = self.total_bytes();
                work
            }
            Phase::Sweep => {
                let work = self.do_sweep();
                if work == 0 {
                    self.phase = Phase::Pause;
                }
                work
            }
        }
    }

    fn trace_roots(&mut self, roots: &RootSet) {
        let mut tracer = Tracer {
            gray: &mut self.gray,
        };
        for ptr in roots.stack.borrow().iter().flatten() {
            unsafe { ptr.as_ref() }.trace(&mut tracer);
        }
    }

    fn do_pause(&mut self, roots: &RootSet) {
        debug_assert!(self.gray.is_empty());
        debug_assert!(self.gray_again.borrow().is_empty());
        self.trace_roots(roots);
    }

    fn do_propagate(&mut self) -> usize {
        if let Some(ptr) = self.gray.pop() {
            self.propagate_gray(ptr)
        } else {
            0
        }
    }

    fn do_atomic(&mut self, roots: &RootSet) -> usize {
        self.trace_roots(roots);

        let mut work = 0;
        while let Some(ptr) = self.gray.pop() {
            work += self.propagate_gray(ptr);
        }
        std::mem::swap(&mut self.gray, &mut self.gray_again.borrow_mut());
        while let Some(ptr) = self.gray.pop() {
            work += self.propagate_gray(ptr);
        }

        self.current_white = !self.current_white;
        work
    }

    fn do_sweep(&mut self) -> usize {
        let mut count = 0;
        let mut work = 0;
        let mut debt = self.debt.get();

        let old_debt = debt;
        let current_white = Color::White(self.current_white);
        let other_white = Color::White(!self.current_white);

        let mut finalizer = Finalizer {
            string_pool: &mut self.string_pool.borrow_mut(),
        };

        while let Some(ptr) = self.sweep {
            let gc_box = unsafe { ptr.as_ref() };
            work += std::mem::size_of_val(gc_box);
            if gc_box.color.get() == other_white {
                if let Some(prev) = &mut self.prev_sweep {
                    let prev = unsafe { prev.as_mut() };
                    prev.next = gc_box.next;
                } else {
                    self.all.set(gc_box.next);
                }
                self.sweep = gc_box.next;
                debt -= std::mem::size_of_val(gc_box) as isize;

                gc_box.value.finalize(&mut finalizer);
                unsafe { Box::from_raw(ptr.as_ptr()) };
            } else {
                debug_assert_eq!(gc_box.color.get(), Color::Black);
                gc_box.color.set(current_white);
                self.prev_sweep = Some(ptr);
                self.sweep = gc_box.next;
            }
            count += 1;
            if count >= GCSWEEPMAX {
                break;
            }
        }

        self.debt.set(debt);
        self.estimate = (self.estimate as isize + debt - old_debt) as usize;
        work
    }
}

unsafe fn gc_ptr_to_static<'a>(ptr: GcPtr<dyn GarbageCollect + 'a>) -> GcPtr<dyn GarbageCollect> {
    std::mem::transmute(ptr)
}

unsafe fn bind_value<'gc, 'a, T: GcBind<'gc, 'a>>(x: T) -> T::Bound {
    debug_assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<T::Bound>());
    let mut v = MaybeUninit::uninit();
    (v.as_mut_ptr() as *mut T).write(x);
    v.assume_init()
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Phase {
    Pause,
    Propagate,
    Atomic,
    Sweep,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Color {
    White(bool),
    Black,
    Gray,
}

struct GcBox<T: ?Sized> {
    color: Cell<Color>,
    next: Option<GcPtr<dyn GarbageCollect>>,
    value: T,
}

pub struct Gc<'gc, 'a, T: 'a + ?Sized> {
    ptr: NonNull<GcBox<T>>,
    invariant: PhantomData<Cell<(&'gc (), &'a ())>>,
}

impl<T> Clone for Gc<'_, '_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<'_, '_, T> {}

impl<T: Debug> Debug for Gc<'_, '_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("Gc").field(self.deref()).finish()
    }
}

impl<T> Deref for Gc<'_, '_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let gc_box = unsafe { self.ptr.as_ref() };
        &gc_box.value
    }
}

impl<T> AsRef<T> for Gc<'_, '_, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<'gc, T: PartialEq> PartialEq for Gc<'gc, '_, T> {
    fn eq(&self, other: &Gc<'gc, '_, T>) -> bool {
        **self == **other
    }
}

impl<'gc, T: PartialOrd> PartialOrd for Gc<'gc, '_, T> {
    fn partial_cmp(&self, other: &Gc<'gc, '_, T>) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}

impl<T: Hash> Hash for Gc<'_, '_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

unsafe impl<T: GarbageCollect> Trace for Gc<'_, '_, T> {
    fn trace(&self, tracer: &mut Tracer) {
        let gc_box = unsafe { self.ptr.as_ref() };
        let color = &gc_box.color;
        if let Color::White(_) = color.get() {
            if T::needs_trace() {
                color.set(Color::Gray);
                tracer.gray.push(unsafe { gc_ptr_to_static(self.ptr) });
            } else {
                color.set(Color::Black);
            }
        }
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for Gc<'_, '_, T> {}

unsafe impl<'a, 'gc: 'a, T: GcBind<'gc, 'a>> GcBind<'gc, 'a> for Gc<'gc, '_, T> {
    type Bound = Gc<'gc, 'a, T::Bound>;
}

impl<'gc, T> Gc<'gc, '_, T> {
    fn new(ptr: GcPtr<T>) -> Self {
        Self {
            ptr,
            invariant: PhantomData,
        }
    }

    pub fn ptr_eq(this: Self, other: Gc<'gc, '_, T>) -> bool {
        this.ptr.as_ptr() == other.ptr.as_ptr()
    }

    pub fn as_ptr(this: Self) -> *const T {
        let gc_box = unsafe { this.ptr.as_ref() };
        &gc_box.value as *const T
    }
}

#[derive(Debug)]
struct GcRefCell<T>(RefCell<T>);

unsafe impl<T: Trace> Trace for GcRefCell<T> {
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    fn trace(&self, tracer: &mut Tracer) {
        self.0.borrow().trace(tracer);
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for GcRefCell<T> {}

unsafe impl<'gc, 'a, T: GcBind<'gc, 'a>> GcBind<'gc, 'a> for GcRefCell<T> {
    type Bound = GcRefCell<T::Bound>;
}

impl<T> GcRefCell<T> {
    fn new(value: T) -> Self {
        Self(RefCell::new(value))
    }
}

pub struct GcCell<'gc, 'a, T: 'a>(Gc<'gc, 'a, GcRefCell<T>>);

impl<T> Clone for GcCell<'_, '_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for GcCell<'_, '_, T> {}

impl<T: Debug> Debug for GcCell<'_, '_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("GcCell").field(&self.0.deref().0).finish()
    }
}

unsafe impl<T: GarbageCollect> GarbageCollect for GcCell<'_, '_, T> {}

unsafe impl<T: GarbageCollect> Trace for GcCell<'_, '_, T> {
    fn trace(&self, tracer: &mut Tracer) {
        Gc::trace(&self.0, tracer);
    }
}

impl<'gc, T> GcCell<'gc, '_, T> {
    pub fn ptr_eq(self, other: GcCell<'gc, '_, T>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }

    pub fn as_ptr(self) -> *const T {
        let gc_box = unsafe { self.0.ptr.as_ref() };
        gc_box.value.0.as_ptr()
    }
}

unsafe impl<'a, 'gc: 'a, T: GcBind<'gc, 'a>> GcBind<'gc, 'a> for GcCell<'gc, '_, T> {
    type Bound = GcCell<'gc, 'a, T::Bound>;
}

impl<'gc, 'a, T> GcCell<'gc, '_, T>
where
    T: GarbageCollect + GcBind<'gc, 'a>,
{
    #[allow(unused_variables)]
    pub fn borrow(self, gc: &'a GcContext<'gc>) -> Ref<'a, T::Bound> {
        let ptr = self.0.ptr.as_ptr() as *mut GcBox<GcRefCell<T::Bound>>;
        unsafe { &*ptr }.value.0.borrow()
    }

    pub fn borrow_mut(self, gc: &'a GcContext<'gc>) -> RefMut<'a, T::Bound> {
        let ptr = self.0.ptr.as_ptr() as *mut GcBox<GcRefCell<T::Bound>>;
        let b = unsafe { &*ptr }.value.0.borrow_mut();
        gc.write_barrier(self.0.ptr);
        b
    }
}

#[macro_export]
macro_rules! rebind {
    ($gc:expr, $value:expr) => {
        unsafe {
            let v = $crate::gc::unbind($value);
            $crate::gc::bind($gc, v)
        }
    };
}

#[doc(hidden)]
pub unsafe fn unbind<'gc, 'a, T: GcBind<'gc, 'a>>(x: T) -> T::Bound {
    bind_value(x)
}

#[doc(hidden)]
#[allow(unused_variables)]
pub unsafe fn bind<'gc, 'a, T: GcBind<'gc, 'a>>(gc: &'a GcContext<'gc>, x: T) -> T::Bound {
    bind_value(x)
}
