mod trace;

pub use trace::{Trace, Tracer};

use crate::types::Value;
use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
};

const GCSWEEPMAX: i32 = 100;
const PAUSEADJ: usize = 100;
const WORK2MEM: usize = std::mem::size_of::<GcBox<Value>>();

type GcPtr<T> = NonNull<GcBox<T>>;

fn into_ptr_to_static<'a>(ptr: GcPtr<dyn Trace + 'a>) -> GcPtr<dyn Trace> {
    unsafe { std::mem::transmute(ptr) }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Phase {
    Pause,
    Propagate,
    Atomic,
    Sweep,
}

pub struct GcHeap {
    pause: usize,
    step_multiplier: usize,
    step_size: usize,
    phase: Cell<Phase>,
    current_white: Cell<bool>,
    total_bytes: Cell<usize>,
    debt: Cell<isize>,
    estimate: Cell<usize>,
    all: Cell<Option<GcPtr<dyn Trace>>>,
    sweep: Cell<Option<GcPtr<dyn Trace>>>,
    prev_sweep: Cell<Option<GcPtr<dyn Trace>>>,
    gray: RefCell<Vec<GcPtr<dyn Trace>>>,
    gray_again: RefCell<Vec<GcPtr<dyn Trace>>>,
}

impl Default for GcHeap {
    fn default() -> Self {
        Self {
            pause: 200,
            step_multiplier: 100,
            step_size: 13,
            phase: Cell::new(Phase::Pause),
            current_white: Default::default(),
            total_bytes: Default::default(),
            debt: Default::default(),
            estimate: Default::default(),
            all: Default::default(),
            sweep: Default::default(),
            prev_sweep: Default::default(),
            gray: Default::default(),
            gray_again: Default::default(),
        }
    }
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        let mut it = self.all.get();
        while let Some(ptr) = it {
            let gc_box = unsafe { ptr.as_ref() };
            it = gc_box.next;
            unsafe { Box::from_raw(ptr.as_ptr()) };
        }
    }
}

impl GcHeap {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate<T: Trace>(&self, value: T) -> Gc<T> {
        let color = Color::White(self.current_white.get());
        let gc_box = Box::new(GcBox {
            color: Cell::new(color),
            next: self.all.take(),
            value,
        });
        let ptr = Box::into_raw(gc_box);
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        self.all.set(Some(into_ptr_to_static(ptr)));
        self.debt
            .set(self.debt.get() + std::mem::size_of::<GcBox<T>>() as isize);
        Gc {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn allocate_cell<T: Trace>(&self, value: T) -> GcCell<T> {
        GcCell(self.allocate(RefCell::new(value)))
    }

    /// # Safety
    /// Only `Gc` and `GcCell` traced from `root`, including those on stack,
    /// should be dereferenced after calling this function.
    pub unsafe fn step<T: Trace>(&self, root: &T) {
        let mut debt = self.debt.get();
        if debt <= 0 {
            return;
        }
        let step_size = 1 << self.step_size;
        loop {
            let work = self.do_single_step(root) / self.step_multiplier;
            debt -= work as isize;
            if self.phase.get() == Phase::Pause {
                let estimate = self.estimate.get() / PAUSEADJ;
                let threshold = estimate * self.pause;
                debt = self.total_bytes.get() as isize + self.debt.get() - threshold as isize;
                if debt > 0 {
                    debt = 0;
                }
                break;
            }
            if debt <= -step_size {
                break;
            }
        }
        self.total_bytes
            .set((self.total_bytes.get() as isize + self.debt.get() - debt) as usize);
        self.debt.set(debt);
    }

    pub fn leak_all(&mut self) {
        self.phase = Cell::new(Phase::Pause);
        self.total_bytes = Default::default();
        self.debt = Default::default();
        self.estimate = Default::default();
        self.all = Default::default();
        self.sweep = Default::default();
        self.prev_sweep = Default::default();
        self.gray.borrow_mut().clear();
        self.gray_again.borrow_mut().clear();
    }

    fn write_barrier<T: Trace>(&self, ptr: GcPtr<T>) {
        if self.phase.get() != Phase::Propagate {
            return;
        }

        let gc_box = unsafe { ptr.as_ref() };
        if gc_box.color.get() == Color::Black {
            gc_box.color.set(Color::Gray);
            self.gray_again.borrow_mut().push(into_ptr_to_static(ptr));
        }
    }

    fn do_single_step<T: Trace>(&self, root: &T) -> usize {
        match self.phase.get() {
            Phase::Pause => {
                self.do_pause(root);
                self.phase.set(Phase::Propagate);
                WORK2MEM
            }
            Phase::Propagate => {
                let work = self.do_propagate();
                if work == 0 {
                    self.phase.set(Phase::Atomic);
                }
                work
            }
            Phase::Atomic => {
                let work = self.do_atomic(root);
                self.phase.set(Phase::Sweep);
                self.estimate
                    .set((self.total_bytes.get() as isize + self.debt.get()) as usize);
                self.sweep.set(self.all.get());
                self.prev_sweep.take();
                work
            }
            Phase::Sweep => {
                let work = self.do_sweep();
                if work == 0 {
                    self.phase.set(Phase::Pause);
                }
                work
            }
        }
    }

    fn do_pause<T: Trace>(&self, root: &T) {
        let mut gray = self.gray.borrow_mut();
        gray.clear();
        self.gray_again.borrow_mut().clear();
        root.trace(&mut Tracer { gray: &mut gray });
    }

    fn do_propagate(&self) -> usize {
        let mut gray = self.gray.borrow_mut();
        if let Some(ptr) = gray.pop() {
            let gc_box = unsafe { ptr.as_ref() };
            gc_box.value.trace(&mut Tracer { gray: &mut gray });
            gc_box.color.set(Color::Black);
            std::mem::size_of_val(gc_box)
        } else {
            0
        }
    }

    fn do_atomic<T: Trace>(&self, root: &T) -> usize {
        let mut gray = self.gray.borrow_mut();
        root.trace(&mut Tracer { gray: &mut gray });

        let mut work = 0;
        while let Some(ptr) = gray.pop() {
            let gc_box = unsafe { ptr.as_ref() };
            gc_box.value.trace(&mut Tracer { gray: &mut gray });
            gc_box.color.set(Color::Black);
            work += std::mem::size_of_val(gc_box)
        }

        let mut gray_again = self.gray_again.borrow_mut();
        while let Some(ptr) = gray_again.pop() {
            let gc_box = unsafe { ptr.as_ref() };
            gc_box.value.trace(&mut Tracer { gray: &mut gray });
            gc_box.color.set(Color::Black);
            work += std::mem::size_of_val(gc_box)
        }

        self.current_white.set(!self.current_white.get());
        work
    }

    fn do_sweep(&self) -> usize {
        let mut count = 0;
        let mut work = 0;
        let mut debt = self.debt.get();

        let old_debt = debt;
        let current_white = Color::White(self.current_white.get());
        let other_white = Color::White(!self.current_white.get());

        while let Some(ptr) = self.sweep.get() {
            let gc_box = unsafe { ptr.as_ref() };
            work += std::mem::size_of_val(gc_box);
            if gc_box.color.get() == other_white {
                if let Some(prev) = &mut self.prev_sweep.get() {
                    let prev = unsafe { prev.as_mut() };
                    prev.next = gc_box.next;
                } else {
                    self.all.set(gc_box.next);
                }
                self.sweep.set(gc_box.next);
                debt -= std::mem::size_of_val(gc_box) as isize;
                unsafe { Box::from_raw(ptr.as_ptr()) };
            } else {
                gc_box.color.set(current_white);
                self.prev_sweep.set(Some(ptr));
                self.sweep.set(gc_box.next);
            }
            count += 1;
            if count >= GCSWEEPMAX {
                break;
            }
        }

        self.debt.set(debt);
        self.estimate
            .set((self.estimate.get() as isize + debt - old_debt) as usize);
        work
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Color {
    White(bool),
    Black,
    Gray,
}

struct GcBox<T: ?Sized + Trace> {
    color: Cell<Color>,
    next: Option<GcPtr<dyn Trace>>,
    value: T,
}

pub struct Gc<'a, T: ?Sized + Trace + 'a> {
    ptr: NonNull<GcBox<T>>,
    phantom: PhantomData<&'a GcHeap>,
}

impl<T: Trace> Clone for Gc<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Trace> Copy for Gc<'_, T> {}

impl<T: Trace> Deref for Gc<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        let gc_box = unsafe { &self.ptr.as_ref() };
        &gc_box.value
    }
}

impl<T: Trace + Debug> Debug for Gc<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("Gc").field(self.deref()).finish()
    }
}

impl<T: Trace + PartialEq> PartialEq for Gc<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Trace + PartialOrd> PartialOrd for Gc<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}

impl<T: Trace + Hash> Hash for Gc<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T: Trace> AsRef<T> for Gc<'_, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

unsafe impl<T: Trace> Trace for Gc<'_, T> {
    fn trace(&self, tracer: &mut Tracer) {
        let gc_box = unsafe { self.ptr.as_ref() };
        let color = &gc_box.color;
        if matches!(color.get(), Color::White(_)) {
            if T::needs_trace() {
                color.set(Color::Gray);
                tracer.gray.push(into_ptr_to_static(self.ptr));
            } else {
                color.set(Color::Black);
            }
        }
    }
}

impl<T: Trace> Gc<'_, T> {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr.as_ptr() == other.ptr.as_ptr()
    }

    pub fn as_ptr(&self) -> *const T {
        let gc_box = unsafe { self.ptr.as_ref() };
        &gc_box.value as *const T
    }
}

pub struct GcCell<'a, T: Trace + 'a>(Gc<'a, RefCell<T>>);

impl<T: Trace> Clone for GcCell<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Trace> Copy for GcCell<'_, T> {}

impl<T: Trace + Debug> Debug for GcCell<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("GcCell").field(&*self.0).finish()
    }
}

unsafe impl<T: Trace> Trace for GcCell<'_, T> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

impl<T: Trace> GcCell<'_, T> {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }

    pub fn as_ptr(&self) -> *const T {
        let gc_box = unsafe { self.0.ptr.as_ref() };
        gc_box.value.as_ptr()
    }

    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self, heap: &GcHeap) -> RefMut<T> {
        let b = self.0.deref().borrow_mut();
        heap.write_barrier(self.0.ptr);
        b
    }
}
