use crate::{
    gc::{GarbageCollect, Gc, GcCell, GcContext, GcLifetime, RootSet, Tracer},
    runtime::{ErrorKind, Instruction, Vm},
    types::{LuaString, LuaThread, Value},
};
use std::{fmt::Debug, hash::Hash, ops::RangeInclusive};

pub type NativeFunctionPtr = for<'gc> fn(
    &'gc mut GcContext,
    &RootSet,
    GcCell<Vm>,
    &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind>;

#[derive(Clone, Copy)]
pub struct NativeFunction(pub(crate) NativeFunctionPtr);

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NativeFunction")
            .field(&self.as_ptr())
            .finish()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl Hash for NativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}

impl NativeFunction {
    pub fn new(x: NativeFunctionPtr) -> Self {
        Self(x)
    }

    pub(crate) fn as_ptr(&self) -> *const () {
        self.0 as *const ()
    }
}

#[derive(Debug, Clone)]
pub struct LuaClosureProto<'gc> {
    pub max_stack_size: u8,
    pub lines_defined: LineRange,
    pub constants: Box<[Value<'gc>]>,
    pub code: Box<[Instruction]>,
    pub protos: Box<[Gc<'gc, LuaClosureProto<'gc>>]>,
    pub upvalues: Box<[UpvalueDescription]>,
    pub source: LuaString<'gc>,
}

unsafe impl GarbageCollect for LuaClosureProto<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.constants.trace(tracer);
        self.protos.trace(tracer);
        self.source.trace(tracer);
    }
}

unsafe impl<'a> GcLifetime<'a> for LuaClosureProto<'_> {
    type Aged = LuaClosureProto<'a>;
}

#[derive(Debug, Clone)]
pub enum LineRange {
    File,
    Lines(RangeInclusive<u32>),
}

#[derive(Debug, Clone)]
pub struct LuaClosure<'gc> {
    pub(crate) proto: Gc<'gc, LuaClosureProto<'gc>>,
    pub(crate) upvalues: Vec<GcCell<'gc, Upvalue<'gc>>>,
}

unsafe impl GarbageCollect for LuaClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.proto.trace(tracer);
        self.upvalues.trace(tracer);
    }
}

unsafe impl<'a> GcLifetime<'a> for LuaClosure<'_> {
    type Aged = LuaClosure<'a>;
}

impl<'gc> From<Gc<'gc, LuaClosureProto<'gc>>> for LuaClosure<'gc> {
    fn from(proto: Gc<'gc, LuaClosureProto<'gc>>) -> Self {
        Self {
            proto,
            upvalues: Default::default(),
        }
    }
}

trait NativeClosureFn: GarbageCollect {
    fn call<'gc>(
        &self,
        gc: &'gc mut GcContext,
        roots: &RootSet,
        vm: GcCell<Vm>,
        args: &[Value<'gc>],
    ) -> Result<Vec<Value<'gc>>, ErrorKind>;
}

pub struct NativeClosure<'gc>(Box<dyn NativeClosureFn + 'gc>);

impl std::fmt::Debug for NativeClosure<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeClosure").finish()
    }
}

unsafe impl GarbageCollect for NativeClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

unsafe impl<'a> GcLifetime<'a> for NativeClosure<'_> {
    type Aged = NativeClosure<'a>;
}

impl<'gc> NativeClosure<'gc> {
    pub fn new<F>(f: F) -> Self
    where
        F: 'static
            + for<'a> Fn(
                &'a mut GcContext,
                &RootSet,
                GcCell<Vm>,
                &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind>,
    {
        struct SimpleNativeClosure<F>(F);

        impl<F> NativeClosureFn for SimpleNativeClosure<F>
        where
            F: for<'a> Fn(
                &'a mut GcContext,
                &RootSet,
                GcCell<Vm>,
                &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind>,
        {
            fn call<'a>(
                &self,
                gc: &'a mut GcContext,
                roots: &RootSet,
                vm: GcCell<Vm>,
                args: &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind> {
                (self.0)(gc, roots, vm, args)
            }
        }

        unsafe impl<F> GarbageCollect for SimpleNativeClosure<F> {
            fn needs_trace() -> bool {
                false
            }
        }

        Self(Box::new(SimpleNativeClosure(f)))
    }

    pub fn with_upvalue<F, U>(upvalue: U, f: F) -> Self
    where
        F: 'static
            + for<'a> Fn(
                &'a mut GcContext,
                &RootSet,
                GcCell<Vm>,
                &U,
                &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind>,
        U: 'gc + GarbageCollect,
    {
        struct UpvalueNativeClosure<F, U> {
            f: F,
            upvalue: U,
        }

        impl<F, U> NativeClosureFn for UpvalueNativeClosure<F, U>
        where
            F: for<'a> Fn(
                &'a mut GcContext,
                &RootSet,
                GcCell<Vm>,
                &U,
                &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind>,
            U: GarbageCollect,
        {
            fn call<'a>(
                &self,
                gc: &'a mut GcContext,
                roots: &RootSet,
                vm: GcCell<Vm>,
                args: &[Value<'a>],
            ) -> Result<Vec<Value<'a>>, ErrorKind> {
                (self.f)(gc, roots, vm, &self.upvalue, args)
            }
        }

        unsafe impl<F, U: GarbageCollect> GarbageCollect for UpvalueNativeClosure<F, U> {
            fn needs_trace() -> bool {
                U::needs_trace()
            }

            fn trace(&self, tracer: &mut Tracer) {
                self.upvalue.trace(tracer);
            }
        }

        Self(Box::new(UpvalueNativeClosure { f, upvalue }))
    }

    pub fn call<'a>(
        &self,
        gc: &'a mut GcContext,
        roots: &RootSet,
        vm: GcCell<Vm>,
        args: &[Value<'a>],
    ) -> Result<Vec<Value<'a>>, ErrorKind> {
        (self.0).call(gc, roots, vm, args)
    }
}

#[derive(Debug)]
pub enum Upvalue<'gc> {
    Open {
        thread: GcCell<'gc, LuaThread<'gc>>,
        index: usize,
    },
    Closed(Value<'gc>),
}

impl<'gc> From<Value<'gc>> for Upvalue<'gc> {
    fn from(x: Value<'gc>) -> Self {
        Self::Closed(x)
    }
}

unsafe impl GarbageCollect for Upvalue<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Self::Open { thread, .. } => thread.trace(tracer),
            Self::Closed(value) => value.trace(tracer),
        }
    }
}

unsafe impl<'a> GcLifetime<'a> for Upvalue<'_> {
    type Aged = Upvalue<'a>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UpvalueDescription {
    Register(RegisterIndex),
    Upvalue(UpvalueIndex),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegisterIndex(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UpvalueIndex(pub u8);
