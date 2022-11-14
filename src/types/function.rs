use crate::{
    gc::{GarbageCollect, Gc, GcCell, GcContext, GcLifetime, RootSet, Tracer},
    runtime::{ErrorKind, Instruction, Vm},
    types::{LuaString, LuaThread, Value},
};
use std::{fmt::Debug, hash::Hash, ops::RangeInclusive};

pub type NativeFunctionPtr = for<'gc, 'a> fn(
    &'a mut GcContext<'gc>,
    &RootSet<'gc>,
    GcCell<'gc, '_, Vm<'gc, '_>>,
    &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind>;

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
pub struct LuaClosureProto<'gc, 'a> {
    pub max_stack_size: u8,
    pub lines_defined: LineRange,
    pub constants: Box<[Value<'gc, 'a>]>,
    pub code: Box<[Instruction]>,
    pub protos: Box<[Gc<'gc, 'a, LuaClosureProto<'gc, 'a>>]>,
    pub upvalues: Box<[UpvalueDescription]>,
    pub source: LuaString<'gc, 'a>,
}

unsafe impl GarbageCollect for LuaClosureProto<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.constants.trace(tracer);
        self.protos.trace(tracer);
        self.source.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for LuaClosureProto<'gc, '_> {
    type Aged = LuaClosureProto<'gc, 'a>;
}

#[derive(Debug, Clone)]
pub enum LineRange {
    File,
    Lines(RangeInclusive<u32>),
}

#[derive(Debug, Clone)]
pub struct LuaClosure<'gc, 'a> {
    pub(crate) proto: Gc<'gc, 'a, LuaClosureProto<'gc, 'a>>,
    pub(crate) upvalues: Vec<GcCell<'gc, 'a, Upvalue<'gc, 'a>>>,
}

unsafe impl GarbageCollect for LuaClosure<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.proto.trace(tracer);
        self.upvalues.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for LuaClosure<'gc, '_> {
    type Aged = LuaClosure<'gc, 'a>;
}

impl<'gc, 'a> From<Gc<'gc, 'a, LuaClosureProto<'gc, 'a>>> for LuaClosure<'gc, 'a> {
    fn from(proto: Gc<'gc, 'a, LuaClosureProto<'gc, 'a>>) -> Self {
        Self {
            proto,
            upvalues: Default::default(),
        }
    }
}

trait NativeClosureFn<'gc>: GarbageCollect {
    fn call<'a>(
        &self,
        gc: &'a mut GcContext<'gc>,
        roots: &RootSet<'gc>,
        vm: GcCell<'gc, '_, Vm<'gc, '_>>,
        args: &[Value<'gc, 'a>],
    ) -> Result<Vec<Value<'gc, 'a>>, ErrorKind>;
}

pub struct NativeClosure<'gc, 'a>(Box<dyn NativeClosureFn<'gc> + 'a>);

impl std::fmt::Debug for NativeClosure<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeClosure").finish()
    }
}

unsafe impl GarbageCollect for NativeClosure<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for NativeClosure<'gc, '_> {
    type Aged = NativeClosure<'gc, 'a>;
}

impl<'a, 'gc: 'a> NativeClosure<'gc, 'a> {
    pub fn new<F>(f: F) -> Self
    where
        F: 'static
            + for<'b> Fn(
                &'b mut GcContext<'gc>,
                &RootSet<'gc>,
                GcCell<'gc, '_, Vm<'gc, '_>>,
                &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind>,
    {
        struct SimpleNativeClosure<F>(F);

        impl<'gc, F> NativeClosureFn<'gc> for SimpleNativeClosure<F>
        where
            F: for<'b> Fn(
                &'b mut GcContext<'gc>,
                &RootSet<'gc>,
                GcCell<'gc, '_, Vm<'gc, '_>>,
                &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind>,
        {
            fn call<'b>(
                &self,
                gc: &'b mut GcContext<'gc>,
                roots: &RootSet<'gc>,
                vm: GcCell<'gc, '_, Vm<'gc, '_>>,
                args: &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind> {
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
            + for<'b> Fn(
                &'b mut GcContext<'gc>,
                &RootSet<'gc>,
                GcCell<'gc, '_, Vm<'gc, '_>>,
                &U,
                &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind>,
        U: 'a + GarbageCollect,
    {
        struct UpvalueNativeClosure<F, U> {
            f: F,
            upvalue: U,
        }

        impl<'gc, F, U> NativeClosureFn<'gc> for UpvalueNativeClosure<F, U>
        where
            F: for<'b> Fn(
                &'b mut GcContext<'gc>,
                &RootSet<'gc>,
                GcCell<'gc, '_, Vm<'gc, '_>>,
                &U,
                &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind>,
            U: GarbageCollect,
        {
            fn call<'b>(
                &self,
                gc: &'b mut GcContext<'gc>,
                roots: &RootSet<'gc>,
                vm: GcCell<'gc, '_, Vm<'gc, '_>>,
                args: &[Value<'gc, 'b>],
            ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind> {
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

    pub fn call<'b>(
        &self,
        gc: &'b mut GcContext<'gc>,
        roots: &RootSet<'gc>,
        vm: GcCell<'gc, '_, Vm<'gc, '_>>,
        args: &[Value<'gc, 'b>],
    ) -> Result<Vec<Value<'gc, 'b>>, ErrorKind> {
        (self.0).call(gc, roots, vm, args)
    }
}

#[derive(Debug)]
pub enum Upvalue<'gc, 'a> {
    Open {
        thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
        index: usize,
    },
    Closed(Value<'gc, 'a>),
}

impl<'gc, 'a> From<Value<'gc, 'a>> for Upvalue<'gc, 'a> {
    fn from(x: Value<'gc, 'a>) -> Self {
        Self::Closed(x)
    }
}

unsafe impl GarbageCollect for Upvalue<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Self::Open { thread, .. } => thread.trace(tracer),
            Self::Closed(value) => value.trace(tracer),
        }
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for Upvalue<'gc, '_> {
    type Aged = Upvalue<'gc, 'a>;
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
