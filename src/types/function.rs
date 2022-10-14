use crate::{
    gc::{GarbageCollect, Gc, GcCell, GcContext, Tracer},
    runtime::{Action, ErrorKind, Instruction, Vm},
    types::{LuaString, LuaThread, Value},
};
use std::{fmt::Debug, hash::Hash, ops::RangeInclusive};

pub type NativeFunctionPtr =
    for<'gc> fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>;

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

impl<'gc> From<Gc<'gc, LuaClosureProto<'gc>>> for LuaClosure<'gc> {
    fn from(proto: Gc<'gc, LuaClosureProto<'gc>>) -> Self {
        Self {
            proto,
            upvalues: Default::default(),
        }
    }
}

trait NativeClosureFn<'gc>: GarbageCollect {
    fn call(
        &self,
        gc: &'gc GcContext,
        vm: &mut Vm<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<Action<'gc>, ErrorKind>;
}

pub struct NativeClosure<'gc>(Box<dyn NativeClosureFn<'gc> + 'gc>);

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

impl<'gc> NativeClosure<'gc> {
    pub fn new<F>(f: F) -> Self
    where
        F: 'static
            + Fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
    {
        struct SimpleNativeClosure<F>(F);

        impl<'gc, F> NativeClosureFn<'gc> for SimpleNativeClosure<F>
        where
            F: Fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
        {
            fn call(
                &self,
                gc: &'gc GcContext,
                vm: &mut Vm<'gc>,
                args: Vec<Value<'gc>>,
            ) -> Result<Action<'gc>, ErrorKind> {
                (self.0)(gc, vm, args)
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
            + Fn(&'gc GcContext, &mut Vm<'gc>, &U, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
        U: 'gc + GarbageCollect,
    {
        struct UpvalueNativeClosure<F, U> {
            f: F,
            upvalue: U,
        }

        impl<'gc, F, U> NativeClosureFn<'gc> for UpvalueNativeClosure<F, U>
        where
            F: Fn(
                &'gc GcContext,
                &mut Vm<'gc>,
                &U,
                Vec<Value<'gc>>,
            ) -> Result<Action<'gc>, ErrorKind>,
            U: 'gc + GarbageCollect,
        {
            fn call(
                &self,
                gc: &'gc GcContext,
                vm: &mut Vm<'gc>,
                args: Vec<Value<'gc>>,
            ) -> Result<Action<'gc>, ErrorKind> {
                (self.f)(gc, vm, &self.upvalue, args)
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

    pub fn call(
        &self,
        gc: &'gc GcContext,
        vm: &mut Vm<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<Action<'gc>, ErrorKind> {
        (self.0).call(gc, vm, args)
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpvalueDescription {
    Register(RegisterIndex),
    Upvalue(UpvalueIndex),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RegisterIndex(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueIndex(pub u8);
