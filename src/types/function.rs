use super::{thread::StackWindow, LuaThread};
use crate::{
    gc::{GarbageCollect, Gc, GcCell, GcContext, Tracer},
    runtime::{ErrorKind, Instruction, Vm},
    types::{LuaString, Value},
};
use std::{fmt::Debug, hash::Hash, ops::RangeInclusive};

pub enum Action {
    Return {
        num_results: usize,
    },
    Call {
        callee_bottom: usize,
        continuation: Box<NativeClosureFn>,
    },
    TailCall {
        num_args: usize,
    },
}

pub type NativeFunctionPtr = for<'gc> fn(
    &'gc GcContext,
    &mut Vm<'gc>,
    GcCell<'gc, LuaThread<'gc>>,
    StackWindow,
) -> Result<Action, ErrorKind>;

#[derive(Clone, Copy)]
pub struct NativeFunction(pub NativeFunctionPtr);

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

    pub fn as_ptr(&self) -> *const () {
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
    pub proto: Gc<'gc, LuaClosureProto<'gc>>,
    pub upvalues: Vec<GcCell<'gc, Upvalue<'gc>>>,
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

pub type NativeClosureFn = dyn for<'gc> Fn(
    &'gc GcContext,
    &mut Vm<'gc>,
    GcCell<'gc, LuaThread<'gc>>,
    StackWindow,
) -> Result<Action, ErrorKind>;

pub struct NativeClosure<'gc> {
    func: Box<NativeClosureFn>,
    upvalues: Box<[Value<'gc>]>,
}

impl std::fmt::Debug for NativeClosure<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeClosure")
            .field("upvalues", &self.upvalues)
            .finish()
    }
}

unsafe impl GarbageCollect for NativeClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.upvalues.trace(tracer);
    }
}

impl<'gc> NativeClosure<'gc> {
    pub fn new<T>(func: T) -> Self
    where
        T: 'static
            + for<'a> Fn(
                &'a GcContext,
                &mut Vm<'a>,
                GcCell<'a, LuaThread<'a>>,
                StackWindow,
            ) -> Result<Action, ErrorKind>,
    {
        Self {
            func: Box::new(func),
            upvalues: Default::default(),
        }
    }

    pub fn with_upvalues<T, U>(func: T, upvalues: U) -> Self
    where
        T: 'static
            + for<'a> Fn(
                &'a GcContext,
                &mut Vm<'a>,
                GcCell<'a, LuaThread<'a>>,
                StackWindow,
            ) -> Result<Action, ErrorKind>,
        U: Into<Box<[Value<'gc>]>>,
    {
        Self {
            func: Box::new(func),
            upvalues: upvalues.into(),
        }
    }

    pub fn function(&self) -> &NativeClosureFn {
        &self.func
    }

    pub fn upvalues(&self) -> &[Value<'gc>] {
        &self.upvalues
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue<'gc> {
    Open(usize),
    Closed(Value<'gc>),
}

impl<'gc> From<Value<'gc>> for Upvalue<'gc> {
    fn from(x: Value<'gc>) -> Self {
        Self::Closed(x)
    }
}

unsafe impl GarbageCollect for Upvalue<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        if let Self::Closed(x) = self {
            x.trace(tracer);
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
