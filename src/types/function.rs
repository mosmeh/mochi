use crate::{
    gc::{GarbageCollect, Gc, GcCell, Tracer},
    types::{LuaString, Value},
    vm::{ErrorKind, Instruction, Vm},
};
use std::{fmt::Debug, hash::Hash, ops::Range};

#[derive(Clone)]
pub struct StackWindow(pub(crate) Range<usize>);

pub type NativeFunctionPtr = fn(&mut Vm, StackWindow) -> Result<usize, ErrorKind>;

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
    Lines(Range<u32>),
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

pub type NativeClosureFn = dyn Fn(&mut Vm, StackWindow) -> Result<usize, ErrorKind>;

pub struct NativeClosure(pub Box<NativeClosureFn>);

impl std::fmt::Debug for NativeClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("NativeClosure").finish()
    }
}

unsafe impl GarbageCollect for NativeClosure {
    fn needs_trace() -> bool {
        false
    }
}

impl NativeClosure {
    pub fn new<T>(func: T) -> Self
    where
        T: 'static + Fn(&mut Vm, StackWindow) -> Result<usize, ErrorKind>,
    {
        Self(Box::new(func))
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

#[derive(Debug, Clone)]
pub struct UpvalueDescription {
    pub in_stack: bool,
    pub index: u8,
}
