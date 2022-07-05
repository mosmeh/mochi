use crate::{
    gc::{Gc, GcCell, GcHeap, Trace, Tracer},
    types::{LuaString, Value},
    vm::{ErrorKind, Instruction, Vm},
};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct LuaClosureProto<'a> {
    pub max_stack_size: u8,
    pub lines_defined: LineRange,
    pub constants: Vec<Value<'a>>,
    pub code: Vec<Instruction>,
    pub protos: Vec<Gc<'a, LuaClosureProto<'a>>>,
    pub upvalues: Vec<UpvalueDescription>,
    pub source: Gc<'a, LuaString>,
}

unsafe impl Trace for LuaClosureProto<'_> {
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
pub struct LuaClosure<'a> {
    pub proto: Gc<'a, LuaClosureProto<'a>>,
    pub upvalues: Vec<GcCell<'a, Upvalue<'a>>>,
}

unsafe impl Trace for LuaClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.proto.trace(tracer);
        self.upvalues.trace(tracer);
    }
}

#[derive(Clone)]
pub struct StackKey(pub(crate) Range<usize>);

pub type NativeClosureFn =
    dyn for<'a> Fn(&'a GcHeap, &mut Vm<'a>, StackKey) -> Result<usize, ErrorKind>;

pub struct NativeClosure(pub Box<NativeClosureFn>);

impl std::fmt::Debug for NativeClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("NativeClosure").finish()
    }
}

unsafe impl Trace for NativeClosure {
    fn needs_trace() -> bool {
        false
    }
}

impl NativeClosure {
    pub fn new<T>(func: T) -> Self
    where
        T: 'static + for<'a> Fn(&'a GcHeap, &mut Vm<'a>, StackKey) -> Result<usize, ErrorKind>,
    {
        Self(Box::new(func))
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue<'a> {
    Open(usize),
    Closed(Value<'a>),
}

impl<'a> From<Value<'a>> for Upvalue<'a> {
    fn from(x: Value<'a>) -> Self {
        Self::Closed(x)
    }
}

unsafe impl Trace for Upvalue<'_> {
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
