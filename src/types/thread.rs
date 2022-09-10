use super::{Upvalue, Value};
use crate::{
    gc::{GarbageCollect, GcCell, Tracer},
    runtime::Frame,
};
use std::collections::BTreeMap;

pub struct StackWindow {
    pub(crate) bottom: usize,
}

#[derive(Default, Debug)]
pub struct LuaThread<'gc> {
    pub(crate) stack: Vec<Value<'gc>>,
    pub(crate) frames: Vec<Frame>,
    pub(crate) open_upvalues: BTreeMap<usize, GcCell<'gc, Upvalue<'gc>>>,
}

unsafe impl GarbageCollect for LuaThread<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.open_upvalues.trace(tracer);
    }
}

impl<'gc> LuaThread<'gc> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn stack(&self, window: &StackWindow) -> &[Value<'gc>] {
        &self.stack[window.bottom..]
    }

    pub fn stack_mut(&mut self, window: &StackWindow) -> &mut [Value<'gc>] {
        &mut self.stack[window.bottom..]
    }

    pub fn ensure_stack(&mut self, window: &mut StackWindow, len: usize) {
        if self.stack.len() < window.bottom + len {
            self.stack.resize(window.bottom + len, Value::Nil);
        }
    }
}
