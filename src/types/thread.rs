use super::{Upvalue, Value};
use crate::{
    gc::{GarbageCollect, GcCell, GcContext, Tracer},
    runtime::Frame,
};
use std::{collections::BTreeMap, ops::Range};

#[derive(Clone)]
pub struct StackWindow(pub(crate) Range<usize>);

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

    pub fn stack(&self, window: StackWindow) -> &[Value<'gc>] {
        &self.stack[window.0]
    }

    pub fn stack_mut(&mut self, window: StackWindow) -> &mut [Value<'gc>] {
        &mut self.stack[window.0]
    }

    pub fn ensure_stack(&mut self, old_window: StackWindow, len: usize) -> StackWindow {
        if old_window.0.len() >= len {
            old_window
        } else {
            let new_end = old_window.0.start + len;
            self.stack.resize(new_end, Value::Nil);
            StackWindow(old_window.0.start..new_end)
        }
    }

    pub(crate) fn current_frame(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap()
    }

    pub(crate) fn close_upvalues(&mut self, gc: &'gc GcContext, boundary: usize) {
        for (_, upvalue) in self.open_upvalues.split_off(&boundary) {
            let mut upvalue = upvalue.borrow_mut(gc);
            if let Upvalue::Open(i) = *upvalue {
                *upvalue = Upvalue::Closed(self.stack[i]);
            }
        }
    }
}
