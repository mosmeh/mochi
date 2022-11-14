use super::{LineRange, Upvalue, Value};
use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcLifetime, Tracer},
    runtime::{ErrorKind, Frame},
};
use std::{collections::BTreeMap, fmt::Display};

#[derive(Default)]
pub struct LuaThread<'gc, 'a> {
    pub(crate) status: ThreadStatus,
    pub(crate) stack: Vec<Value<'gc, 'a>>,
    pub(crate) frames: Vec<Frame>,
    pub(crate) open_upvalues: BTreeMap<usize, GcCell<'gc, 'a, Upvalue<'gc, 'a>>>,
}

unsafe impl GarbageCollect for LuaThread<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.open_upvalues.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for LuaThread<'gc, '_> {
    type Aged = LuaThread<'gc, 'a>;
}

impl std::fmt::Debug for LuaThread<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LuaThread")
            .field("status", &self.status)
            .field("frames", &self.frames)
            .finish()
    }
}

impl<'gc, 'a> LuaThread<'gc, 'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn close(&mut self, gc: &'a GcContext<'gc>) {
        self.close_upvalues(gc, 0);
        *self = Self {
            status: ThreadStatus::Unresumable,
            ..Default::default()
        };
    }

    pub fn traceback(&self) -> Vec<TracebackFrame> {
        self.frames
            .iter()
            .rev()
            .map(|frame| match frame {
                Frame::Lua(frame) => {
                    let value = self.stack[frame.bottom];
                    let proto = value.as_lua_closure().unwrap().proto;
                    TracebackFrame::Lua {
                        source: String::from_utf8_lossy(&proto.source).to_string(),
                        lines_defined: proto.lines_defined.clone(),
                    }
                }
                _ => TracebackFrame::Native,
            })
            .collect()
    }

    pub(crate) fn close_upvalues(&mut self, gc: &'a GcContext<'gc>, boundary: usize) {
        for (_, upvalue) in self.open_upvalues.split_off(&boundary) {
            let mut upvalue = upvalue.borrow_mut(gc);
            if let Upvalue::Open { index, .. } = *upvalue {
                *upvalue = Upvalue::Closed(self.stack[index]);
            }
        }
    }
}

#[derive(Debug)]
pub(crate) enum ThreadStatus {
    Resumable,
    Unresumable,
    Error(ErrorKind),
}

impl Default for ThreadStatus {
    fn default() -> Self {
        Self::Resumable
    }
}

#[derive(Debug)]
pub enum TracebackFrame {
    Lua {
        source: String,
        lines_defined: LineRange,
    },
    Native,
}

impl Display for TracebackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua {
                source,
                lines_defined,
            } => {
                let source = crate::chunk_id_from_source(source);
                match &lines_defined {
                    LineRange::File => write!(f, "{source}: in main chunk"),
                    LineRange::Lines(range) => {
                        write!(f, "{source}: in function <{source}:{}>", range.start())
                    }
                }
            }
            Self::Native => f.write_str("[C]: in function"),
        }
    }
}
