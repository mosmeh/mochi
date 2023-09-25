use super::{LineRange, Upvalue, Value};
use crate::{
    gc::{GarbageCollect, GcCell, GcContext, Tracer},
    runtime::{ErrorKind, Frame},
};
use std::{collections::BTreeMap, fmt::Display};

#[derive(Default)]
pub struct LuaThread<'gc> {
    pub(crate) status: ThreadStatus,
    pub(crate) stack: Vec<Value<'gc>>,
    pub(crate) frames: Vec<Frame<'gc>>,
    pub(crate) open_upvalues: BTreeMap<usize, GcCell<'gc, Upvalue<'gc>>>,
}

unsafe impl GarbageCollect for LuaThread<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.frames.trace(tracer);
        self.open_upvalues.trace(tracer);
    }
}

impl std::fmt::Debug for LuaThread<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LuaThread")
            .field("status", &self.status)
            .field("frames", &self.frames)
            .finish()
    }
}

impl<'gc> LuaThread<'gc> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn close(&mut self, gc: &'gc GcContext) {
        self.close_upvalues(gc, 0);
        *self = Self {
            status: ThreadStatus::Unresumable,
            ..Default::default()
        };
    }

    pub fn traceback(&self) -> Vec<TracebackFrame> {
        self.frames
            .iter()
            .enumerate()
            .rev()
            .map(|(i, frame)| match frame {
                Frame::Lua(frame) => {
                    let value = self.stack[frame.bottom];
                    let proto = value.as_lua_closure().unwrap().proto;
                    TracebackFrame::Lua {
                        source: String::from_utf8_lossy(&proto.source).to_string(),
                        line: proto.get_currentline(frame),
                        lines_defined: proto.lines_defined.clone(),
                    }
                }
                Frame::Native { .. } => {
                    let func = if let Some(frame) =
                        self.frames[..i].iter().rev().find_map(Frame::as_lua)
                    {
                        let value = self.stack[frame.bottom];
                        let proto = value.as_lua_closure().unwrap().proto;
                        proto
                            .funcname_from_code(frame.last_pc() as _)
                            .map(|x| x.name.to_string())
                    } else {
                        None
                    };
                    TracebackFrame::Native { func }
                }
                _ => TracebackFrame::Native { func: None },
            })
            .collect()
    }

    pub(crate) fn close_upvalues(&mut self, gc: &'gc GcContext, boundary: usize) {
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
        line: Option<u32>,
        lines_defined: LineRange,
    },
    Native {
        func: Option<String>,
    },
}

impl Display for TracebackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua {
                source,
                line,
                lines_defined,
            } => {
                let source = crate::chunk_id_from_source(source);
                let line = line.map(|n| n.to_string()).unwrap_or_else(|| "?".into());
                match &lines_defined {
                    LineRange::File => write!(f, "{source}:{line}: in main chunk"),
                    LineRange::Lines(range) => {
                        write!(
                            f,
                            "{source}:{line}: in function <{source}:{}>",
                            range.start()
                        )
                    }
                }
            }
            Self::Native { func: Some(fname) } => write!(f, "[C]: in function '{fname}'"),
            Self::Native { func: None } => f.write_str("[C]: in function"),
        }
    }
}
