use super::{LineRange, Upvalue, Value};
use crate::{
    gc::{GarbageCollect, GcCell, Tracer},
    runtime::{ErrorKind, Frame, LuaFrame, NativeFrame, Operation},
};
use std::{borrow::Cow, collections::BTreeMap, fmt::Display};

#[derive(Default)]
pub struct LuaThread<'gc> {
    pub(crate) status: ThreadStatus,
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

    pub fn with_body(body: Value<'gc>) -> Self {
        let mut thread = Self {
            stack: vec![body],
            ..Default::default()
        };
        thread.deferred_call(0).unwrap();
        thread
    }

    pub fn close(&mut self) {
        *self = Self {
            status: ThreadStatus::Unresumable,
            ..Default::default()
        };
    }

    pub fn stack(&self, window: &StackWindow) -> &[Value<'gc>] {
        &self.stack[window.bottom..]
    }

    pub fn stack_mut(&mut self, window: &StackWindow) -> &mut [Value<'gc>] {
        &mut self.stack[window.bottom..]
    }

    pub fn resize_stack(&mut self, window: &mut StackWindow, len: usize) {
        self.stack.resize(window.bottom + len, Value::Nil);
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
                Frame::Native(_) | Frame::Continuation(_) => TracebackFrame::Native,
            })
            .collect()
    }

    pub(crate) fn deferred_call(&mut self, bottom: usize) -> Result<(), ErrorKind> {
        match self.stack[bottom] {
            Value::LuaClosure(_) => {
                self.frames.push(Frame::Lua(LuaFrame::new(bottom)));
                Ok(())
            }
            Value::NativeFunction(_) | Value::NativeClosure(_) => {
                self.frames.push(Frame::Native(NativeFrame::new(bottom)));
                Ok(())
            }
            value => Err(ErrorKind::TypeError {
                operation: Operation::Call,
                ty: value.ty(),
            }),
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

pub struct StackWindow {
    pub(crate) bottom: usize,
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
                let source = format_source(source);
                match &lines_defined {
                    LineRange::File => write!(f, "{}: in main chunk", source),
                    LineRange::Lines(range) => {
                        write!(f, "{}: in function <{}:{}>", source, source, range.start())
                    }
                }
            }
            Self::Native => f.write_str("[C]: in function"),
        }
    }
}

fn format_source(source: &str) -> Cow<str> {
    const LUA_IDSIZE: usize = 60;
    const RETS: &str = "...";
    const PRE: &str = "[string \"";
    const POS: &str = "\"]";

    match source.chars().next() {
        Some('=') => source.chars().take(LUA_IDSIZE).skip(1).collect(),
        Some('@') => {
            let filename_len = source.len() - 1;
            if filename_len < LUA_IDSIZE {
                source.strip_prefix('@').unwrap().into()
            } else {
                let reversed: String = source
                    .chars()
                    .rev()
                    .take(filename_len.min(LUA_IDSIZE - RETS.len() - 1))
                    .collect();
                let mut ellipsized = RETS.to_owned();
                ellipsized.extend(reversed.chars().rev());
                ellipsized.into()
            }
        }
        _ => {
            const MAX_STR_LEN: usize = LUA_IDSIZE - PRE.len() - RETS.len() - POS.len() - 1;
            let mut lines = source.lines();
            let first_line = lines.next().unwrap_or_default();
            let is_multiline = lines.next().is_some();
            if !is_multiline && first_line.len() < MAX_STR_LEN {
                format!("{PRE}{first_line}{POS}").into()
            } else {
                let truncated: String = first_line.chars().take(MAX_STR_LEN).collect();
                format!("{PRE}{truncated}{RETS}{POS}").into()
            }
        }
    }
}
