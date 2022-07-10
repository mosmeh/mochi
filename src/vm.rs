mod error;
mod instruction;
mod main_loop;
mod opcode;
mod ops;

pub use error::{ErrorKind, Operation, RuntimeError, TracebackFrame};
pub use instruction::Instruction;
pub use opcode::OpCode;

use crate::{
    gc::{GcCell, GcHeap, Trace, Tracer},
    types::{StackKey, Table, Upvalue, Value},
    LuaClosure,
};
use std::{collections::BTreeMap, ops::Range};

#[derive(Debug, Clone)]
struct Frame {
    bottom: usize,
    base: usize,
    pc: usize,
    num_extra_args: usize,
}

impl Frame {
    fn new(bottom: usize) -> Self {
        Self {
            bottom,
            base: bottom + 1,
            pc: 0,
            num_extra_args: 0,
        }
    }
}

#[derive(Debug)]
pub(crate) struct State<'a, 'b> {
    base: usize,
    pc: usize,
    stack: &'b mut [Value<'a>],
    lower_stack: &'b [Value<'a>],
}

unsafe impl Trace for State<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.lower_stack.trace(tracer);
    }
}

impl<'a, 'b> State<'a, 'b> {
    fn resolve_upvalue(&'b self, upvalue: &'b Upvalue<'a>) -> &'b Value<'a> {
        match upvalue {
            Upvalue::Open(i) => {
                if *i < self.base {
                    &self.lower_stack[*i]
                } else {
                    &self.stack[*i - self.base]
                }
            }
            Upvalue::Closed(x) => x,
        }
    }
}

#[derive(Debug)]
pub struct Vm<'a> {
    stack: Vec<Value<'a>>,
    frames: Vec<Frame>,
    global_table: GcCell<'a, Table<'a>>,
    open_upvalues: BTreeMap<usize, GcCell<'a, Upvalue<'a>>>,
}

unsafe impl Trace for Vm<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.global_table.trace(tracer);
        self.open_upvalues.trace(tracer);
    }
}

impl<'a> Vm<'a> {
    pub fn new(global_table: GcCell<'a, Table<'a>>) -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            global_table,
            open_upvalues: BTreeMap::new(),
        }
    }

    pub fn global_table(&self) -> GcCell<'a, Table<'a>> {
        self.global_table
    }

    pub fn local_stack(&self, key: StackKey) -> &[Value<'a>] {
        &self.stack[key.0]
    }

    pub fn local_stack_mut(&mut self, key: StackKey) -> &mut [Value<'a>] {
        &mut self.stack[key.0]
    }

    pub fn execute(
        &mut self,
        heap: &'a GcHeap,
        mut closure: LuaClosure<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        assert!(closure.upvalues.is_empty());
        closure
            .upvalues
            .push(heap.allocate_cell(Value::from(self.global_table).into()));

        let bottom = self.stack.len();
        self.stack.push(heap.allocate(closure).into());

        let frame_level = self.frames.len();
        self.frames.push(Frame::new(bottom));

        while self.frames.len() > frame_level {
            if let Err(source) = self.execute_frame(heap) {
                let traceback = self
                    .frames
                    .iter()
                    .rev()
                    .map(|frame| {
                        let value = self.stack[frame.bottom];
                        let proto = value.as_lua_closure().unwrap().proto;
                        TracebackFrame {
                            source: String::from_utf8_lossy(&proto.source).to_string(),
                            lines_defined: proto.lines_defined.clone(),
                        }
                    })
                    .collect();
                self.stack.truncate(bottom);
                self.frames.truncate(frame_level);
                return Err(RuntimeError { source, traceback });
            }
        }

        let result = self.stack.drain(bottom..).next().unwrap_or_default();
        unsafe { heap.step(self) };
        Ok(result)
    }

    fn call_closure(
        &mut self,
        heap: &'a GcHeap,
        callee: Value<'a>,
        stack_range: Range<usize>,
        insn: Instruction,
    ) -> Result<(), ErrorKind> {
        match callee {
            Value::LuaClosure(_) => {
                self.frames.push(Frame::new(stack_range.start));
                Ok(())
            }
            Value::NativeClosure(closure) => {
                let b = insn.b();
                let range = if b > 0 {
                    stack_range.start..stack_range.start + b // fixed number of args
                } else {
                    stack_range.clone() // variable number of args
                };
                let num_results = (closure.0)(heap, self, StackKey(range))?;
                self.stack.truncate(stack_range.start + num_results);
                Ok(())
            }
            value => Err(ErrorKind::TypeError {
                operation: Operation::Call,
                ty: value.ty(),
            }),
        }
    }

    fn close_upvalues(&mut self, heap: &GcHeap, boundary: usize) {
        for (_, upvalue) in self.open_upvalues.split_off(&boundary) {
            let mut upvalue = upvalue.borrow_mut(heap);
            if let Upvalue::Open(i) = *upvalue {
                *upvalue = Upvalue::Closed(self.stack[i]);
            }
        }
    }
}
