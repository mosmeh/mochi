mod error;
mod instruction;
mod main_loop;
mod opcode;
mod ops;
mod tag_method;

pub use error::{ErrorKind, Operation, RuntimeError, TracebackFrame};
pub use instruction::Instruction;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcHeap, Tracer},
    types::{LuaString, StackKey, Table, Upvalue, Value},
    LuaClosure,
};
use std::{collections::BTreeMap, num::NonZeroUsize, ops::Range};

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
struct State<'gc, 'stack> {
    base: usize,
    pc: usize,
    stack: &'stack mut [Value<'gc>],
    lower_stack: &'stack mut [Value<'gc>],
}

unsafe impl GarbageCollect for State<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.lower_stack.trace(tracer);
    }
}

impl<'gc, 'stack> State<'gc, 'stack> {
    fn resolve_upvalue<'a>(&'a self, upvalue: &'a Upvalue<'gc>) -> &'a Value<'gc> {
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

    fn resolve_upvalue_mut<'a>(&'a mut self, upvalue: &'a mut Upvalue<'gc>) -> &'a mut Value<'gc> {
        match upvalue {
            Upvalue::Open(i) => {
                if *i < self.base {
                    &mut self.lower_stack[*i]
                } else {
                    &mut self.stack[*i - self.base]
                }
            }
            Upvalue::Closed(x) => x,
        }
    }
}

struct Root<'gc, 'vm, 'stack> {
    state: &'stack State<'gc, 'stack>,
    global_table: GcCell<'gc, Table<'gc>>,
    open_upvalues: &'vm BTreeMap<usize, GcCell<'gc, Upvalue<'gc>>>,
    tag_method_names: &'vm [LuaString<'gc>; tag_method::COUNT],
}

unsafe impl GarbageCollect for Root<'_, '_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.state.trace(tracer);
        self.global_table.trace(tracer);
        self.open_upvalues.trace(tracer);
        self.tag_method_names.as_ref().trace(tracer);
    }
}

pub struct Vm<'gc> {
    heap: &'gc GcHeap,
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
    global_table: GcCell<'gc, Table<'gc>>,
    open_upvalues: BTreeMap<usize, GcCell<'gc, Upvalue<'gc>>>,
    tag_method_names: [LuaString<'gc>; tag_method::COUNT],
}

unsafe impl GarbageCollect for Vm<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.stack.trace(tracer);
        self.global_table.trace(tracer);
        self.open_upvalues.trace(tracer);
    }
}

impl<'gc> Vm<'gc> {
    pub fn new(heap: &'gc GcHeap, global_table: GcCell<'gc, Table<'gc>>) -> Self {
        Self {
            heap,
            stack: Vec::new(),
            frames: Vec::new(),
            global_table,
            open_upvalues: BTreeMap::new(),
            tag_method_names: tag_method::allocate_tag_method_names(heap),
        }
    }

    pub fn heap(&self) -> &'gc GcHeap {
        self.heap
    }

    pub fn global_table(&self) -> GcCell<'gc, Table<'gc>> {
        self.global_table
    }

    pub fn local_stack(&self, key: StackKey) -> &[Value<'gc>] {
        &self.stack[key.0]
    }

    pub fn local_stack_mut(&mut self, key: StackKey) -> &mut [Value<'gc>] {
        &mut self.stack[key.0]
    }

    pub fn execute(&mut self, mut closure: LuaClosure<'gc>) -> Result<Value<'gc>, RuntimeError> {
        assert!(self.stack.is_empty());
        assert!(self.frames.is_empty());
        assert!(self.open_upvalues.is_empty());

        if closure.upvalues.is_empty() {
            closure.upvalues.push(
                self.heap
                    .allocate_cell(Value::Table(self.global_table).into()),
            );
        }

        let callee = self.heap.allocate(closure).into();
        match self.execute_inner(callee, &[]) {
            Ok(result) => Ok(result),
            Err(source) => {
                let traceback = self
                    .frames
                    .drain(..)
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
                self.stack.clear();
                self.open_upvalues.clear();
                Err(RuntimeError { source, traceback })
            }
        }
    }

    pub(crate) fn execute_inner(
        &mut self,
        callee: Value<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, ErrorKind> {
        let bottom = self.stack.len();
        self.stack.reserve(args.len() + 1);
        self.stack.push(callee);
        self.stack.extend_from_slice(args);

        let frame_level = self.frames.len();
        self.frames.push(Frame::new(bottom));

        while self.frames.len() > frame_level {
            self.execute_frame()?;
        }

        self.stack.truncate(bottom + 1);
        unsafe { self.heap.step(self) };

        let result = self.stack.drain(bottom..).next().unwrap_or_default();
        Ok(result)
    }

    fn call_closure(
        &mut self,
        callee: Value<'gc>,
        stack_range: Range<usize>,
        num_args: Option<NonZeroUsize>,
    ) -> Result<(), ErrorKind> {
        match callee {
            Value::LuaClosure(_) => {
                self.frames.push(Frame::new(stack_range.start));
                Ok(())
            }
            Value::NativeClosure(closure) => {
                let range = if let Some(num_args) = num_args {
                    stack_range.start..stack_range.start + num_args.get() // fixed number of args
                } else {
                    stack_range.clone() // variable number of args
                };
                let num_results = (closure.0)(self, StackKey(range))?;
                self.stack.truncate(stack_range.start + num_results);
                Ok(())
            }
            value => Err(ErrorKind::TypeError {
                operation: Operation::Call,
                ty: value.ty(),
            }),
        }
    }

    fn close_upvalues(&mut self, boundary: usize) {
        for (_, upvalue) in self.open_upvalues.split_off(&boundary) {
            let mut upvalue = upvalue.borrow_mut(self.heap);
            if let Upvalue::Open(i) = *upvalue {
                *upvalue = Upvalue::Closed(self.stack[i]);
            }
        }
    }
}
