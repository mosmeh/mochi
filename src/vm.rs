pub(crate) mod instruction;
pub(crate) mod tag_method;

mod error;
mod main_loop;
mod opcode;
mod ops;

pub use error::{ErrorKind, Operation, RuntimeError, TracebackFrame};
pub use instruction::Instruction;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, Tracer},
    types::{LuaClosureProto, LuaString, LuaThread, StackWindow, Table, Upvalue, Value},
    Error, LuaClosure,
};
use std::{num::NonZeroUsize, ops::Range};
use tag_method::TagMethod;

#[derive(Default)]
pub struct Runtime {
    heap: GcHeap,
}

impl Runtime {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn heap(&mut self) -> &mut GcHeap {
        &mut self.heap
    }

    pub fn into_heap(self) -> GcHeap {
        self.heap
    }

    pub fn execute<F>(&mut self, f: F) -> Result<(), Error>
    where
        F: for<'gc> FnOnce(
            &'gc GcContext,
            GcCell<'gc, Vm<'gc>>,
        ) -> Result<LuaClosureProto<'gc>, Error>,
    {
        self.heap.with(|gc, vm| -> Result<(), Error> {
            let proto = f(gc, vm)?;
            let closure = gc.allocate(proto).into();
            vm.borrow().execute_main(gc, closure)?;
            Ok(())
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Frame {
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

struct ExecutionState<'gc, 'stack> {
    base: usize,
    pc: usize,
    stack: &'stack mut [Value<'gc>],
    lower_stack: &'stack mut [Value<'gc>],
}

impl<'gc, 'stack> ExecutionState<'gc, 'stack> {
    fn resolve_upvalue(&self, upvalue: &Upvalue<'gc>) -> Value<'gc> {
        match upvalue {
            Upvalue::Open(i) => {
                if *i < self.base {
                    self.lower_stack[*i]
                } else {
                    self.stack[*i - self.base]
                }
            }
            Upvalue::Closed(x) => *x,
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

pub struct Vm<'gc> {
    main_thread: GcCell<'gc, LuaThread<'gc>>,
    global_table: GcCell<'gc, Table<'gc>>,
    tag_method_names: [LuaString<'gc>; TagMethod::COUNT],
}

unsafe impl GarbageCollect for Vm<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.main_thread.trace(tracer);
        self.global_table.trace(tracer);
        self.tag_method_names.as_ref().trace(tracer);
    }
}

impl<'gc> Vm<'gc> {
    pub(crate) fn new(gc: &'gc GcContext) -> Self {
        Self {
            main_thread: gc.allocate_cell(LuaThread::new()),
            global_table: gc.allocate_cell(Table::new()),
            tag_method_names: TagMethod::allocate_names(gc),
        }
    }

    pub fn global_table(&self) -> GcCell<'gc, Table<'gc>> {
        self.global_table
    }

    pub fn load_stdlib(&self, gc: &'gc GcContext) {
        crate::stdlib::load(gc, self.global_table);
    }

    fn execute_main(
        &self,
        gc: &'gc GcContext,
        mut closure: LuaClosure<'gc>,
    ) -> Result<(), RuntimeError> {
        assert!(closure.upvalues.is_empty());
        closure
            .upvalues
            .push(gc.allocate_cell(Value::Table(self.global_table).into()));

        {
            let thread = self.main_thread.borrow();
            assert!(thread.stack.is_empty());
            assert!(thread.frames.is_empty());
            assert!(thread.open_upvalues.is_empty());
        }

        let result =
            unsafe { self.execute_value(gc, self.main_thread, gc.allocate(closure).into(), &[]) };
        match result {
            Ok(_) => Ok(()),
            Err(kind) => {
                let mut thread = self.main_thread.borrow_mut(gc);
                let frames = std::mem::take(&mut thread.frames);
                let traceback = frames
                    .into_iter()
                    .rev()
                    .map(|frame| {
                        let value = thread.stack[frame.bottom];
                        let proto = value.as_lua_closure().unwrap().proto;
                        TracebackFrame {
                            source: String::from_utf8_lossy(&proto.source).to_string(),
                            lines_defined: proto.lines_defined.clone(),
                        }
                    })
                    .collect();
                thread.stack.clear();
                thread.open_upvalues.clear();
                Err(RuntimeError { kind, traceback })
            }
        }
    }

    pub(crate) unsafe fn execute_value(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
        callee: Value<'gc>,
        args: &[Value<'gc>],
    ) -> Result<Value<'gc>, ErrorKind> {
        let mut thread_ref = thread.borrow_mut(gc);
        let bottom = thread_ref.stack.len();
        thread_ref.stack.reserve(args.len() + 1);
        thread_ref.stack.push(callee);
        thread_ref.stack.extend_from_slice(args);

        let frame_level = thread_ref.frames.len();
        thread_ref.frames.push(Frame::new(bottom));
        drop(thread_ref);

        loop {
            self.execute_frame(gc, thread)?;
            if thread.borrow().frames.len() <= frame_level {
                break;
            }
            if gc.debt() > 0 {
                gc.step_unguarded();
            }
        }

        thread.borrow_mut(gc).stack.truncate(bottom + 1);
        if gc.debt() > 0 {
            gc.step_unguarded();
        }

        let result = thread
            .borrow_mut(gc)
            .stack
            .drain(bottom..)
            .next()
            .unwrap_or_default();
        Ok(result)
    }

    fn call_value(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
        callee: Value,
        stack_range: Range<usize>,
        window_len: Option<NonZeroUsize>,
    ) -> Result<(), ErrorKind> {
        match callee {
            Value::NativeFunction(func) => {
                self.call_native(gc, thread, func.0, stack_range, window_len)
            }
            Value::LuaClosure(_) => {
                thread
                    .borrow_mut(gc)
                    .frames
                    .push(Frame::new(stack_range.start));
                Ok(())
            }
            Value::NativeClosure(closure) => {
                self.call_native(gc, thread, &closure.0, stack_range, window_len)
            }
            value => Err(ErrorKind::TypeError {
                operation: Operation::Call,
                ty: value.ty(),
            }),
        }
    }

    fn call_native<F>(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
        callee: F,
        stack_range: Range<usize>,
        window_len: Option<NonZeroUsize>,
    ) -> Result<(), ErrorKind>
    where
        F: for<'a> Fn(
            &'a GcContext,
            &Vm<'a>,
            GcCell<'a, LuaThread<'a>>,
            StackWindow,
        ) -> Result<usize, ErrorKind>,
    {
        let range = if let Some(window_len) = window_len {
            stack_range.start..stack_range.start + window_len.get() // fixed number of args
        } else {
            stack_range.clone() // variable number of args
        };
        let num_results = callee(gc, self, thread, StackWindow(range))?;
        thread
            .borrow_mut(gc)
            .stack
            .truncate(stack_range.start + num_results);
        Ok(())
    }

    unsafe fn call_index_metamethod<K>(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
        mut table: GcCell<'gc, Table<'gc>>,
        key: K,
    ) -> Result<Value<'gc>, ErrorKind>
    where
        K: Into<Value<'gc>>,
    {
        let key = key.into();
        let index_key = self.tag_method_names[TagMethod::Index as usize];
        loop {
            let metamethod_value = table
                .borrow()
                .metatable()
                .map(|metatable| metatable.borrow().get_field(index_key))
                .unwrap_or_default();
            match metamethod_value {
                Value::Nil => return Ok(Value::Nil),
                Value::NativeFunction(_) | Value::LuaClosure(_) | Value::NativeClosure(_) => {
                    return self.execute_value(gc, thread, metamethod_value, &[table.into(), key])
                }
                Value::Table(next_table) => {
                    let value = next_table.borrow().get(key);
                    if !value.is_nil() {
                        return Ok(value);
                    }
                    table = next_table;
                }
                _ => {
                    return Err(ErrorKind::TypeError {
                        operation: Operation::Index,
                        ty: metamethod_value.ty(),
                    })
                }
            }
        }
    }
}
