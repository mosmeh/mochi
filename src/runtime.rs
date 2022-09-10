pub(crate) mod instruction;

mod error;
mod main_loop;
mod metamethod;
mod opcode;
mod ops;

pub use error::{ErrorKind, Operation, RuntimeError, TracebackFrame};
pub use instruction::Instruction;
pub(crate) use metamethod::Metamethod;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, Tracer},
    types::{
        Action, LuaString, LuaThread, NativeClosureFn, StackWindow, Table, Type, Upvalue, Value,
    },
    Error, LuaClosure,
};
use std::{ops::ControlFlow, path::Path};

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

    pub fn execute<F>(
        &mut self,
        f: F,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>>
    where
        F: for<'gc> FnOnce(
            &'gc GcContext,
            GcCell<'gc, Vm<'gc>>,
        ) -> Result<
            Value<'gc>,
            Box<dyn std::error::Error + Send + Sync + 'static>,
        >,
    {
        self.heap.with(
            |gc, vm| -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
                let value = f(gc, vm)?;
                let vm = vm.borrow();
                let mut thread = vm.main_thread.borrow_mut(gc);
                assert!(thread.stack.is_empty());
                assert!(thread.frames.is_empty());
                assert!(thread.open_upvalues.is_empty());
                thread.stack.push(value);
                thread.deferred_call(0)?;
                Ok(())
            },
        )?;

        let result = loop {
            let result = self.heap.with(|gc, vm| {
                let vm = vm.borrow();
                loop {
                    if vm.execute_frame(gc, vm.main_thread)?.is_break() {
                        return Ok(ControlFlow::Break(()));
                    }
                    if gc.debt() > 0 {
                        return Ok(ControlFlow::Continue(()));
                    }
                }
            });
            match result {
                Ok(ControlFlow::Continue(())) => self.heap.step(),
                Ok(ControlFlow::Break(())) => break Ok(()),
                Err(kind) => break Err(kind),
            }
        };

        self.heap.with(|gc, vm| {
            let vm = vm.borrow();
            let mut thread = vm.main_thread.borrow_mut(gc);
            let result = match result {
                Ok(()) => Ok(()),
                Err(kind) => {
                    let frames = std::mem::take(&mut thread.frames);
                    let traceback = frames
                        .into_iter()
                        .rev()
                        .map(|frame| match frame {
                            Frame::Lua(LuaFrame { bottom, .. }) => {
                                let value = thread.stack[bottom];
                                let proto = value.as_lua_closure().unwrap().proto;
                                TracebackFrame::Lua {
                                    source: String::from_utf8_lossy(&proto.source).to_string(),
                                    lines_defined: proto.lines_defined.clone(),
                                }
                            }
                            Frame::Native(_) | Frame::Continuation(_) => TracebackFrame::Native,
                        })
                        .collect();
                    Err(RuntimeError { kind, traceback }.into())
                }
            };
            thread.stack.clear();
            thread.frames.clear();
            thread.open_upvalues.clear();
            result
        })
    }
}

#[derive(Debug)]
pub(crate) enum Frame {
    Lua(LuaFrame),
    Native(NativeFrame),
    Continuation(ContinuationFrame),
}

#[derive(Debug, Clone)]
pub(crate) struct LuaFrame {
    bottom: usize,
    base: usize,
    pc: usize,
    num_extra_args: usize,
}

impl LuaFrame {
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
pub(crate) struct NativeFrame {
    bottom: usize,
}

impl NativeFrame {
    fn new(bottom: usize) -> Self {
        Self { bottom }
    }
}

pub(crate) struct ContinuationFrame {
    bottom: usize,
    continuation: Box<NativeClosureFn>,
}

impl std::fmt::Debug for ContinuationFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContinuationFrame")
            .field("bottom", &self.bottom)
            .finish()
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
    registry: GcCell<'gc, Table<'gc>>,
    main_thread: GcCell<'gc, LuaThread<'gc>>,
    globals: GcCell<'gc, Table<'gc>>,
    metamethod_names: [LuaString<'gc>; Metamethod::COUNT],
    metatables: GcCell<'gc, [Option<GcCell<'gc, Table<'gc>>>; Type::COUNT]>,
}

unsafe impl GarbageCollect for Vm<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.registry.trace(tracer);
        self.main_thread.trace(tracer);
        self.globals.trace(tracer);
        self.metamethod_names.trace(tracer);
        self.metatables.trace(tracer);
    }
}

impl<'gc> Vm<'gc> {
    pub(crate) fn new(gc: &'gc GcContext) -> Self {
        let main_thread = gc.allocate_cell(LuaThread::new());
        let globals = gc.allocate_cell(Table::new());
        let registry = Table::from(vec![main_thread.into(), globals.into()]);
        Self {
            registry: gc.allocate_cell(registry),
            main_thread,
            globals,
            metamethod_names: Metamethod::allocate_names(gc),
            metatables: gc.allocate_cell(Default::default()),
        }
    }

    pub fn registry(&self) -> GcCell<'gc, Table<'gc>> {
        self.registry
    }

    pub fn globals(&self) -> GcCell<'gc, Table<'gc>> {
        self.globals
    }

    pub fn load_stdlib(&self, gc: &'gc GcContext) {
        crate::stdlib::load(gc, self);
    }

    pub fn load<B, S>(
        &self,
        gc: &'gc GcContext,
        bytes: B,
        source: S,
    ) -> Result<LuaClosure<'gc>, Error>
    where
        B: AsRef<[u8]>,
        S: AsRef<[u8]>,
    {
        let proto = crate::load(gc, bytes, source)?;
        let mut closure = LuaClosure::from(gc.allocate(proto));
        closure
            .upvalues
            .push(gc.allocate_cell(Value::Table(self.globals).into()));
        Ok(closure)
    }

    pub fn load_file<P: AsRef<Path>>(
        &self,
        gc: &'gc GcContext,
        path: P,
    ) -> Result<LuaClosure<'gc>, Error> {
        let proto = crate::load_file(gc, path)?;
        let mut closure = LuaClosure::from(gc.allocate(proto));
        closure
            .upvalues
            .push(gc.allocate_cell(Value::Table(self.globals).into()));
        Ok(closure)
    }

    pub fn metamethod_name(&self, metamethod: Metamethod) -> LuaString<'gc> {
        self.metamethod_names[metamethod as usize]
    }

    pub fn metatable_of_object(&self, object: Value<'gc>) -> Option<GcCell<'gc, Table<'gc>>> {
        object
            .metatable()
            .or_else(|| self.metatables.borrow()[object.ty() as usize])
    }

    pub fn set_metatable_of_type<T>(&self, gc: &'gc GcContext, ty: Type, metatable: T)
    where
        T: Into<Option<GcCell<'gc, Table<'gc>>>>,
    {
        self.metatables.borrow_mut(gc)[ty as usize] = metatable.into();
    }

    fn execute_frame(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
    ) -> Result<ControlFlow<(), ()>, ErrorKind> {
        let mut thread_ref = thread.borrow_mut(gc);
        if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
            drop(thread_ref);
            self.execute_lua_frame(gc, thread)?;
            return Ok(ControlFlow::Continue(()));
        }

        let current_frame = thread_ref.frames.pop();
        let (bottom, result) = match &current_frame {
            Some(Frame::Lua(_)) => unreachable!(),
            Some(Frame::Native(NativeFrame { bottom })) => {
                let bottom = *bottom;
                let callee = thread_ref.stack[bottom];
                let func = match &callee {
                    Value::NativeFunction(func) => &func.0,
                    Value::NativeClosure(closure) => closure.function(),
                    Value::LuaClosure(_) => unreachable!(),
                    value => {
                        return Err(ErrorKind::TypeError {
                            operation: Operation::Call,
                            ty: value.ty(),
                        })
                    }
                };
                drop(thread_ref);
                (bottom, func(gc, self, thread, StackWindow { bottom }))
            }
            Some(Frame::Continuation(ContinuationFrame {
                bottom,
                continuation,
            })) => {
                drop(thread_ref);
                let bottom = *bottom;
                (
                    bottom,
                    continuation(gc, self, thread, StackWindow { bottom }),
                )
            }
            None => return Ok(ControlFlow::Break(())),
        };

        let mut thread_ref = thread.borrow_mut(gc);
        let action = match result {
            Ok(action) => action,
            Err(err) => {
                thread_ref.frames.push(current_frame.unwrap());
                return Err(err);
            }
        };

        match action {
            Action::Return { num_results } => thread_ref.stack.truncate(bottom + num_results),
            Action::Call {
                callee_bottom,
                continuation,
            } => {
                thread_ref
                    .frames
                    .push(Frame::Continuation(ContinuationFrame {
                        bottom,
                        continuation: Box::new(continuation),
                    }));
                thread_ref.deferred_call(bottom + callee_bottom)?;
            }
            Action::TailCall { num_args } => {
                thread_ref.stack.truncate(bottom + num_args + 1);
                thread_ref.deferred_call(bottom)?;
            }
        }

        Ok(ControlFlow::Continue(()))
    }

    fn deferred_call_index_metamethod<K>(
        &self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
        mut table_like: Value<'gc>,
        key: K,
        dest: usize,
    ) -> Result<(), ErrorKind>
    where
        K: Into<Value<'gc>>,
    {
        let key = key.into();
        let index_key = self.metamethod_name(Metamethod::Index);
        loop {
            let metamethod = if let Value::Table(table) = table_like {
                let metamethod = table
                    .borrow()
                    .metatable()
                    .map(|metatable| metatable.borrow().get_field(index_key))
                    .unwrap_or_default();
                if metamethod.is_nil() {
                    thread.borrow_mut(gc).stack[dest] = Value::Nil;
                    return Ok(());
                }
                metamethod
            } else {
                let metamethod = self
                    .metatable_of_object(table_like)
                    .map(|metatable| metatable.borrow().get_field(index_key))
                    .unwrap_or_default();
                if metamethod.is_nil() {
                    return Err(ErrorKind::TypeError {
                        operation: Operation::Index,
                        ty: table_like.ty(),
                    });
                }
                metamethod
            };
            match metamethod {
                Value::NativeFunction(_) | Value::LuaClosure(_) | Value::NativeClosure(_) => {
                    thread.borrow_mut(gc).deferred_call_metamethod(
                        metamethod,
                        &[table_like, key],
                        dest,
                    );
                    return Ok(());
                }
                Value::Table(table) => {
                    let value = table.borrow().get(key);
                    if !value.is_nil() {
                        thread.borrow_mut(gc).stack[dest] = value;
                        return Ok(());
                    }
                }
                Value::Nil => unreachable!(),
                _ => (),
            }
            table_like = metamethod;
        }
    }
}

impl<'gc> LuaThread<'gc> {
    fn current_lua_frame(&mut self) -> &mut LuaFrame {
        if let Some(Frame::Lua(frame)) = self.frames.last_mut() {
            frame
        } else {
            unreachable!()
        }
    }

    fn deferred_call(&mut self, bottom: usize) -> Result<(), ErrorKind> {
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

    fn deferred_call_metamethod(
        &mut self,
        metamethod: Value<'gc>,
        args: &[Value<'gc>],
        dest: usize,
    ) {
        let current_bottom = self.current_lua_frame().bottom;
        let metamethod_bottom = self.stack.len();
        self.stack.push(metamethod);
        self.stack.extend_from_slice(args);
        self.frames.push(Frame::Continuation(ContinuationFrame {
            bottom: current_bottom,
            continuation: Box::new(move |gc, _, thread, _| {
                let stack = &mut thread.borrow_mut(gc).stack;
                stack[dest] = stack[metamethod_bottom];
                Ok(Action::Return {
                    num_results: metamethod_bottom - current_bottom,
                })
            }),
        }));
        self.frames
            .push(Frame::Lua(LuaFrame::new(metamethod_bottom)));
    }

    fn close_upvalues(&mut self, gc: &'gc GcContext, boundary: usize) {
        for (_, upvalue) in self.open_upvalues.split_off(&boundary) {
            let mut upvalue = upvalue.borrow_mut(gc);
            if let Upvalue::Open(i) = *upvalue {
                *upvalue = Upvalue::Closed(self.stack[i]);
            }
        }
    }
}
