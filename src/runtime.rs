pub(crate) mod instruction;

mod error;
mod main_loop;
mod metamethod;
mod opcode;
mod ops;

pub use error::{ErrorKind, Operation, RuntimeError};
pub use instruction::Instruction;
pub(crate) use metamethod::Metamethod;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, Tracer},
    types::{
        Action, LuaString, LuaThread, NativeClosureFn, StackWindow, Table, ThreadStatus, Type,
        Upvalue, UserData, Value,
    },
    Error, LuaClosure,
};
use std::{ops::ControlFlow, path::Path};

pub(crate) type CoroutineResult = Result<(), ErrorKind>;

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

                let mut vm = vm.borrow_mut(gc);
                let main_thread = vm.main_thread;
                main_thread.borrow_mut(gc).status = ThreadStatus::Unresumable;
                assert!(vm.thread_stack.is_empty());
                vm.thread_stack.push(main_thread);

                let mut thread_ref = main_thread.borrow_mut(gc);
                assert!(thread_ref.stack.is_empty());
                assert!(thread_ref.frames.is_empty());
                assert!(thread_ref.open_upvalues.is_empty());
                thread_ref.stack.push(value);
                thread_ref.deferred_call(0)?;

                Ok(())
            },
        )?;

        loop {
            match self.heap.with(execute_until_gc) {
                Ok(ControlFlow::Continue(())) => self.heap.step(),
                Ok(ControlFlow::Break(())) => return Ok(()),
                Err(err) => return Err(Box::new(err)),
            }
        }
    }
}

fn execute_until_gc<'gc>(
    gc: &'gc GcContext,
    vm: GcCell<'gc, Vm<'gc>>,
) -> Result<ControlFlow<(), ()>, RuntimeError> {
    let mut vm = vm.borrow_mut(gc);
    while let Some(thread) = vm.thread_stack.last().copied() {
        vm.execute_single_step(gc, thread)?;
        if gc.debt() > 0 {
            return Ok(ControlFlow::Continue(()));
        }
    }
    Ok(ControlFlow::Break(()))
}

#[derive(Debug)]
pub(crate) enum Frame {
    Lua(LuaFrame),
    Native(NativeFrame),
    Continuation(ContinuationFrame),
}

#[derive(Debug, Clone)]
pub(crate) struct LuaFrame {
    pub(crate) bottom: usize,
    base: usize,
    pc: usize,
    num_extra_args: usize,
}

impl LuaFrame {
    pub fn new(bottom: usize) -> Self {
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
    pub fn new(bottom: usize) -> Self {
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

pub struct Vm<'gc> {
    registry: GcCell<'gc, Table<'gc>>,
    main_thread: GcCell<'gc, LuaThread<'gc>>,
    globals: GcCell<'gc, Table<'gc>>,
    thread_stack: Vec<GcCell<'gc, LuaThread<'gc>>>,
    metamethod_names: [LuaString<'gc>; Metamethod::COUNT],
    metatables: [Option<GcCell<'gc, Table<'gc>>>; Type::COUNT],
}

unsafe impl GarbageCollect for Vm<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.registry.trace(tracer);
        self.main_thread.trace(tracer);
        self.globals.trace(tracer);
        self.thread_stack.trace(tracer);
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
            thread_stack: Default::default(),
            metamethod_names: Metamethod::allocate_names(gc),
            metatables: Default::default(),
        }
    }

    pub fn registry(&self) -> GcCell<'gc, Table<'gc>> {
        self.registry
    }

    pub fn main_thread(&self) -> GcCell<'gc, LuaThread<'gc>> {
        self.main_thread
    }

    pub fn globals(&self) -> GcCell<'gc, Table<'gc>> {
        self.globals
    }

    pub fn load_stdlib(&mut self, gc: &'gc GcContext) {
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
            .or_else(|| self.metatables[object.ty() as usize])
    }

    pub fn set_metatable_of_type<T>(&mut self, ty: Type, metatable: T)
    where
        T: Into<Option<GcCell<'gc, Table<'gc>>>>,
    {
        self.metatables[ty as usize] = metatable.into();
    }

    fn execute_single_step(
        &mut self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
    ) -> Result<(), RuntimeError> {
        match self.execute_next_frame(gc, thread) {
            Ok(()) => (),
            Err(kind) => {
                self.thread_stack.pop().unwrap();
                {
                    let mut thread_ref = thread.borrow_mut(gc);
                    thread_ref.status = ThreadStatus::Error(kind.clone());

                    if self.thread_stack.is_empty() {
                        let traceback = thread_ref.traceback();
                        *thread_ref = LuaThread::new();
                        return Err(RuntimeError { kind, traceback });
                    }
                }

                let result = UserData::new(CoroutineResult::Err(kind));
                self.thread_stack
                    .last()
                    .unwrap()
                    .borrow_mut(gc)
                    .stack
                    .push(gc.allocate_cell(result).into());
            }
        };
        Ok(())
    }

    fn execute_next_frame(
        &mut self,
        gc: &'gc GcContext,
        thread: GcCell<'gc, LuaThread<'gc>>,
    ) -> Result<(), ErrorKind> {
        let mut thread_ref = thread.borrow_mut(gc);
        if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
            drop(thread_ref);
            return self.execute_lua_frame(gc, thread);
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
            None => {
                let coroutine = self.thread_stack.pop().unwrap();
                debug_assert!(GcCell::ptr_eq(&coroutine, &thread));

                let mut values = std::mem::take(&mut thread_ref.stack);
                if let Some(coroutine) = self.thread_stack.last() {
                    let mut coroutine_ref = coroutine.borrow_mut(gc);
                    let result = UserData::new(CoroutineResult::Ok(()));
                    coroutine_ref.stack.push(gc.allocate_cell(result).into());
                    coroutine_ref.stack.append(&mut values);
                }
                return Ok(());
            }
        };

        let mut thread_ref = thread.borrow_mut(gc);
        thread_ref.frames.push(current_frame.unwrap());

        let action = match result {
            Ok(action) => action,
            Err(kind) => return Err(kind),
        };

        match action {
            Action::Return { num_results } => {
                thread_ref.frames.pop().unwrap();
                thread_ref.stack.truncate(bottom + num_results)
            }
            Action::Call {
                callee_bottom,
                continuation,
            } => {
                *thread_ref.frames.last_mut().unwrap() = Frame::Continuation(ContinuationFrame {
                    bottom,
                    continuation: Box::new(continuation),
                });
                thread_ref.deferred_call(bottom + callee_bottom)?;
            }
            Action::TailCall { num_args } => {
                thread_ref.frames.pop().unwrap();
                thread_ref.stack.truncate(bottom + num_args + 1);
                thread_ref.deferred_call(bottom)?;
            }
            Action::Resume {
                coroutine_bottom,
                num_values,
                continuation,
            } => {
                let mut args = thread_ref.stack.split_off(bottom + coroutine_bottom);
                let coroutine = args[0].as_thread().unwrap();

                *thread_ref.frames.last_mut().unwrap() = Frame::Continuation(ContinuationFrame {
                    bottom,
                    continuation: Box::new(continuation),
                });
                drop(thread_ref);

                let mut coroutine_ref = coroutine.borrow_mut(gc);
                let resume_result = match coroutine_ref.status {
                    ThreadStatus::Error(_) => Err(ErrorKind::ExplicitError(
                        "cannot resume dead coroutine".to_owned(),
                    )),
                    ThreadStatus::Unresumable if coroutine_ref.frames.is_empty() => Err(
                        ErrorKind::ExplicitError("cannot resume dead coroutine".to_owned()),
                    ),
                    ThreadStatus::Unresumable => Err(ErrorKind::ExplicitError(
                        "cannot resume non-suspended coroutine".to_owned(),
                    )),
                    ThreadStatus::Resumable => Ok(()),
                };
                match resume_result {
                    Ok(()) => (),
                    Err(err) => {
                        drop(coroutine_ref);
                        let result = UserData::new(CoroutineResult::Err(err));
                        thread
                            .borrow_mut(gc)
                            .stack
                            .push(gc.allocate_cell(result).into());
                        return Ok(());
                    }
                }

                self.thread_stack.push(coroutine);
                coroutine_ref.status = ThreadStatus::Unresumable;

                let mut values = args.split_off(1);
                values.truncate(num_values);
                coroutine_ref.stack.append(&mut values);
            }
            Action::Yield { num_values } => {
                match self.thread_stack.len() {
                    0 => unreachable!(),
                    1 => {
                        return Err(ErrorKind::ExplicitError(
                            "attempt to yield from outside a coroutine".to_owned(),
                        ))
                    }
                    _ => (),
                }

                let resumer = self.thread_stack.pop().unwrap();
                debug_assert!(GcCell::ptr_eq(&resumer, &thread));

                thread_ref.frames.pop().unwrap();
                thread_ref.status = ThreadStatus::Resumable;

                let mut resumer_ref = self.thread_stack.last().unwrap().borrow_mut(gc);
                let result = UserData::new(CoroutineResult::Ok(()));
                resumer_ref.stack.push(gc.allocate_cell(result).into());

                let mut values = thread_ref.stack.split_off(bottom);
                values.truncate(num_values);
                resumer_ref.stack.append(&mut values);
            }
        }

        Ok(())
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
