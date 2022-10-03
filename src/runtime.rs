pub(crate) mod instruction;

mod action;
mod error;
mod frame;
mod lua_dispatch;
mod metamethod;
mod opcode;
mod ops;

pub use action::{Action, Continuation};
pub use error::{ErrorKind, Operation, RuntimeError};
pub(crate) use frame::{ContinuationFrame, Frame, LuaFrame};
pub use instruction::Instruction;
pub use metamethod::Metamethod;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, Tracer},
    types::{LuaString, LuaThread, Table, ThreadStatus, Type, Upvalue, Value},
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

    pub fn execute<F>(&mut self, f: F) -> Result<(), RuntimeError>
    where
        F: for<'gc> FnOnce(
            &'gc GcContext,
            GcCell<'gc, Vm<'gc>>,
        ) -> Result<
            Value<'gc>,
            Box<dyn std::error::Error + Send + Sync + 'static>,
        >,
    {
        let result = self.heap.with(|gc, vm| {
            let value = match f(gc, vm) {
                Ok(value) => value,
                Err(err) => return Err(ErrorKind::External(err.into())),
            };

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
            thread_ref.push_frame(0)?;

            Ok(())
        });
        match result {
            Ok(()) => (),
            Err(kind) => {
                return Err(RuntimeError {
                    kind,
                    traceback: Vec::new(),
                })
            }
        }

        loop {
            let action = self
                .heap
                .with(|gc, vm| vm.borrow_mut(gc).execute_single_step(gc))?;
            match action {
                RuntimeAction::StepGc => self.heap.step(),
                RuntimeAction::MutateGc(mutator) => mutator(&mut self.heap),
                RuntimeAction::Exit => return Ok(()),
            }
        }
    }
}

enum RuntimeAction {
    StepGc,
    MutateGc(Box<dyn Fn(&mut GcHeap)>),
    Exit,
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

    pub fn current_thread(&self) -> GcCell<'gc, LuaThread<'gc>> {
        self.thread_stack
            .last()
            .copied()
            .unwrap_or(self.main_thread)
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

    pub fn metamethod_of_object(
        &self,
        metamethod: Metamethod,
        object: Value<'gc>,
    ) -> Option<Value<'gc>> {
        self.metatable_of_object(object).and_then(|metatable| {
            let metamethod = metatable
                .borrow()
                .get_field(self.metamethod_name(metamethod));
            (!metamethod.is_nil()).then_some(metamethod)
        })
    }

    pub fn set_metatable_of_type<T>(&mut self, ty: Type, metatable: T)
    where
        T: Into<Option<GcCell<'gc, Table<'gc>>>>,
    {
        self.metatables[ty as usize] = metatable.into();
    }

    fn execute_single_step(&mut self, gc: &'gc GcContext) -> Result<RuntimeAction, RuntimeError> {
        while !self.thread_stack.is_empty() {
            match self.execute_next_frame(gc) {
                Ok(Some(action)) => return Ok(action),
                Ok(None) => (),
                Err(kind) => {
                    let thread = self.current_thread();
                    let mut thread_ref = thread.borrow_mut(gc);
                    if let Some((i, frame)) =
                        thread_ref
                            .frames
                            .iter_mut()
                            .enumerate()
                            .rfind(|(_, frame)| {
                                matches!(frame, Frame::ProtectedCallContinuation { .. })
                            })
                    {
                        if let Frame::ProtectedCallContinuation { inner, .. } = frame {
                            inner.continuation.as_mut().unwrap().set_args(Err(kind));
                        } else {
                            unreachable!()
                        }
                        thread_ref.frames.truncate(i + 1);
                    } else {
                        self.thread_stack.pop().unwrap();
                        thread_ref.status = ThreadStatus::Error(kind.clone());

                        if self.thread_stack.is_empty() {
                            let traceback = thread_ref.traceback();
                            *thread_ref = LuaThread::new();
                            return Err(RuntimeError { kind, traceback });
                        }
                        drop(thread_ref);

                        let mut resumer_ref = self.thread_stack.last().unwrap().borrow_mut(gc);
                        if let Frame::ResumeContinuation(frame) =
                            resumer_ref.frames.last_mut().unwrap()
                        {
                            frame.continuation.as_mut().unwrap().set_args(Err(kind));
                        } else {
                            unreachable!()
                        }
                    }
                }
            }
            if gc.should_perform_gc() {
                return Ok(RuntimeAction::StepGc);
            }
        }
        Ok(if gc.should_perform_gc() {
            RuntimeAction::StepGc
        } else {
            RuntimeAction::Exit
        })
    }

    fn index_slow_path<K>(
        &self,
        thread: &mut LuaThread<'gc>,
        mut table_like: Value<'gc>,
        key: K,
        dest: usize,
    ) -> Result<ControlFlow<()>, ErrorKind>
    where
        K: Into<Value<'gc>>,
    {
        let key = key.into();
        let index_key = self.metamethod_name(Metamethod::Index);
        for _ in 0..2000 {
            let metamethod = if let Value::Table(table) = table_like {
                let metamethod = table
                    .borrow()
                    .metatable()
                    .map(|metatable| metatable.borrow().get_field(index_key))
                    .unwrap_or_default();
                if metamethod.is_nil() {
                    thread.stack[dest] = Value::Nil;
                    return Ok(ControlFlow::Continue(()));
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
                    return Ok(thread.push_metamethod_frame(
                        metamethod,
                        &[table_like, key],
                        move |gc, vm, results| {
                            vm.current_thread().borrow_mut(gc).stack[dest] =
                                results.first().copied().unwrap_or_default();
                            Ok(Action::ReturnArguments)
                        },
                    ));
                }
                Value::Table(table) => {
                    let value = table.borrow().get(key);
                    if !value.is_nil() {
                        thread.stack[dest] = value;
                        return Ok(ControlFlow::Continue(()));
                    }
                }
                Value::Nil => unreachable!(),
                _ => (),
            }
            table_like = metamethod;
        }
        Err(ErrorKind::other("'__index' chain too long; possible loop"))
    }

    fn compare_slow_path(
        &self,
        thread: &mut LuaThread<'gc>,
        metamethod: Metamethod,
        a: Value<'gc>,
        b: Value<'gc>,
        pc: usize,
        code: &[Instruction],
    ) -> Result<ControlFlow<()>, ErrorKind> {
        let metamethod = self
            .metamethod_of_object(metamethod, a)
            .or_else(|| self.metamethod_of_object(metamethod, b))
            .ok_or_else(|| ErrorKind::TypeError {
                operation: Operation::Compare,
                ty: b.ty(),
            })?;

        let insn = code[pc - 1];
        let next_insn = code[pc];

        Ok(
            thread.push_metamethod_frame(metamethod, &[a, b], move |gc, vm, results| {
                let cond = results.first().map(Value::to_boolean).unwrap_or_default();
                let new_pc = if cond == insn.k() {
                    (pc as isize + next_insn.sj() as isize + 1) as usize
                } else {
                    pc + 1
                };
                vm.current_thread().borrow_mut(gc).current_lua_frame().pc = new_pc;
                Ok(Action::ReturnArguments)
            }),
        )
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

    pub(crate) fn push_frame(&mut self, bottom: usize) -> Result<ControlFlow<()>, ErrorKind> {
        match self.stack[bottom] {
            Value::LuaClosure(_) => {
                self.frames.push(Frame::Lua(LuaFrame::new(bottom)));
                Ok(ControlFlow::Continue(()))
            }
            Value::NativeFunction(_) | Value::NativeClosure(_) => {
                self.frames.push(Frame::Native { bottom });
                Ok(ControlFlow::Break(()))
            }
            value => Err(ErrorKind::TypeError {
                operation: Operation::Call,
                ty: value.ty(),
            }),
        }
    }

    #[must_use]
    fn push_metamethod_frame<F>(
        &mut self,
        metamethod: Value<'gc>,
        args: &[Value<'gc>],
        continuation: F,
    ) -> ControlFlow<()>
    where
        F: 'static
            + Fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
    {
        let current_bottom = self.current_lua_frame().bottom;
        let metamethod_bottom = self.stack.len();
        self.stack.push(metamethod);
        self.stack.extend_from_slice(args);
        self.frames.push(Frame::CallContinuation {
            inner: ContinuationFrame {
                bottom: current_bottom,
                continuation: Some(Continuation::new(continuation)),
            },
            callee_bottom: metamethod_bottom,
        });
        self.push_frame(metamethod_bottom).unwrap()
    }
}

fn get_upvalue<'gc>(
    current_thread: GcCell<'gc, LuaThread<'gc>>,
    base: usize,
    lower_stack: &[Value<'gc>],
    stack: &[Value<'gc>],
    upvalue: &Upvalue<'gc>,
) -> Value<'gc> {
    match upvalue {
        Upvalue::Open { thread, index } => {
            if GcCell::ptr_eq(thread, &current_thread) {
                if *index < base {
                    lower_stack[*index]
                } else {
                    stack[*index - base]
                }
            } else {
                thread.borrow().stack[*index]
            }
        }
        Upvalue::Closed(value) => *value,
    }
}

fn set_upvalue<'gc>(
    gc: &'gc GcContext,
    current_thread: GcCell<'gc, LuaThread<'gc>>,
    base: usize,
    lower_stack: &mut [Value<'gc>],
    stack: &mut [Value<'gc>],
    upvalue: &mut Upvalue<'gc>,
    value: Value<'gc>,
) {
    match upvalue {
        Upvalue::Open { thread, index } => {
            if GcCell::ptr_eq(thread, &current_thread) {
                if *index < base {
                    lower_stack[*index] = value;
                } else {
                    stack[*index - base] = value;
                }
            } else {
                thread.borrow_mut(gc).stack[*index] = value;
            }
        }
        Upvalue::Closed(v) => *v = value,
    }
}
