pub(crate) mod instruction;

mod action;
mod bytecode_vm;
mod error;
mod frame;
mod metamethod;
mod opcode;
mod ops;

pub use error::{ErrorKind, Operation, RuntimeError};
pub(crate) use frame::{Frame, LuaFrame};
pub use instruction::Instruction;
pub use metamethod::Metamethod;
pub use opcode::OpCode;

use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, GcLifetime, RootSet, Tracer},
    new_root, rebind, to_rooted,
    types::{LuaString, LuaThread, Table, ThreadStatus, Type, Upvalue, Value},
    Error, LuaClosure,
};
use std::path::Path;

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
        F: for<'gc, 'a> FnOnce(
            &'a GcContext<'gc>,
            GcCell<'gc, '_, Vm<'gc, '_>>,
        ) -> Result<
            Value<'gc, 'a>,
            Box<dyn std::error::Error + Send + Sync + 'static>,
        >,
    {
        let result = self.heap.with(|gc, roots, vm| {
            let value = match f(gc, vm) {
                Ok(value) => value,
                Err(err) => return Err(ErrorKind::External(err.into())),
            };
            let main_thread = {
                let mut vm = vm.borrow_mut(gc);
                assert!(vm.thread_stack.is_empty());
                let main_thread = vm.main_thread;
                vm.thread_stack.push(main_thread);
                main_thread
            };
            {
                let mut thread = main_thread.borrow_mut(gc);
                assert!(thread.stack.is_empty());
                assert!(thread.frames.is_empty());
                assert!(thread.open_upvalues.is_empty());
                thread.status = ThreadStatus::Unresumable;
                thread.stack.push(value);
            }
            to_rooted!(roots, main_thread);
            call_value(gc, roots, vm, *main_thread, 0)
        });
        match result {
            Ok(()) => self.heap.with(execute),
            Err(kind) => Err(RuntimeError {
                kind,
                traceback: Vec::new(),
            }),
        }
    }
}

pub struct Vm<'gc, 'a> {
    registry: GcCell<'gc, 'a, Table<'gc, 'a>>,
    main_thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
    globals: GcCell<'gc, 'a, Table<'gc, 'a>>,
    thread_stack: Vec<GcCell<'gc, 'a, LuaThread<'gc, 'a>>>,
    metamethod_names: [LuaString<'gc, 'a>; Metamethod::COUNT],
    metatables: [Option<GcCell<'gc, 'a, Table<'gc, 'a>>>; Type::COUNT],
}

unsafe impl GarbageCollect for Vm<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.registry.trace(tracer);
        self.main_thread.trace(tracer);
        self.globals.trace(tracer);
        self.thread_stack.trace(tracer);
        self.metamethod_names.trace(tracer);
        self.metatables.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for Vm<'gc, '_> {
    type Aged = Vm<'gc, 'a>;
}

impl<'gc, 'a> Vm<'gc, 'a> {
    pub(crate) fn new(gc: &'a GcContext<'gc>) -> Self {
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

    pub fn registry(&self) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
        self.registry
    }

    pub fn main_thread(&self) -> GcCell<'gc, 'a, LuaThread<'gc, 'a>> {
        self.main_thread
    }

    pub fn current_thread(&self) -> GcCell<'gc, 'a, LuaThread<'gc, 'a>> {
        match self.thread_stack.as_slice() {
            [.., current] => *current,
            _ => self.main_thread,
        }
    }

    pub fn globals(&self) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
        self.globals
    }

    pub fn load_stdlib(&mut self, gc: &'a GcContext<'gc>) {
        crate::stdlib::load(gc, self);
    }

    pub fn load<B, S>(
        &self,
        gc: &'a GcContext<'gc>,
        bytes: B,
        source: S,
    ) -> Result<LuaClosure<'gc, 'a>, Error>
    where
        B: AsRef<[u8]>,
        S: AsRef<[u8]>,
    {
        let proto = crate::load(gc, bytes, source)?;
        let mut closure = LuaClosure::from(gc.allocate(proto));
        closure
            .upvalues
            .push(gc.allocate_cell(Upvalue::from(Value::Table(self.globals))));
        Ok(closure)
    }

    pub fn load_file<P: AsRef<Path>>(
        &self,
        gc: &'a GcContext<'gc>,
        path: P,
    ) -> Result<LuaClosure<'gc, 'a>, Error> {
        let proto = crate::load_file(gc, path)?;
        let mut closure = LuaClosure::from(gc.allocate(proto));
        closure
            .upvalues
            .push(gc.allocate_cell(Upvalue::from(Value::Table(self.globals))));
        Ok(closure)
    }

    pub fn metamethod_name(&self, metamethod: Metamethod) -> LuaString<'gc, 'a> {
        self.metamethod_names[metamethod as usize]
    }

    pub fn metatable_of_object(
        &self,
        gc: &'a GcContext<'gc>,
        object: Value<'gc, 'a>,
    ) -> Option<GcCell<'gc, 'a, Table<'gc, 'a>>> {
        match object {
            Value::Table(table) => table.borrow(gc).metatable(),
            Value::UserData(ud) => ud.borrow(gc).metatable(),
            _ => self.metatables[object.ty() as usize],
        }
    }

    pub fn metamethod_of_object(
        &self,
        gc: &'a GcContext<'gc>,
        metamethod: Metamethod,
        object: Value<'gc, 'a>,
    ) -> Option<Value<'gc, 'a>> {
        self.metatable_of_object(gc, object).and_then(|metatable| {
            let metamethod = metatable
                .borrow(gc)
                .get_field(self.metamethod_name(metamethod));
            (!metamethod.is_nil()).then_some(metamethod)
        })
    }

    pub fn set_metatable_of_type<T>(&mut self, ty: Type, metatable: T)
    where
        T: Into<Option<GcCell<'gc, 'a, Table<'gc, 'a>>>>,
    {
        self.metatables[ty as usize] = metatable.into();
    }
}

fn execute<'gc>(
    gc: &mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
) -> Result<(), RuntimeError> {
    bytecode_vm::execute_lua_frame(gc, roots, vm).unwrap();

    let mut vm = vm.borrow_mut(gc);
    let thread = vm.current_thread();
    let thread_ref = thread.borrow(gc);
    debug_assert!(thread_ref.frames.is_empty());
    let coroutine = vm.thread_stack.pop().unwrap();
    debug_assert!(GcCell::ptr_eq(&coroutine, &thread));
    Ok(())

    /*let thread = vm.borrow(gc).current_thread();
    let mut thread_ref = thread.borrow_mut(gc);

    let protection_boundary =
        thread_ref
            .frames
            .iter_mut()
            .enumerate()
            .rev()
            .find_map(|(i, frame)| match frame {
                Frame::ProtectedCallContinuation {
                    inner,
                    callee_bottom,
                } => {
                    inner
                        .continuation
                        .as_mut()
                        .unwrap()
                        .set_args(Err(kind.clone()));
                    Some((i, *callee_bottom))
                }
                _ => None,
            });

    if let Some((frame_index, boundary)) = protection_boundary {
        thread_ref.close_upvalues(gc, boundary);
        thread_ref.frames.truncate(frame_index + 1);
    } else {
        vm.borrow_mut(gc).thread_stack.pop().unwrap();
        thread_ref.status = ThreadStatus::Error(kind.clone());

        if vm.borrow(gc).thread_stack.is_empty() {
            let traceback = thread_ref.traceback(gc);
            *thread_ref = LuaThread::new();
            return Err(RuntimeError { kind, traceback });
        }
        drop(thread_ref);

        let mut resumer_ref = vm.borrow(gc).thread_stack.last().unwrap().borrow_mut(gc);
        match resumer_ref.frames.as_mut_slice() {
            [.., Frame::ResumeContinuation(frame)] => {
                frame.continuation.as_mut().unwrap().set_args(Err(kind))
            }
            _ => unreachable!(),
        }
    }*/
}

pub(crate) fn call_value<'gc>(
    gc: &mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    bottom: usize,
) -> Result<(), ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    match thread_ref.stack[bottom] {
        Value::LuaClosure(_) => {
            thread_ref.frames.push(Frame::Lua(LuaFrame::new(bottom)));
            Ok(())
        }
        Value::NativeFunction(f) => {
            thread_ref.frames.push(Frame::Native { bottom });
            let args = thread_ref.stack.split_off(bottom);
            drop(thread_ref);
            to_rooted!(roots, args);
            let results = (f.0)(gc, roots, vm, &args)?;
            let mut results = rebind!(gc, results);
            let mut thread_ref = thread.borrow_mut(gc);
            thread_ref.stack.append(&mut results);
            thread_ref.frames.pop().unwrap();
            Ok(())
        }
        Value::NativeClosure(f) => {
            thread_ref.frames.push(Frame::Native { bottom });
            let args = thread_ref.stack.split_off(bottom);
            drop(thread_ref);
            to_rooted!(roots, args, f);
            let results = f.call(gc, roots, vm, &args)?;
            let mut results = rebind!(gc, results);
            let mut thread_ref = thread.borrow_mut(gc);
            thread_ref.stack.append(&mut results);
            thread_ref.frames.pop().unwrap();
            Ok(())
        }
        value => {
            match vm
                .borrow(gc)
                .metamethod_of_object(gc, Metamethod::Call, value)
            {
                Some(metatable) => thread_ref.stack.insert(bottom, metatable),
                None => {
                    return Err(ErrorKind::TypeError {
                        operation: Operation::Call,
                        ty: value.ty(),
                    })
                }
            };
            drop(thread_ref);
            call_value(gc, roots, vm, thread, bottom)
        }
    }
}

impl<'gc> LuaThread<'gc, '_> {
    fn save_pc(&mut self, pc: usize) {
        match self.frames.as_mut_slice() {
            [.., Frame::Lua(frame)] => frame.pc = pc,
            _ => unreachable!(),
        }
    }
}

impl<'gc, 'a> Upvalue<'gc, 'a> {
    fn get(
        &self,
        gc: &'a GcContext<'gc>,
        current_thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
        base: usize,
        lower_stack: &[Value<'gc, 'a>],
        stack: &[Value<'gc, 'a>],
    ) -> Value<'gc, 'a> {
        match self {
            Upvalue::Open { thread, index } => {
                if GcCell::ptr_eq(thread, &current_thread) {
                    if *index < base {
                        lower_stack[*index]
                    } else {
                        stack[*index - base]
                    }
                } else {
                    thread.borrow(gc).stack[*index]
                }
            }
            Upvalue::Closed(value) => *value,
        }
    }

    fn set(
        &mut self,
        gc: &'a GcContext<'gc>,
        current_thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
        base: usize,
        lower_stack: &mut [Value<'gc, 'a>],
        stack: &mut [Value<'gc, 'a>],
        value: Value<'gc, 'a>,
    ) {
        match self {
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
}
