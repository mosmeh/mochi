pub(crate) mod instruction;

mod action;
mod bytecode_vm;
mod error;
mod frame;
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
    types::{LuaClosureProto, LuaString, LuaThread, Table, ThreadStatus, Type, Upvalue, Value},
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
            vm.push_frame(&mut thread_ref, 0)?;

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
        match self.thread_stack.as_slice() {
            [.., current] => *current,
            _ => self.main_thread,
        }
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
        match object {
            Value::Table(table) => table.borrow().metatable(),
            Value::UserData(ud) => ud.borrow().metatable(),
            _ => self.metatables[object.ty() as usize],
        }
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

                    let protection_boundary = thread_ref
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
                        self.thread_stack.pop().unwrap();
                        thread_ref.status = ThreadStatus::Error(kind.clone());

                        if self.thread_stack.is_empty() {
                            let traceback = thread_ref.traceback();
                            *thread_ref = LuaThread::new();
                            return Err(RuntimeError { kind, traceback });
                        }
                        drop(thread_ref);

                        let mut resumer_ref = self.thread_stack.last().unwrap().borrow_mut(gc);
                        match resumer_ref.frames.as_mut_slice() {
                            [.., Frame::ResumeContinuation(frame)] => {
                                frame.continuation.as_mut().unwrap().set_args(Err(kind))
                            }
                            _ => unreachable!(),
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

    pub(crate) fn push_frame(
        &self,
        thread: &mut LuaThread<'gc>,
        bottom: usize,
    ) -> Result<ControlFlow<()>, ErrorKind> {
        match thread.stack[bottom] {
            Value::LuaClosure(_) => {
                thread.frames.push(Frame::Lua(LuaFrame::new(bottom)));
                Ok(ControlFlow::Continue(()))
            }
            Value::NativeFunction(_) | Value::NativeClosure(_) => {
                thread.frames.push(Frame::Native { bottom });
                Ok(ControlFlow::Break(()))
            }
            value => match self.metamethod_of_object(Metamethod::Call, value) {
                Some(metatable) => {
                    thread.stack.insert(bottom, metatable);
                    self.push_frame(thread, bottom)
                }
                None => {
                    if let Some((kind, name)) = self.funcname_from_call(thread, bottom) {
                        Err(ErrorKind::other(format!(
                            "attempt to call a nil value ({kind}:{name:?})"
                        )))
                    } else {
                        Err(ErrorKind::TypeError {
                            operation: Operation::Call,
                            ty: value.ty(),
                        })
                    }
                }
            },
        }
    }

    pub(crate) fn funcname_from_call<'a>(
        &self,
        thread: &'a mut LuaThread<'gc>,
        bottom: usize,
    ) -> Option<(&'static str, &'a str)> {
        // if (ci->callstatus & CIST_HOOKED) {  /* was it called inside a hook? */
        // *name = "?";
        // return "hook";
        // }
        // else if (ci->callstatus & CIST_FIN) {  /* was it called as a finalizer? */
        // *name = "__gc";
        // return "metamethod";  /* report it as such */
        // }
        let frame = thread.last_lua_frame()?;
        let closure = thread.stack_closure(frame.bottom)?;
        self.funcname_from_code(&closure.proto, frame.pc - 1)
    }

    pub(crate) fn funcname_from_code<'a>(
        &self,
        // thread: &mut LuaThread<'gc>,
        proto: &'a LuaClosureProto<'gc>,
        pc: usize,
    ) -> Option<(&'static str, &'a str)> {
        let i = proto.code.get(pc)?;
        match i.raw_opcode() {
            opcode::CALL | opcode::TAILCALL => {
                return proto.get_objname(pc, i.a()); // Get function name
            }
            opcode::TFORCALL => {
                // For iterator
                return Some(("for iterator", "for iterator"));
            }
            // TODO: metamethod name
            // Other instructions can do calls through metamethods
            // opcode::SELF | opcode::GETTABUP | opcode::GETTABLE | opcode::GETI | opcode::GETFIELD => {
            //     tm = TMS::TM_INDEX;
            // }
            // opcode::SETTABUP | opcode::SETTABLE | opcode::SETI | opcode::SETFIELD => {
            //     tm = TMS::TM_NEWINDEX;
            // }
            // opcode::MMBIN | opcode::MMBINI | opcode::MMBINK => {
            //     tm = TMS::TM_LAST;
            // }
            // opcode::UNM => tm = TMS::TM_UNM,
            // opcode::BNOT => tm = TMS::TM_BNOT,
            // opcode::LEN => tm = TMS::TM_LEN,
            // opcode::CONCAT => tm = TMS::TM_CONCAT,
            // opcode::EQ => tm = TMS::TM_EQ,
            // // No cases for OP_EQI and OP_EQK, as they don't call metamethods
            // opcode::LT | opcode::LTI | opcode::GTI => tm = TMS::TM_LT,
            // opcode::LE | opcode::LEI | opcode::GEI => tm = TMS::TM_LE,
            // opcode::CLOSE | opcode::RETURN => tm = TMS::TM_CLOSE,
            _ => {}
        }
        None
    }
}

impl<'gc> LuaClosureProto<'gc> {
    // refer to "getobjname" in ldebug.c
    pub(crate) fn get_objname(&self, lastpc: usize, reg: usize) -> Option<(&'static str, &'_ str)> {
        // TODO: getlocalname
        // *name = luaF_getlocalname(p, reg + 1, lastpc);
        // if (*name)  /* is a local? */
        //   return "local";

        let pc = self.find_setreg(lastpc, reg)?;
        let i = *self.code.get(pc)?;
        match i.opcode() {
            OpCode::Move => {
                let b = i.b();
                if b < i.a() {
                    return self.get_objname(pc, b);
                }
            }
            OpCode::GetTabUp => {
                let k = i.c();
                return Some((self.gxf(pc, i, true), self.kname(k)));
            }
            OpCode::GetTable => {
                let k = i.c();
                return Some((self.gxf(pc, i, false), self.rname(pc, k as _)));
            }
            OpCode::GetI => {
                return Some(("field", "integer index"));
            }
            OpCode::GetField => {
                let k = i.c();
                return Some((self.gxf(pc, i, false), self.kname(k)));
            }
            OpCode::GetTabUp => {
                return Some(("upvalue", self.upvalname(i.b())?));
            }
            // OpCode::LoadK | OpCode::LoadKX => {
            //     let b = if op == OpCode::LOADK {
            //         GETARG_Bx(i)
            //     } else {
            //         GETARG_Ax(p.code[pc + 1])
            //     };
            //     if ttisstring(&p.k[b]) {
            //         *name = Some(svalue(&p.k[b]));
            //         return Some("constant");
            //     }
            // }
            // OpCode::Self_ => {
            //     rkname(p, pc, i, name);
            //     return Some("method");
            // }
            _ => {}
        }
        None
    }

    // refer to "findsetreg" in ldebug.c
    /*
     ** Try to find last instruction before 'lastpc' that modified register 'reg'.
     */
    pub(crate) fn find_setreg(&self, mut lastpc: usize, reg: usize) -> Option<usize> {
        if self.code.get(lastpc)?.opcode().modes().mm {
            // Previous instruction was not actually executed.
            lastpc = lastpc.checked_sub(1)?;
        }

        let mut setreg = None; // Keep last instruction that changed 'reg'.
        let mut jmptarget = 0; // Any code before this address is conditional.
        let mut pc = 0;
        while pc < lastpc {
            let i = self.code.get(pc)?;
            let op: OpCode = i.opcode();
            let a = i.a();

            let mut change: bool = false; // True if current instruction changed 'reg'.
            match op {
                OpCode::LoadNil => {
                    // Set registers from 'a' to 'a+b'.
                    let b = i.b();
                    change = a <= reg && reg <= a + b;
                }
                OpCode::TForCall => {
                    // Affect all regs above its base.
                    change = reg >= a + 2;
                }
                OpCode::Call | OpCode::TailCall => {
                    // Affect all registers above base.
                    change = reg >= a;
                }
                OpCode::Jmp => {
                    // Doesn't change registers, but changes 'jmptarget'.
                    let b = i.sj();
                    let dest = (pc as i32 + 1 + b) as usize;

                    // Jump does not skip 'lastpc' and is larger than the current one?
                    if dest <= lastpc && dest > jmptarget {
                        jmptarget = dest; // Update 'jmptarget'.
                    }
                    change = false;
                }
                _ => {
                    // Any instruction that sets A.
                    change = op.modes().set_a && reg == a;
                }
            }

            if change {
                setreg = filterpc(pc, jmptarget);
            }

            pc += 1;
        }

        fn filterpc(pc: usize, jmptarget: usize) -> Option<usize> {
            /* is code conditional (inside a jump)? */
            if pc < jmptarget {
                None /* cannot know who sets that register */
            } else {
                /* current position sets that register */
                Some(pc)
            }
        }

        setreg
    }

    fn kname(&self, k: u8) -> &'_ str {
        self.constants
            .get(k as usize)
            .and_then(Value::as_lua_string)
            .and_then(|s| s.as_str().ok())
            .unwrap_or("?")
    }

    fn rname(&self, pc: usize, c: usize) -> &'_ str {
        self.get_objname(pc, c)
            .map(|x| if x.0 == "c" { x.1 } else { "?" })
            .unwrap_or("??")
    }

    fn upvalname(&self, uv: usize) -> Option<&str> {
        // TString *s = check_exp(uv < p->sizeupvalues, p->upvalues[uv].name);
        // if (s == NULL) return "?";
        // else return getstr(s);
        None
    }

    /*
     ** Check whether table being indexed by instruction 'i' is the
     ** environment '_ENV'
     */
    fn gxf(&self, pc: usize, i: Instruction, isup: bool) -> &'static str {
        let t = i.b(); /* table index */
        // const char *name;  /* name of indexed variable */
        let name = if isup {
            /* is an upvalue? */
            self.upvalname(t)
        } else {
            self.get_objname(pc, t).map(|x| x.1)
        };
        if name.filter(|&n| n == "_ENV").is_some() {
            "global"
        } else {
            "field"
        }
    }
}

impl<'gc> LuaThread<'gc> {
    fn save_pc(&mut self, pc: usize) {
        match self.frames.as_mut_slice() {
            [.., Frame::Lua(frame)] => frame.pc = pc,
            _ => unreachable!(),
        }
    }

    fn last_lua_frame(&self) -> Option<&LuaFrame> {
        match self.frames.as_slice() {
            [.., Frame::Lua(frame)] => Some(frame),
            _ => None,
        }
    }

    fn stack_closure(&self, n: usize) -> Option<&'_ LuaClosure<'gc>> {
        self.stack.get(n).and_then(|v| v.as_lua_closure())
    }
}

impl<'gc> Upvalue<'gc> {
    fn get(
        &self,
        current_thread: GcCell<'gc, LuaThread<'gc>>,
        base: usize,
        lower_stack: &[Value<'gc>],
        stack: &[Value<'gc>],
    ) -> Value<'gc> {
        match self {
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

    fn set(
        &mut self,
        gc: &'gc GcContext,
        current_thread: GcCell<'gc, LuaThread<'gc>>,
        base: usize,
        lower_stack: &mut [Value<'gc>],
        stack: &mut [Value<'gc>],
        value: Value<'gc>,
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
