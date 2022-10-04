use super::{
    Action, Continuation, ContinuationFrame, ErrorKind, Frame, Instruction, Operation, Vm,
};
use crate::{
    gc::GcContext,
    types::{LuaString, LuaThread, Value},
};
use bstr::B;
use std::ops::ControlFlow;

macro_rules! metamethods {
    ($($variant:ident => $name:tt,)*) => {
        #[allow(dead_code)]
        #[derive(Clone, Copy)]
        pub enum Metamethod {
            $($variant,)*
        }

        impl Metamethod {
            pub const COUNT: usize = crate::count!($($variant)*);

            pub fn allocate_names(gc: &GcContext) -> [LuaString; Self::COUNT] {
                [
                    $(gc.allocate_string(B($name)),)*
                ]
            }
        }

        impl From<u8> for Metamethod {
            fn from(i: u8) -> Self {
                const METAMETHODS: [Metamethod; crate::count!($($variant)*)] = [$(Metamethod::$variant,)*];
                METAMETHODS[i as usize]
            }
        }
    }
}

metamethods!(
    Index => "__index",
    NewIndex => "__newindex",
    Gc => "__gc",
    Mode => "__mode",
    Len => "__len",
    Eq => "__eq",
    Add => "__add",
    Sub => "__sub",
    Mul => "__mul",
    Mod => "__mod",
    Pow => "__pow",
    Div => "__div",
    IDiv => "__idiv",
    BAnd => "__band",
    BOr => "__bor",
    BXor => "__bxor",
    Shl => "__shl",
    Shr => "__shr",
    Unm => "__unm",
    BNot => "__bnot",
    Lt => "__lt",
    Le => "__le",
    Concat => "__concat",
    Call => "__call",
    Close => "__close",
);

impl<'gc> Vm<'gc> {
    pub(super) fn index_slow_path<K>(
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
                    return Ok(thread.push_metamethod_frame_with_continuation(
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

    pub(super) fn new_index_slow_path<K, V>(
        &self,
        gc: &'gc GcContext,
        thread: &mut LuaThread<'gc>,
        mut table_like: Value<'gc>,
        key: K,
        value: V,
    ) -> Result<ControlFlow<()>, ErrorKind>
    where
        K: Into<Value<'gc>>,
        V: Into<Value<'gc>>,
    {
        let key = key.into();
        let new_index_key = self.metamethod_name(Metamethod::NewIndex);
        for _ in 0..2000 {
            let metamethod = if let Value::Table(table) = table_like {
                let metamethod = table
                    .borrow()
                    .metatable()
                    .map(|metatable| metatable.borrow().get_field(new_index_key))
                    .unwrap_or_default();
                if metamethod.is_nil() {
                    table.borrow_mut(gc).set(key, value)?;
                    return Ok(ControlFlow::Continue(()));
                }
                metamethod
            } else {
                let metamethod = self
                    .metatable_of_object(table_like)
                    .map(|metatable| metatable.borrow().get_field(new_index_key))
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
                    return Ok(
                        thread.push_metamethod_frame(metamethod, &[table_like, key, value.into()])
                    );
                }
                Value::Table(table) => {
                    let value = table.borrow().get(key);
                    if !value.is_nil() {
                        table.borrow_mut(gc).set(key, value)?;
                        return Ok(ControlFlow::Continue(()));
                    }
                }
                Value::Nil => unreachable!(),
                _ => (),
            }
            table_like = metamethod;
        }
        Err(ErrorKind::other(
            "'__newindex' chain too long; possible loop",
        ))
    }

    pub(super) fn compare_slow_path(
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

        Ok(thread.push_metamethod_frame_with_continuation(
            metamethod,
            &[a, b],
            move |gc, vm, results| {
                let cond = results.first().map(Value::to_boolean).unwrap_or_default();
                let new_pc = if cond == insn.k() {
                    (pc as isize + next_insn.sj() as isize + 1) as usize
                } else {
                    pc + 1
                };
                vm.current_thread().borrow_mut(gc).current_lua_frame().pc = new_pc;
                Ok(Action::ReturnArguments)
            },
        ))
    }
}

impl<'gc> LuaThread<'gc> {
    #[must_use]
    pub(super) fn push_metamethod_frame(
        &mut self,
        metamethod: Value<'gc>,
        args: &[Value<'gc>],
    ) -> ControlFlow<()> {
        let metamethod_bottom = self.stack.len();
        self.stack.push(metamethod);
        self.stack.extend_from_slice(args);
        self.push_frame(metamethod_bottom).unwrap()
    }

    #[must_use]
    pub(super) fn push_metamethod_frame_with_continuation<F>(
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
