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
                    return Ok(self.push_metamethod_frame_with_continuation(
                        thread,
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
                    return Ok(self.push_metamethod_frame(
                        thread,
                        metamethod,
                        &[table_like, key, value.into()],
                    ));
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

    pub(super) fn arithmetic_slow_path(
        &self,
        thread: &mut LuaThread<'gc>,
        metamethod: Metamethod,
        a: Value<'gc>,
        b: Value<'gc>,
        dest: usize,
    ) -> Result<ControlFlow<()>, ErrorKind> {
        let metamethod_value = self
            .metamethod_of_object(metamethod, a)
            .or_else(|| self.metamethod_of_object(metamethod, b));
        let metamethod_value = match metamethod_value {
            Some(value) => value,
            None => {
                let operation = match metamethod {
                    Metamethod::BAnd
                    | Metamethod::BOr
                    | Metamethod::BXor
                    | Metamethod::Shl
                    | Metamethod::Shr
                    | Metamethod::BNot => Operation::BitwiseOp,
                    _ => Operation::Arithmetic,
                };
                return Err(ErrorKind::TypeError {
                    operation,
                    ty: b.ty(),
                });
            }
        };

        Ok(self.push_metamethod_frame_with_continuation(
            thread,
            metamethod_value,
            &[a, b],
            move |gc, vm, results| {
                vm.current_thread().borrow_mut(gc).stack[dest] =
                    results.first().copied().unwrap_or_default();
                Ok(Action::ReturnArguments)
            },
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

        Ok(self.push_metamethod_frame_with_continuation(
            thread,
            metamethod,
            &[a, b],
            move |gc, vm, results| {
                let cond = results.first().map(Value::to_boolean).unwrap_or_default();
                let new_pc = if cond == insn.k() {
                    (pc as isize + next_insn.sj() as isize + 1) as usize
                } else {
                    pc + 1
                };
                vm.current_thread().borrow_mut(gc).save_pc(new_pc);
                Ok(Action::ReturnArguments)
            },
        ))
    }

    pub(super) fn len_slow_path(
        &self,
        thread: &mut LuaThread<'gc>,
        value: Value<'gc>,
        dest: usize,
    ) -> Result<ControlFlow<()>, ErrorKind> {
        let metamethod = self
            .metamethod_of_object(Metamethod::Len, value)
            .ok_or_else(|| ErrorKind::TypeError {
                operation: Operation::Length,
                ty: value.ty(),
            })?;

        Ok(self.push_metamethod_frame_with_continuation(
            thread,
            metamethod,
            &[value, value],
            move |gc, vm, results| {
                vm.current_thread().borrow_mut(gc).stack[dest] =
                    results.first().copied().unwrap_or_default();
                Ok(Action::ReturnArguments)
            },
        ))
    }

    pub(super) fn concat_slow_path<R>(
        &self,
        thread: &mut LuaThread<'gc>,
        lhs_index: usize,
        rhs: R,
        dest: usize,
    ) -> Result<ControlFlow<()>, ErrorKind>
    where
        R: Into<Value<'gc>>,
    {
        let lhs = thread.stack[dest + lhs_index];
        let rhs = rhs.into();
        let metamethod = self
            .metamethod_of_object(Metamethod::Concat, lhs)
            .or_else(|| self.metamethod_of_object(Metamethod::Concat, rhs))
            .ok_or_else(|| ErrorKind::TypeError {
                operation: Operation::Concatenate,
                ty: rhs.ty(),
            })?;

        Ok(self.push_metamethod_frame_with_continuation(
            thread,
            metamethod,
            &[lhs, rhs],
            move |gc, vm, results| {
                let thread = vm.current_thread();
                let mut thread = thread.borrow_mut(gc);
                let stack = thread.stack.as_mut_slice();

                let concatenated = results.first().copied().unwrap_or_default();
                if lhs_index == 0 {
                    stack[dest] = concatenated;
                    return Ok(Action::ReturnArguments);
                }

                let s = match concatenated.to_string() {
                    Some(s) => s,
                    None => {
                        vm.concat_slow_path(&mut thread, lhs_index - 1, concatenated, dest)?;
                        return Ok(Action::ReturnArguments);
                    }
                };

                let mut strings = vec![s];
                for (i, value) in stack[dest..].iter().take(lhs_index).enumerate().rev() {
                    if let Some(string) = value.to_string() {
                        strings.push(string);
                        continue;
                    }
                    let (lhs_index, rhs) = match strings.len() {
                        0 => unreachable!(),
                        1 => (i, concatenated),
                        _ => {
                            strings.reverse();
                            (i, gc.allocate_string(strings.concat()).into())
                        }
                    };
                    vm.concat_slow_path(&mut thread, lhs_index, rhs, dest)?;
                    return Ok(Action::ReturnArguments);
                }
                strings.reverse();
                stack[dest] = gc.allocate_string(strings.concat()).into();
                Ok(Action::ReturnArguments)
            },
        ))
    }

    #[must_use]
    pub(super) fn push_metamethod_frame(
        &self,
        thread: &mut LuaThread<'gc>,
        metamethod: Value<'gc>,
        args: &[Value<'gc>],
    ) -> ControlFlow<()> {
        let metamethod_bottom = thread.stack.len();
        thread.stack.push(metamethod);
        thread.stack.extend_from_slice(args);
        self.push_frame(thread, metamethod_bottom).unwrap()
    }

    #[must_use]
    pub(super) fn push_metamethod_frame_with_continuation<F>(
        &self,
        thread: &mut LuaThread<'gc>,
        metamethod: Value<'gc>,
        args: &[Value<'gc>],
        continuation: F,
    ) -> ControlFlow<()>
    where
        F: 'static
            + Fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
    {
        let current_bottom = match thread.frames.as_slice() {
            [.., Frame::Lua(frame)] => frame.bottom,
            _ => unreachable!(),
        };
        let metamethod_bottom = thread.stack.len();
        thread.stack.push(metamethod);
        thread.stack.extend_from_slice(args);
        thread.frames.push(Frame::CallContinuation {
            inner: ContinuationFrame {
                bottom: current_bottom,
                continuation: Some(Continuation::new(continuation)),
            },
            callee_bottom: metamethod_bottom,
        });
        self.push_frame(thread, metamethod_bottom).unwrap()
    }
}
