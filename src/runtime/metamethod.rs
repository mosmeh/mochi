use super::{ErrorKind, Instruction, Operation, Vm};
use crate::{
    gc::{GcCell, GcContext, RootSet},
    rebind, to_rooted,
    types::{LuaString, LuaThread, Value},
};
use bstr::B;

macro_rules! metamethods {
    ($($variant:ident => $name:tt,)*) => {
        #[derive(Clone, Copy, Debug)]
        pub enum Metamethod {
            $($variant,)*
        }

        impl Metamethod {
            pub const COUNT: usize = crate::count!($($variant)*);

            pub fn allocate_names<'gc, 'a>(gc: &'a GcContext<'gc>) -> [LuaString<'gc, 'a>; Self::COUNT] {
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

pub(super) fn index_slow_path<'a, 'gc: 'a, K>(
    gc: &mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    table_like: Value<'gc, '_>,
    key: K,
    dest: usize,
) -> Result<(), ErrorKind>
where
    K: Into<Value<'gc, 'a>>,
{
    let key = key.into();
    let index_key = vm.borrow(gc).metamethod_name(Metamethod::Index);
    let mut table_like = rebind!(gc, table_like);
    for _ in 0..2000 {
        let metamethod = if let Value::Table(table) = table_like {
            let metamethod = table
                .borrow(gc)
                .metatable()
                .map(|metatable| metatable.borrow(gc).get_field(index_key))
                .unwrap_or_default();
            if metamethod.is_nil() {
                thread.borrow_mut(gc).stack[dest] = Value::Nil;
                return Ok(());
            }
            metamethod
        } else {
            let metamethod = vm
                .borrow(gc)
                .metatable_of_object(gc, table_like)
                .map(|metatable| metatable.borrow(gc).get_field(index_key))
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
                to_rooted!(roots, thread, metamethod, table_like, key);
                let result =
                    call_metamethod(gc, roots, vm, *thread, *metamethod, &[*table_like, *key])?;
                thread.borrow_mut(gc).stack[dest] = rebind!(gc, result);
                return Ok(());
            }
            Value::Table(table) => {
                let key = rebind!(gc, key);
                let value = table.borrow(gc).get(key);
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
    Err(ErrorKind::other("'__index' chain too long; possible loop"))
}

pub(super) fn new_index_slow_path<'a, 'gc: 'a, K, V>(
    gc: &'a mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    table_like: Value<'gc, '_>,
    key: K,
    value: V,
) -> Result<(), ErrorKind>
where
    K: Into<Value<'gc, 'a>>,
    V: Into<Value<'gc, 'a>>,
{
    let key = key.into();
    let new_index_key = vm.borrow(gc).metamethod_name(Metamethod::NewIndex);
    let mut table_like = rebind!(gc, table_like);
    for _ in 0..2000 {
        let metamethod = if let Value::Table(table) = table_like {
            let metamethod = table
                .borrow(gc)
                .metatable()
                .map(|metatable| metatable.borrow(gc).get_field(new_index_key))
                .unwrap_or_default();
            if metamethod.is_nil() {
                table.borrow_mut(gc).set(key, value)?;
                return Ok(());
            }
            metamethod
        } else {
            let metamethod = vm
                .borrow(gc)
                .metatable_of_object(gc, table_like)
                .map(|metatable| metatable.borrow(gc).get_field(new_index_key))
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
                let value = value.into();
                to_rooted!(roots, thread, metamethod, table_like, key, value);
                call_metamethod(
                    gc,
                    roots,
                    vm,
                    *thread,
                    *metamethod,
                    &[*table_like, *key, *value],
                )?;
                return Ok(());
            }
            Value::Table(table) => {
                let key = rebind!(gc, key);
                let value = table.borrow(gc).get(key);
                if !value.is_nil() {
                    table.borrow_mut(gc).set(key, value)?;
                    return Ok(());
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

pub(super) fn arithmetic_slow_path<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    metamethod: Metamethod,
    a: Value<'gc, 'a>,
    b: Value<'gc, 'a>,
    dest: usize,
) -> Result<(), ErrorKind> {
    let metamethod_value = {
        let a = rebind!(gc, a);
        let b = rebind!(gc, b);
        let vm = vm.borrow(gc);
        vm.metamethod_of_object(gc, metamethod, a)
            .or_else(|| vm.metamethod_of_object(gc, metamethod, b))
    };
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

    to_rooted!(roots, thread, metamethod_value, a, b);
    let result = call_metamethod(gc, roots, vm, *thread, *metamethod_value, &[*a, *b])?;
    thread.borrow_mut(gc).stack[dest] = rebind!(gc, result);
    Ok(())
}

pub(super) fn compare_slow_path<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    metamethod: Metamethod,
    a: Value<'gc, 'a>,
    b: Value<'gc, 'a>,
    pc: usize,
    insn: Instruction,
    next_insn: Instruction,
) -> Result<(), ErrorKind> {
    let metamethod = {
        let a = rebind!(gc, a);
        let b = rebind!(gc, b);
        let vm = vm.borrow(gc);
        vm.metamethod_of_object(gc, metamethod, a)
            .or_else(|| vm.metamethod_of_object(gc, metamethod, b))
            .ok_or_else(|| ErrorKind::TypeError {
                operation: Operation::Compare,
                ty: b.ty(),
            })?
    };

    to_rooted!(roots, thread, metamethod, a, b);
    let result = call_metamethod(gc, roots, vm, *thread, *metamethod, &[*a, *b])?;
    let new_pc = if result.to_boolean() == insn.k() {
        (pc as isize + next_insn.sj() as isize + 1) as usize
    } else {
        pc + 1
    };
    thread.borrow_mut(gc).save_pc(new_pc);
    Ok(())
}

pub(super) fn len_slow_path<'gc>(
    gc: &mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, '_, LuaThread<'gc, '_>>,
    value: Value<'gc, '_>,
    dest: usize,
) -> Result<(), ErrorKind> {
    let metamethod = {
        let value = rebind!(gc, value);
        vm.borrow(gc)
            .metamethod_of_object(gc, Metamethod::Len, value)
            .ok_or_else(|| ErrorKind::TypeError {
                operation: Operation::Length,
                ty: value.ty(),
            })?
    };

    to_rooted!(roots, thread, metamethod, value);
    let result = call_metamethod(gc, roots, vm, *thread, *metamethod, &[*value, *value])?;
    thread.borrow_mut(gc).stack[dest] = rebind!(gc, result);
    Ok(())
}

/*pub(super) fn concat_slow_path<R>(
    &self,
    gc: &'gc GcContext,
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
        .metamethod_of_object(gc, Metamethod::Concat, lhs)
        .or_else(|| self.metamethod_of_object(gc, Metamethod::Concat, rhs))
        .ok_or_else(|| ErrorKind::TypeError {
            operation: Operation::Concatenate,
            ty: rhs.ty(),
        })?;

    Ok(self.push_metamethod_frame_with_continuation(
        gc,
        thread,
        metamethod,
        &[lhs, rhs],
        move |gc, vm, results| {
            let thread = vm.borrow(gc).current_thread();
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
                    vm.borrow(gc).concat_slow_path(
                        gc,
                        &mut thread,
                        lhs_index - 1,
                        concatenated,
                        dest,
                    )?;
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
                vm.borrow(gc)
                    .concat_slow_path(gc, &mut thread, lhs_index, rhs, dest)?;
                return Ok(Action::ReturnArguments);
            }
            strings.reverse();
            stack[dest] = gc.allocate_string(strings.concat()).into();
            Ok(Action::ReturnArguments)
        },
    ))
}*/

/*#[must_use]
pub(super) fn push_metamethod_frame_with_continuation<F>(
    &self,
    gc: &'gc GcContext,
    thread: &mut LuaThread<'gc>,
    metamethod: Value<'gc>,
    args: &[Value<'gc>],
    continuation: F,
) -> ControlFlow<()>
where
    F: 'static
        + Fn(&'gc GcContext, GcCell<Vm<'gc>>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>,
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
    self.push_frame(gc, thread, metamethod_bottom).unwrap()
}*/

pub(super) fn call_metamethod<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
    callee: Value<'gc, 'a>,
    args: &[Value<'gc, 'a>],
) -> Result<Value<'gc, 'a>, ErrorKind> {
    let results = super::call(gc, roots, vm, thread, callee, args)?;
    let result = results.first().copied().unwrap_or_default();
    Ok(result)
}
