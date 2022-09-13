use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    binary_chunk,
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Metamethod, Vm},
    types::{Action, Integer, LuaThread, StackWindow, Table, Type, Value},
};
use bstr::B;
use std::ops::Range;

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("byte"), string_byte),
            (B("char"), string_char),
            (B("dump"), string_dump),
            (B("len"), string_len),
            (B("lower"), string_lower),
            (B("sub"), string_sub),
            (B("rep"), string_rep),
            (B("reverse"), string_reverse),
            (B("upper"), string_upper),
        ],
    );

    let string = gc.allocate_cell(table);

    let mut metatable = Table::new();
    metatable.set_field(vm.metamethod_name(Metamethod::Index), string);
    vm.set_metatable_of_type(Type::String, gc.allocate_cell(metatable));

    string
}

fn string_byte<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack(&window);

    let s = stack.arg(0);
    let s = s.to_string()?;

    let i = stack.arg(1).to_integer_or(1)?;
    let j = stack.arg(2).to_integer_or(i)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    let num_results = range.len();
    thread.ensure_stack(&mut window, num_results);
    let stack = thread.stack_mut(&window);
    for (x, b) in stack.iter_mut().zip(s[range].iter()) {
        *x = (*b as Integer).into();
    }
    Ok(Action::Return { num_results })
}

fn string_char<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let len = stack.args().len();
    let mut bytes = Vec::with_capacity(len);
    for nth in 0..len {
        let ch = stack.arg(nth).to_integer()?;
        if let Ok(ch) = ch.try_into() {
            bytes.push(ch);
        } else {
            return Err(ErrorKind::ArgumentError {
                nth,
                message: "value out of range",
            });
        }
    }

    stack[0] = gc.allocate_string(bytes).into();
    Ok(Action::Return { num_results: 1 })
}

fn string_dump<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    match stack.arg(0).get() {
        Some(Value::LuaClosure(closure)) => {
            let mut binary = Vec::new();
            binary_chunk::dump(&mut binary, &closure.proto)?;
            stack[0] = gc.allocate_string(binary).into();
            Ok(Action::Return { num_results: 1 })
        }
        Some(value) if value.ty() == Type::Function => Err(ErrorKind::ExplicitError(
            "unable to dump given function".to_owned(),
        )),
        value => Err(ErrorKind::ArgumentTypeError {
            nth: 0,
            expected_type: "function",
            got_type: value.map(|value| value.ty().name()),
        }),
    }
}

fn string_len<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let len = stack.arg(0).to_string()?.len() as Integer;
    stack[0] = len.into();
    Ok(Action::Return { num_results: 1 })
}

fn string_lower<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let lower = stack.arg(0).to_string()?.to_ascii_lowercase();
    let lower = gc.allocate_string(lower);
    stack[0] = lower.into();
    Ok(Action::Return { num_results: 1 })
}

fn string_sub<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let s = stack.arg(0);
    let s = s.to_string()?;

    let i = stack.arg(1).to_integer()?;
    let j = stack.arg(2).to_integer_or(-1)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    stack[0] = gc.allocate_string(&s[range]).into();
    Ok(Action::Return { num_results: 1 })
}

fn string_rep<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let s = stack.arg(0);
    let s = s.to_string()?;
    let n = stack.arg(1).to_integer()?;
    let sep = stack.arg(2);
    let sep = sep.to_string_or(B(""))?;

    let count = n.max(0) as usize;
    let string = if count > 0 {
        let mut string = Vec::with_capacity(count * s.len() + (count - 1) * sep.len());
        for _ in 0..count - 1 {
            string.extend_from_slice(s.as_ref());
            string.extend_from_slice(sep.as_ref());
        }
        string.extend_from_slice(s.as_ref());
        string
    } else {
        Vec::new()
    };

    stack[0] = gc.allocate_string(string).into();
    Ok(Action::Return { num_results: 1 })
}

fn string_reverse<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let mut string = stack.arg(0).to_string()?.to_vec();
    string.reverse();
    stack[0] = gc.allocate_string(string).into();
    Ok(Action::Return { num_results: 1 })
}

fn string_upper<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let upper = stack.arg(0).to_string()?.to_ascii_uppercase();
    let upper = gc.allocate_string(upper);
    stack[0] = upper.into();
    Ok(Action::Return { num_results: 1 })
}

fn indices_to_range(i: Integer, j: Integer, len: Integer) -> Range<usize> {
    let start = match i {
        0 => 0,
        _ if i < -len => 0,
        1.. => i - 1,
        _ => len + i,
    } as usize;
    let end = match j {
        0.. => j,
        _ if j < -len => 0,
        _ if j > len => len,
        _ => len + j + 1,
    } as usize;
    start..end
}
