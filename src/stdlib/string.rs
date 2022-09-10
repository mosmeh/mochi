use super::helpers::StackExt;
use crate::{
    binary_chunk,
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Metamethod, Vm},
    types::{Action, Integer, LuaThread, NativeFunction, StackWindow, Table, Type, Value},
};
use bstr::B;
use std::ops::Range;

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("byte")), NativeFunction::new(byte));
    table.set_field(gc.allocate_string(B("char")), NativeFunction::new(char));
    table.set_field(gc.allocate_string(B("dump")), NativeFunction::new(dump));
    table.set_field(gc.allocate_string(B("len")), NativeFunction::new(len));
    table.set_field(gc.allocate_string(B("lower")), NativeFunction::new(lower));
    table.set_field(gc.allocate_string(B("sub")), NativeFunction::new(sub));
    table.set_field(gc.allocate_string(B("rep")), NativeFunction::new(rep));
    table.set_field(
        gc.allocate_string(B("reverse")),
        NativeFunction::new(reverse),
    );
    table.set_field(gc.allocate_string(B("upper")), NativeFunction::new(upper));

    let table = Value::from(gc.allocate_cell(table));
    vm.globals()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(B("string")), table);

    let mut metatable = Table::new();
    metatable.set_field(vm.metamethod_name(Metamethod::Index), table);
    vm.set_metatable_of_type(gc, Type::String, gc.allocate_cell(metatable));
}

fn byte<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn char<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn dump<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn len<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let len = stack.arg(0).to_string()?.len() as Integer;
    stack[0] = len.into();
    Ok(Action::Return { num_results: 1 })
}

fn lower<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn sub<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn rep<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn reverse<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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

fn upper<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
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
