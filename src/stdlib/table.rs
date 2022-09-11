use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Action, Integer, LuaThread, NativeFunction, StackWindow, Table, Value},
};
use bstr::B;

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("concat")), NativeFunction::new(concat));
    table.set_field(gc.allocate_string(B("insert")), NativeFunction::new(insert));
    table.set_field(gc.allocate_string(B("pack")), NativeFunction::new(pack));
    table.set_field(gc.allocate_string(B("remove")), NativeFunction::new(remove));
    table.set_field(gc.allocate_string(B("unpack")), NativeFunction::new(unpack));
    gc.allocate_cell(table)
}

fn concat<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let table = stack.arg(0);
    let table = table.borrow_as_table()?;
    let sep = stack.arg(1);
    let sep = sep.to_string_or(B(""))?;
    let i = stack.arg(2).to_integer_or(1)?;
    let j = stack.arg(3).to_integer_or_else(|| table.lua_len())?;

    let mut strings = Vec::new();
    for index in i..=j {
        let value = table.get(index);
        if let Some(string) = value.to_string() {
            strings.push(string.to_vec());
        } else {
            return Err(ErrorKind::ExplicitError(format!(
                "invalid value ({}) at index {} in table for 'concat'",
                value.ty().name(),
                index
            )));
        }
    }

    let concatenated = strings.join(sep.as_ref());
    stack[0] = gc.allocate_string(concatenated).into();
    Ok(Action::Return { num_results: 1 })
}

fn insert<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let table = stack.arg(0);
    let mut table = table.borrow_as_table_mut(gc)?;
    let len = table.lua_len();

    match stack.args().len() {
        2 => table.set(len + 1, stack.args()[1])?,
        3 => {
            let pos = stack.arg(1).to_integer()?;
            if pos > len + 1 {
                return Err(ErrorKind::ArgumentError {
                    nth: 1,
                    message: "position out of bounds",
                });
            }
            for i in (1..=len).rev() {
                let value = table.get(i);
                table.set(i + 1, value)?;
            }
            table.set(pos, stack.args()[2])?;
        }
        _ => {
            return Err(ErrorKind::ExplicitError(
                "wrong number of arguments to 'insert'".to_owned(),
            ))
        }
    };
    Ok(Action::Return { num_results: 0 })
}

fn pack<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let mut table = Table::from(stack.args().to_vec());
    table.set_field(gc.allocate_string(B("n")), stack.args().len() as Integer);
    stack[0] = gc.allocate_cell(table).into();
    Ok(Action::Return { num_results: 1 })
}

fn remove<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let table = stack.arg(0);
    let mut table = table.borrow_as_table_mut(gc)?;
    let len = table.lua_len();

    let pos = stack.arg(1).to_integer_or(len)?;
    if pos > len + 1 {
        return Err(ErrorKind::ArgumentError {
            nth: 1,
            message: "position out of bounds",
        });
    }

    stack[0] = table.get(pos);
    for i in pos..len {
        let value = table.get(i + 1);
        table.set(i, value)?;
    }
    table.set(len, Value::Nil)?;
    Ok(Action::Return { num_results: 1 })
}

fn unpack<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack(&window);
    let table = stack.arg(0);
    let table = table.borrow_as_table()?;
    let start = stack.arg(1).to_integer_or(1)?;
    let end = stack.arg(2).to_integer_or_else(|| table.lua_len())?;

    let n = (end - start + 1) as usize;
    thread.ensure_stack(&mut window, n);
    for (dest, src) in thread.stack_mut(&window).iter_mut().zip(start..=end) {
        *dest = table.get(src);
    }
    Ok(Action::Return { num_results: n })
}
