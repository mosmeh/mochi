use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{LuaThread, NativeFunction, StackWindow, Table},
};
use bstr::{ByteSlice, ByteVec, B};

pub fn load<'gc>(gc: &'gc GcContext, global_table: GcCell<'gc, Table<'gc>>) {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("clock")), NativeFunction::new(clock));
    table.set_field(
        gc.allocate_string(B("difftime")),
        NativeFunction::new(difftime),
    );
    table.set_field(gc.allocate_string(B("getenv")), NativeFunction::new(getenv));
    global_table
        .borrow_mut(gc)
        .set_field(gc.allocate_string(B("os")), gc.allocate_cell(table));
}

fn clock<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    thread.borrow_mut(gc).stack_mut(window)[0] = cpu_time::ProcessTime::now()
        .as_duration()
        .as_secs_f64()
        .into();
    Ok(1)
}

fn difftime<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = (stack.arg(0).to_number()? - stack.arg(1).to_number()?).into();
    Ok(1)
}

fn getenv<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack
        .arg(0)
        .to_string()?
        .to_os_str()
        .ok()
        .and_then(std::env::var_os)
        .and_then(|s| Vec::from_os_string(s).ok())
        .map(|s| gc.allocate_string(s).into())
        .unwrap_or_default();
    Ok(1)
}
