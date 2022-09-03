use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{LuaThread, NativeFunction, StackWindow, Table},
};
use bstr::B;
use std::io::Write;

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("flush")), NativeFunction::new(flush));
    table.set_field(gc.allocate_string(B("write")), NativeFunction::new(write));
    vm.globals()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(B("io")), gc.allocate_cell(table));
}

fn flush<'gc>(
    _: &'gc GcContext,
    _: &Vm<'gc>,
    _: GcCell<LuaThread<'gc>>,
    _: StackWindow,
) -> Result<usize, ErrorKind> {
    std::io::stdout().flush()?;
    Ok(0)
}

fn write<'gc>(
    _: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let mut stdout = std::io::stdout().lock();
    for i in 0..stack.args().len() {
        stdout.write_all(stack.arg(i).to_string()?.as_ref())?;
    }
    Ok(0)
}
