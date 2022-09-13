use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Action, LuaThread, StackWindow, Table},
};
use bstr::B;
use std::io::Write;

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[(B("flush"), io_flush), (B("write"), io_write)],
    );
    gc.allocate_cell(table)
}

fn io_flush<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    _: GcCell<LuaThread<'gc>>,
    _: StackWindow,
) -> Result<Action, ErrorKind> {
    std::io::stdout().flush()?;
    Ok(Action::Return { num_results: 0 })
}

fn io_write<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let mut stdout = std::io::stdout().lock();
    for i in 0..stack.args().len() {
        stdout.write_all(stack.arg(i).to_string()?.as_ref())?;
    }
    Ok(Action::Return { num_results: 0 })
}
