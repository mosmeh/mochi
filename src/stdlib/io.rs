use crate::{
    gc::GcHeap,
    types::{NativeFunction, StackWindow, Table},
    vm::{ErrorKind, Vm},
};
use bstr::B;
use std::io::Write;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("flush")), NativeFunction::new(flush));
    table.set_field(heap.allocate_string(B("write")), NativeFunction::new(write));
    table
}

fn flush(_: &mut Vm, _: StackWindow) -> Result<usize, ErrorKind> {
    std::io::stdout().flush()?;
    Ok(0)
}

fn write(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window);
    let mut stdout = std::io::stdout().lock();
    for x in &stack[1..] {
        write!(stdout, "{}", x)?;
    }
    Ok(0)
}
