use crate::{
    gc::GcHeap,
    types::{NativeFunction, StackKey, Table},
    vm::{ErrorKind, Vm},
};
use bstr::B;
use std::io::Write;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("write")), NativeFunction::new(write));
    table
}

fn write(vm: &mut Vm, key: StackKey) -> Result<usize, ErrorKind> {
    let stack = vm.local_stack(key);
    let mut stdout = std::io::stdout().lock();
    for x in &stack[1..] {
        write!(stdout, "{}", x)?;
    }
    Ok(0)
}
