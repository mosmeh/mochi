use super::StackExt;
use crate::{
    gc::GcHeap,
    types::{Integer, NativeFunction, StackWindow, Table},
    vm::{ErrorKind, Vm},
};
use bstr::B;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("pack")), NativeFunction::new(pack));
    table.set_field(
        heap.allocate_string(B("unpack")),
        NativeFunction::new(unpack),
    );
    table
}

fn pack(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let mut table = Table::from(stack.args().to_vec());
    table.set_field(heap.allocate_string(B("n")), stack.args().len() as Integer);
    stack[0] = heap.allocate_cell(table).into();
    Ok(1)
}

fn unpack(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window.clone());
    let table = stack.arg(0);
    let table = table.as_table()?;
    let start = stack.arg(1).to_integer_or(1)?;
    let end = stack.arg(2).to_integer_or_else(|| table.lua_len())?;

    let n = (end - start + 1) as usize;
    let window = vm.ensure_stack(window, n);
    for (dest, src) in vm.stack_mut(window).iter_mut().zip(start..=end) {
        *dest = table.get(src);
    }
    Ok(n)
}
