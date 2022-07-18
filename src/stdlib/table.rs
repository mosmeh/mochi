use super::get_integer_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, NativeFunction, StackWindow, Table, Type},
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
    let mut table = Table::from(stack[1..].to_vec());
    table.set_field(heap.allocate_string(B("n")), (stack.len() - 1) as Integer);
    stack[0] = heap.allocate_cell(table).into();
    Ok(1)
}

fn unpack(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window.clone());
    let table_value = stack[1];
    let table = table_value
        .as_table()
        .ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth: 1,
            expected_type: Type::Table,
            got_type: table_value.ty(),
        })?;
    let start = if stack.len() >= 3 {
        get_integer_arg(vm, window.clone(), 2)?
    } else {
        1
    };
    let end = if stack.len() >= 4 {
        get_integer_arg(vm, window.clone(), 3)?
    } else {
        table.lua_len()
    };

    let n = (end - start + 1) as usize;
    let window = vm.ensure_stack(window, n);
    for (dest, src) in vm.stack_mut(window).iter_mut().zip(start..=end) {
        *dest = table.get(src);
    }
    Ok(n)
}
