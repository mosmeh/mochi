use super::{get_number_arg, get_string_arg};
use crate::{
    gc::GcHeap,
    types::{NativeFunction, StackWindow, Table},
    vm::{ErrorKind, Vm},
};
use bstr::{ByteSlice, ByteVec, B};

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("clock")), NativeFunction::new(clock));
    table.set_field(
        heap.allocate_string(B("difftime")),
        NativeFunction::new(difftime),
    );
    table.set_field(
        heap.allocate_string(B("getenv")),
        NativeFunction::new(getenv),
    );
    table
}

fn clock(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    vm.stack_mut(window)[0] = cpu_time::ProcessTime::now()
        .as_duration()
        .as_secs_f64()
        .into();
    Ok(1)
}

fn difftime(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = (get_number_arg(stack, 1)? - get_number_arg(stack, 2)?).into();
    Ok(1)
}

fn getenv(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let var = get_string_arg(stack, 1)?;
    stack[0] = var
        .to_os_str()
        .ok()
        .and_then(std::env::var_os)
        .and_then(|s| Vec::from_os_string(s).ok())
        .map(|s| heap.allocate_string(s).into())
        .unwrap_or_default();
    Ok(1)
}
