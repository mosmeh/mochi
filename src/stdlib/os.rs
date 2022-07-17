use super::get_number_arg;
use crate::{
    gc::GcHeap,
    types::{NativeFunction, StackWindow, Table},
    vm::{ErrorKind, Vm},
};
use bstr::B;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("clock")), NativeFunction::new(clock));
    table.set_field(
        heap.allocate_string(B("difftime")),
        NativeFunction::new(difftime),
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
    vm.stack_mut(window)[0] =
        (get_number_arg(vm, window.clone(), 1)? - get_number_arg(vm, window.clone(), 2)?).into();
    Ok(1)
}
