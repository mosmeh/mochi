use super::get_number_arg;
use crate::{
    gc::GcHeap,
    types::{NativeClosure, Table},
};
use bstr::B;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();

    table.set(
        heap.allocate_string(B("clock")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = cpu_time::ProcessTime::now()
                .as_duration()
                .as_secs_f64()
                .into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate_string(B("difftime")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] =
                (get_number_arg(vm, key.clone(), 1)? - get_number_arg(vm, key.clone(), 2)?).into();
            Ok(1)
        })),
    );

    table
}
