use super::get_number_arg;
use crate::{
    gc::GcHeap,
    types::{LuaString, NativeClosure, Table},
};
use rustc_hash::FxHashMap;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = FxHashMap::default();

    table.insert(
        heap.allocate(LuaString::from("clock")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = cpu_time::ProcessTime::now()
                .as_duration()
                .as_secs_f64()
                .into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("difftime")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] =
                (get_number_arg(vm, key.clone(), 1)? - get_number_arg(vm, key.clone(), 2)?).into();
            Ok(1)
        }))
        .into(),
    );

    table.into()
}
