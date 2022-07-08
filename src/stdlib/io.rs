use crate::{
    gc::GcHeap,
    types::{LuaString, NativeClosure, Table},
};
use rustc_hash::FxHashMap;
use std::io::Write;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = FxHashMap::default();

    table.insert(
        heap.allocate(LuaString::from("write")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack(key);
            let mut stdout = std::io::stdout().lock();
            for x in &stack[1..] {
                write!(stdout, "{}", x)?;
            }
            Ok(0)
        }))
        .into(),
    );

    table.into()
}
