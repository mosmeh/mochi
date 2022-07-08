use super::get_string_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, LuaString, NativeClosure, Table, Type},
    vm::ErrorKind,
};
use rustc_hash::FxHashMap;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = FxHashMap::default();

    table.insert(
        heap.allocate(LuaString::from("char")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let mut bytes = Vec::with_capacity(key.0.len() - 1);
            let stack = vm.local_stack_mut(key);
            for (i, x) in stack[1..].iter().enumerate() {
                let n = x.as_integer().ok_or_else(|| ErrorKind::ArgumentTypeError {
                    nth: i + 1,
                    expected_type: Type::Number,
                    got_type: x.ty(),
                })?;
                if n <= u8::MAX as Integer {
                    bytes.push(n as u8)
                } else {
                    return Err(ErrorKind::ArgumentError {
                        nth: i + 1,
                        message: "value out of range",
                    });
                }
            }
            stack[0] = heap.allocate(LuaString::from(bytes)).into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("len")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let len = string.len() as Integer;
            vm.local_stack_mut(key)[0] = len.into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("lower")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let lower = heap.allocate(LuaString::from(string.to_ascii_lowercase()));
            vm.local_stack_mut(key)[0] = lower.into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("reverse")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let mut string = get_string_arg(vm, key.clone(), 1)?.to_vec();
            string.reverse();
            vm.local_stack_mut(key)[0] = heap.allocate(LuaString::from(string)).into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("upper")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let upper = heap.allocate(LuaString::from(string.to_ascii_uppercase()));
            vm.local_stack_mut(key)[0] = upper.into();
            Ok(1)
        }))
        .into(),
    );

    table.into()
}
