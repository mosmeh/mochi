use super::get_string_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, NativeClosure, Table, Type},
    vm::ErrorKind,
};
use bstr::B;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();

    table.set(
        heap.allocate_string(B("char")),
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
            stack[0] = heap.allocate_string(bytes).into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate_string(B("len")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(heap, vm, key.clone(), 1)?;
            let len = string.len() as Integer;
            vm.local_stack_mut(key)[0] = len.into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate_string(B("lower")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(heap, vm, key.clone(), 1)?;
            let lower = heap.allocate_string(string.to_ascii_lowercase());
            vm.local_stack_mut(key)[0] = lower.into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate_string(B("reverse")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let mut string = get_string_arg(heap, vm, key.clone(), 1)?.to_vec();
            string.reverse();
            vm.local_stack_mut(key)[0] = heap.allocate_string(string).into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate_string(B("upper")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(heap, vm, key.clone(), 1)?;
            let upper = heap.allocate_string(string.to_ascii_uppercase());
            vm.local_stack_mut(key)[0] = upper.into();
            Ok(1)
        })),
    );

    table
}
