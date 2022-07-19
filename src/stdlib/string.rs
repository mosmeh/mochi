use super::get_string_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, NativeFunction, StackWindow, Table, Type},
    vm::{ErrorKind, Vm},
};
use bstr::B;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("char")), NativeFunction::new(char));
    table.set_field(heap.allocate_string(B("len")), NativeFunction::new(len));
    table.set_field(heap.allocate_string(B("lower")), NativeFunction::new(lower));
    table.set_field(
        heap.allocate_string(B("reverse")),
        NativeFunction::new(reverse),
    );
    table.set_field(heap.allocate_string(B("upper")), NativeFunction::new(upper));
    table
}

fn char(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let mut bytes = Vec::with_capacity(window.0.len() - 1);
    let stack = vm.stack_mut(window);
    for (i, x) in stack[1..].iter().enumerate() {
        let n = x.to_integer().ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth: i + 1,
            expected_type: Type::Number,
            got_type: x.ty(),
        })?;
        if let Ok(n) = n.try_into() {
            bytes.push(n);
        } else {
            return Err(ErrorKind::ArgumentError {
                nth: i + 1,
                message: "value out of range",
            });
        }
    }
    stack[0] = heap.allocate_string(bytes).into();
    Ok(1)
}

fn len(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    let string = get_string_arg(stack, 1)?;
    let len = string.len() as Integer;
    stack[0] = len.into();
    Ok(1)
}

fn lower(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let string = get_string_arg(stack, 1)?;
    let lower = heap.allocate_string(string.to_ascii_lowercase());
    stack[0] = lower.into();
    Ok(1)
}

fn reverse(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let mut string = get_string_arg(stack, 1)?.to_vec();
    string.reverse();
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn upper(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let string = get_string_arg(stack, 1)?;
    let upper = heap.allocate_string(string.to_ascii_uppercase());
    stack[0] = upper.into();
    Ok(1)
}
