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
    let mut bytes = Vec::with_capacity(window.0.len() - 1);
    for (i, x) in vm.stack(window.clone())[1..].iter().enumerate() {
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
    vm.stack_mut(window)[0] = vm.heap().allocate_string(bytes).into();
    Ok(1)
}

fn len(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let string = get_string_arg(vm, window.clone(), 1)?;
    let len = string.len() as Integer;
    vm.stack_mut(window)[0] = len.into();
    Ok(1)
}

fn lower(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let string = get_string_arg(vm, window.clone(), 1)?;
    let lower = vm.heap().allocate_string(string.to_ascii_lowercase());
    vm.stack_mut(window)[0] = lower.into();
    Ok(1)
}

fn reverse(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let mut string = get_string_arg(vm, window.clone(), 1)?.to_vec();
    string.reverse();
    vm.stack_mut(window)[0] = vm.heap().allocate_string(string).into();
    Ok(1)
}

fn upper(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let string = get_string_arg(vm, window.clone(), 1)?;
    let upper = vm.heap().allocate_string(string.to_ascii_uppercase());
    vm.stack_mut(window)[0] = upper.into();
    Ok(1)
}
