use super::StackExt;
use crate::{
    gc::GcHeap,
    types::{Integer, NativeFunction, StackWindow, Table},
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
    let stack = vm.stack_mut(window);
    let mut bytes = Vec::with_capacity(stack.args().len());
    for nth in 0..bytes.len() {
        let n = stack.arg(nth).to_integer()?;
        if let Ok(n) = n.try_into() {
            bytes.push(n);
        } else {
            return Err(ErrorKind::ArgumentError {
                nth,
                message: "value out of range",
            });
        }
    }
    stack[0] = heap.allocate_string(bytes).into();
    Ok(1)
}

fn len(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    let len = stack.arg(0).to_string()?.len() as Integer;
    stack[0] = len.into();
    Ok(1)
}

fn lower(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let lower = stack.arg(0).to_string()?.to_ascii_lowercase();
    let lower = heap.allocate_string(lower);
    stack[0] = lower.into();
    Ok(1)
}

fn reverse(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let mut string = stack.arg(0).to_string()?.to_vec();
    string.reverse();
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn upper(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let upper = stack.arg(0).to_string()?.to_ascii_uppercase();
    let upper = heap.allocate_string(upper);
    stack[0] = upper.into();
    Ok(1)
}
