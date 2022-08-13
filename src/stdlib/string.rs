use super::StackExt;
use crate::{
    binary_chunk,
    gc::GcHeap,
    types::{Integer, NativeFunction, StackWindow, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use bstr::B;
use std::ops::Range;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("byte")), NativeFunction::new(byte));
    table.set_field(heap.allocate_string(B("char")), NativeFunction::new(char));
    table.set_field(heap.allocate_string(B("dump")), NativeFunction::new(dump));
    table.set_field(heap.allocate_string(B("len")), NativeFunction::new(len));
    table.set_field(heap.allocate_string(B("lower")), NativeFunction::new(lower));
    table.set_field(heap.allocate_string(B("sub")), NativeFunction::new(sub));
    table.set_field(heap.allocate_string(B("rep")), NativeFunction::new(rep));
    table.set_field(
        heap.allocate_string(B("reverse")),
        NativeFunction::new(reverse),
    );
    table.set_field(heap.allocate_string(B("upper")), NativeFunction::new(upper));
    table
}

fn byte(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window.clone());

    let s = stack.arg(0);
    let s = s.to_string()?;

    let i = stack.arg(1).to_integer_or(1)?;
    let j = stack.arg(2).to_integer_or(i)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    let num_results = range.len();
    let window = vm.ensure_stack(window, num_results);
    let stack = vm.stack_mut(window);
    for (x, b) in stack.iter_mut().zip(s[range].iter()) {
        *x = (*b as Integer).into();
    }
    Ok(num_results)
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

fn dump(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    match stack.arg(0).get() {
        Some(Value::LuaClosure(closure)) => {
            let mut binary = Vec::new();
            binary_chunk::dump(&mut binary, &closure.proto)?;
            stack[0] = heap.allocate_string(binary).into();
            Ok(1)
        }
        Some(value) if value.ty() == Type::Function => Err(ErrorKind::ExplicitError(
            "unable to dump given function".to_owned(),
        )),
        value => Err(ErrorKind::ArgumentTypeError {
            nth: 0,
            expected_type: "function",
            got_type: value.map(|value| value.ty().name()),
        }),
    }
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

fn sub(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);

    let s = stack.arg(0);
    let s = s.to_string()?;

    let i = stack.arg(1).to_integer()?;
    let j = stack.arg(2).to_integer_or(-1)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    stack[0] = heap.allocate_string(&s[range]).into();
    Ok(1)
}

fn rep(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);

    let s = stack.arg(0);
    let s = s.to_string()?;
    let n = stack.arg(1).to_integer()?;
    let sep = stack.arg(2);
    let sep = sep.to_string_or(b"".to_vec())?;

    let count = n.max(0) as usize;
    let string = if count > 0 {
        let mut string = Vec::with_capacity(count * s.len() + (count - 1) * sep.len());
        for _ in 0..count - 1 {
            string.extend_from_slice(s.as_ref());
            string.extend_from_slice(sep.as_ref());
        }
        string.extend_from_slice(s.as_ref());
        string
    } else {
        Vec::new()
    };

    stack[0] = heap.allocate_string(string).into();
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

fn indices_to_range(i: Integer, j: Integer, len: Integer) -> Range<usize> {
    let start = match i {
        0 => 0,
        _ if i < -len => 0,
        1.. => i - 1,
        _ => len + i,
    } as usize;
    let end = match j {
        0.. => j,
        _ if j < -len => 0,
        _ if j > len => len,
        _ => len + j + 1,
    } as usize;
    start..end
}
