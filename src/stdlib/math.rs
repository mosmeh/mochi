use super::{get_integer_arg, get_number_arg};
use crate::{
    gc::GcHeap,
    types::{Integer, NativeFunction, Number, StackWindow, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use bstr::B;
use rand::Rng;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("abs")), NativeFunction::new(abs));
    table.set_field(heap.allocate_string(B("acos")), NativeFunction::new(acos));
    table.set_field(heap.allocate_string(B("asin")), NativeFunction::new(asin));
    table.set_field(heap.allocate_string(B("floor")), NativeFunction::new(floor));
    table.set_field(heap.allocate_string(B("cos")), NativeFunction::new(cos));
    table.set_field(heap.allocate_string(B("deg")), NativeFunction::new(deg));
    table.set_field(heap.allocate_string(B("exp")), NativeFunction::new(exp));
    table.set_field(heap.allocate_string(B("huge")), Number::INFINITY);
    table.set_field(heap.allocate_string(B("log")), NativeFunction::new(log));
    table.set_field(heap.allocate_string(B("maxinteger")), Integer::MAX);
    table.set_field(heap.allocate_string(B("mininteger")), Integer::MIN);
    table.set_field(
        heap.allocate_string(B("pi")),
        std::f64::consts::PI as Number,
    );
    table.set_field(heap.allocate_string(B("rad")), NativeFunction::new(rad));
    table.set_field(
        heap.allocate_string(B("random")),
        NativeFunction::new(random),
    );
    table.set_field(heap.allocate_string(B("sin")), NativeFunction::new(sin));
    table.set_field(heap.allocate_string(B("sqrt")), NativeFunction::new(sqrt));
    table.set_field(heap.allocate_string(B("tan")), NativeFunction::new(tan));
    table.set_field(heap.allocate_string(B("type")), NativeFunction::new(ty));
    table
}

fn abs(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack[1] {
        Value::Integer(x) => x.abs().into(),
        Value::Number(x) => x.abs().into(),
        _ => {
            return Err(ErrorKind::ArgumentTypeError {
                nth: 1,
                expected_type: Type::Number,
                got_type: stack[1].ty(),
            })
        }
    };
    Ok(1)
}

fn acos(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.acos().into();
    Ok(1)
}

fn asin(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.asin().into();
    Ok(1)
}

fn floor(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack[1] {
        value @ Value::Integer(_) => value,
        value => {
            let x = value
                .to_number()
                .ok_or_else(|| ErrorKind::ArgumentTypeError {
                    nth: 1,
                    expected_type: Type::Number,
                    got_type: stack[1].ty(),
                })?;
            let float_floor = x.floor();
            let int_floor = float_floor as Integer;
            if float_floor == int_floor as Number {
                Value::Integer(int_floor)
            } else {
                Value::Number(float_floor)
            }
        }
    };
    Ok(1)
}

fn cos(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.cos().into();
    Ok(1)
}

fn deg(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.to_degrees().into();
    Ok(1)
}

fn exp(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.exp().into();
    Ok(1)
}

fn log(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    let x = get_number_arg(stack, 1)?;
    let result = if stack.len() > 2 {
        x.log(get_number_arg(stack, 2)?)
    } else {
        x.ln()
    };
    stack[0] = result.into();
    Ok(1)
}

fn rad(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.to_radians().into();
    Ok(1)
}

fn random(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let mut rng = rand::thread_rng();
    let stack = vm.stack_mut(window);
    stack[0] = match stack.len() {
        1 => rng.gen::<Number>().into(),
        2 => {
            let upper = get_integer_arg(stack, 1)?;
            rng.gen_range(1..=upper).into()
        }
        3 => {
            let lower = get_integer_arg(stack, 1)?;
            let upper = get_integer_arg(stack, 2)?;
            rng.gen_range(lower..=upper).into()
        }
        _ => {
            return Err(ErrorKind::ExplicitError(
                "wrong number of arguments".to_owned(),
            ))
        }
    };
    Ok(1)
}

fn sin(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.sin().into();
    Ok(1)
}

fn sqrt(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.sqrt().into();
    Ok(1)
}

fn tan(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.tan().into();
    Ok(1)
}

fn ty(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let result = match stack[1] {
        Value::Integer(_) => heap.allocate_string(B("integer")).into(),
        Value::Number(_) => heap.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    stack[0] = result;
    Ok(1)
}
