use super::get_number_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, LuaString, NativeClosure, Number, Table, Type, Value},
    vm::ErrorKind,
};
use rand::Rng;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();

    table.set(
        heap.allocate(LuaString::from("abs")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
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
        })),
    );

    table.set(
        heap.allocate(LuaString::from("acos")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.acos().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("asin")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.asin().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("floor")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = match stack[1] {
                value @ Value::Integer(_) => value,
                value => {
                    let x = value
                        .as_number()
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
        })),
    );

    table.set(
        heap.allocate(LuaString::from("cos")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.cos().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("deg")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_degrees().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("exp")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.exp().into();
            Ok(1)
        })),
    );

    table.set(heap.allocate(LuaString::from("huge")), Number::INFINITY);

    table.set(
        heap.allocate(LuaString::from("log")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let x = get_number_arg(vm, key.clone(), 1)?;
            let result = if vm.local_stack(key.clone()).len() > 2 {
                x.log(get_number_arg(vm, key.clone(), 2)?)
            } else {
                x.ln()
            };
            vm.local_stack_mut(key)[0] = result.into();
            Ok(1)
        })),
    );

    table.set(heap.allocate(LuaString::from("maxinteger")), Integer::MAX);
    table.set(heap.allocate(LuaString::from("mininteger")), Integer::MIN);

    table.set(
        heap.allocate(LuaString::from("pi")),
        std::f64::consts::PI as Number,
    );

    table.set(
        heap.allocate(LuaString::from("rad")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_radians().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("random")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let mut rng = rand::thread_rng();
            let stack = vm.local_stack_mut(key);
            stack[0] = match stack[1..] {
                [] => rng.gen::<Number>().into(),
                [m] => {
                    let upper = m.as_integer().ok_or_else(|| ErrorKind::ArgumentTypeError {
                        nth: 1,
                        expected_type: Type::Number,
                        got_type: m.ty(),
                    })?;
                    rng.gen_range(1..=upper).into()
                }
                [m, n] => {
                    let lower = m.as_integer().ok_or_else(|| ErrorKind::ArgumentTypeError {
                        nth: 1,
                        expected_type: Type::Number,
                        got_type: m.ty(),
                    })?;
                    let upper = n.as_integer().ok_or_else(|| ErrorKind::ArgumentTypeError {
                        nth: 2,
                        expected_type: Type::Number,
                        got_type: n.ty(),
                    })?;
                    rng.gen_range(lower..=upper).into()
                }
                _ => {
                    return Err(ErrorKind::ExplicitError(
                        "wrong number of arguments".to_owned(),
                    ))
                }
            };
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("sin")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sin().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("sqrt")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sqrt().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("tan")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.tan().into();
            Ok(1)
        })),
    );

    table.set(
        heap.allocate(LuaString::from("type")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = match stack[1] {
                Value::Integer(_) => heap.allocate(LuaString::from("integer")).into(),
                Value::Number(_) => heap.allocate(LuaString::from("float")).into(),
                _ => Value::Nil,
            };
            Ok(1)
        })),
    );

    table
}
