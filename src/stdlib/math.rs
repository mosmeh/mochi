use super::get_number_arg;
use crate::{
    gc::GcHeap,
    types::{Integer, LuaString, NativeClosure, Number, Table, Type, Value},
    vm::ErrorKind,
};
use rand::Rng;
use rustc_hash::FxHashMap;

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = FxHashMap::default();

    table.insert(
        heap.allocate(LuaString::from("abs")).into(),
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
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("acos")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.acos().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("asin")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.asin().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("floor")).into(),
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
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("cos")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.cos().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("deg")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_degrees().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("exp")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.exp().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("huge")).into(),
        Number::INFINITY.into(),
    );

    table.insert(
        heap.allocate(LuaString::from("log")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let x = get_number_arg(vm, key.clone(), 1)?;
            let result = if vm.local_stack(key.clone()).len() > 2 {
                x.log(get_number_arg(vm, key.clone(), 2)?)
            } else {
                x.ln()
            };
            vm.local_stack_mut(key)[0] = result.into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("maxinteger")).into(),
        Integer::MAX.into(),
    );

    table.insert(
        heap.allocate(LuaString::from("mininteger")).into(),
        Integer::MIN.into(),
    );

    table.insert(
        heap.allocate(LuaString::from("pi")).into(),
        (std::f64::consts::PI as Number).into(),
    );

    table.insert(
        heap.allocate(LuaString::from("rad")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_radians().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("random")).into(),
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
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("sin")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sin().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("sqrt")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sqrt().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("tan")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.tan().into();
            Ok(1)
        }))
        .into(),
    );

    table.insert(
        heap.allocate(LuaString::from("type")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = match stack[1] {
                Value::Integer(_) => heap.allocate(LuaString::from("integer")).into(),
                Value::Number(_) => heap.allocate(LuaString::from("float")).into(),
                _ => Value::Nil,
            };
            Ok(1)
        }))
        .into(),
    );

    table.into()
}
