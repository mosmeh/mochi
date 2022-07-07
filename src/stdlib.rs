use crate::{
    gc::{GcCell, GcHeap},
    types::{Integer, LuaString, NativeClosure, Number, StackKey, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use rand::Rng;
use rustc_hash::FxHashMap;
use std::{borrow::Cow, io::Write};

pub fn create_global_table(heap: &GcHeap) -> GcCell<Table> {
    let mut env = FxHashMap::default();
    env.insert(
        heap.allocate(LuaString::from("assert")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            if stack[1].as_boolean() {
                for (to, from) in (1..stack.len()).enumerate() {
                    stack[to] = stack[from];
                }
                Ok(stack.len() - 1)
            } else if stack.len() > 2 {
                Err(error_obj_to_error_kind(stack[2]))
            } else {
                Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
            }
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("error")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let error_obj = vm.local_stack(key)[1];
            Err(error_obj_to_error_kind(error_obj))
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("print")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack(key);
            let mut stdout = std::io::stdout().lock();
            if let Some((last, xs)) = stack[1..].split_last() {
                for x in xs {
                    write!(stdout, "{}\t", x)?;
                }
                writeln!(stdout, "{}", last)?;
            } else {
                writeln!(stdout)?;
            }
            Ok(0)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("rawequal")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = (stack[1] == stack[2]).into();
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("tonumber")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = match stack[1] {
                Value::Integer(x) => Value::Integer(x),
                Value::Number(x) => Value::Number(x),
                Value::String(x) => x
                    .as_str()
                    .ok()
                    .and_then(|s| s.parse().ok())
                    .map(Value::Number)
                    .unwrap_or(Value::Nil),
                _ => Value::Nil,
            };
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("tostring")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = heap.allocate(LuaString::from(stack[1].to_string())).into();
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("type")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = heap
                .allocate(LuaString::from(stack[1].ty().to_string()))
                .into();
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("_VERSION")).into(),
        heap.allocate(LuaString::from("Lua 5.4")).into(),
    );

    env.insert(
        heap.allocate(LuaString::from("require")).into(),
        heap.allocate(NativeClosure::new(move |heap, vm, key| {
            let package_str = Value::from(heap.allocate(LuaString::from("package")));
            let package_table = vm.global_table().borrow().get(package_str);
            let package_table = package_table.as_table().unwrap();

            let loaded_str = Value::from(heap.allocate(LuaString::from("loaded")));
            let maybe_loaded_value = {
                let loaded_table = package_table.get(loaded_str);
                let loaded_table = loaded_table.as_table().unwrap();
                let name = vm.local_stack(key.clone())[1];
                loaded_table.get(name)
            };

            let loaded_value = if maybe_loaded_value == Value::Nil {
                let name = get_string_arg(vm, key.clone(), 1)?;
                let filename = format!("{}.lua", name.as_str()?);
                let closure = crate::load_file(heap, &filename).unwrap();
                let value = vm.execute(heap, closure).unwrap();
                let loaded_table = package_table.get(loaded_str);
                let mut loaded_table = loaded_table.as_table_mut(heap).unwrap();
                let name = vm.local_stack(key.clone())[1];
                loaded_table.set(name, value);
                value
            } else {
                maybe_loaded_value
            };

            vm.local_stack_mut(key)[0] = loaded_value;
            Ok(1)
        }))
        .into(),
    );
    let mut package = FxHashMap::default();
    package.insert(
        heap.allocate(LuaString::from("loaded")).into(),
        heap.allocate_cell(Table::new()).into(),
    );
    env.insert(
        heap.allocate(LuaString::from("package")).into(),
        heap.allocate_cell(package.into()).into(),
    );

    let mut string = FxHashMap::default();
    string.insert(
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
    string.insert(
        heap.allocate(LuaString::from("len")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let len = string.len() as Integer;
            vm.local_stack_mut(key)[0] = len.into();
            Ok(1)
        }))
        .into(),
    );
    string.insert(
        heap.allocate(LuaString::from("lower")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let lower = heap.allocate(LuaString::from(string.to_ascii_lowercase()));
            vm.local_stack_mut(key)[0] = lower.into();
            Ok(1)
        }))
        .into(),
    );
    string.insert(
        heap.allocate(LuaString::from("reverse")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let mut string = get_string_arg(vm, key.clone(), 1)?.to_vec();
            string.reverse();
            vm.local_stack_mut(key)[0] = heap.allocate(LuaString::from(string)).into();
            Ok(1)
        }))
        .into(),
    );
    string.insert(
        heap.allocate(LuaString::from("upper")).into(),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let string = get_string_arg(vm, key.clone(), 1)?;
            let upper = heap.allocate(LuaString::from(string.to_ascii_uppercase()));
            vm.local_stack_mut(key)[0] = upper.into();
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("string")).into(),
        heap.allocate_cell(string.into()).into(),
    );

    let mut math = FxHashMap::default();
    math.insert(
        heap.allocate(LuaString::from("acos")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.acos().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("asin")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.asin().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
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
    math.insert(
        heap.allocate(LuaString::from("cos")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.cos().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("deg")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_degrees().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("exp")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.exp().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("huge")).into(),
        Number::INFINITY.into(),
    );
    math.insert(
        heap.allocate(LuaString::from("maxinteger")).into(),
        Integer::MAX.into(),
    );
    math.insert(
        heap.allocate(LuaString::from("mininteger")).into(),
        Integer::MIN.into(),
    );
    math.insert(
        heap.allocate(LuaString::from("pi")).into(),
        (std::f64::consts::PI as Number).into(),
    );
    math.insert(
        heap.allocate(LuaString::from("rad")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.to_radians().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
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
    math.insert(
        heap.allocate(LuaString::from("sin")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sin().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("sqrt")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.sqrt().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
        heap.allocate(LuaString::from("tan")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = get_number_arg(vm, key.clone(), 1)?.tan().into();
            Ok(1)
        }))
        .into(),
    );
    math.insert(
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
    env.insert(
        heap.allocate(LuaString::from("math")).into(),
        heap.allocate_cell(math.into()).into(),
    );

    let mut io = FxHashMap::default();
    io.insert(
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
    env.insert(
        heap.allocate(LuaString::from("io")).into(),
        heap.allocate_cell(io.into()).into(),
    );

    let mut os = FxHashMap::default();
    os.insert(
        heap.allocate(LuaString::from("clock")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] = cpu_time::ProcessTime::now()
                .as_duration()
                .as_secs_f64()
                .into();
            Ok(1)
        }))
        .into(),
    );
    os.insert(
        heap.allocate(LuaString::from("difftime")).into(),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            vm.local_stack_mut(key)[0] =
                (get_number_arg(vm, key.clone(), 1)? - get_number_arg(vm, key.clone(), 2)?).into();
            Ok(1)
        }))
        .into(),
    );
    env.insert(
        heap.allocate(LuaString::from("os")).into(),
        heap.allocate_cell(os.into()).into(),
    );

    let global = heap.allocate_cell(Table::from(env));
    global
        .borrow_mut(heap)
        .set(heap.allocate(LuaString::from("_G")), global);
    global
}

fn get_string_arg<'a>(
    vm: &'a Vm,
    key: StackKey,
    nth: usize,
) -> Result<Cow<'a, LuaString>, ErrorKind> {
    let arg = &vm.local_stack(key)[nth];
    arg.as_lua_string()
        .ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth,
            expected_type: Type::String,
            got_type: arg.ty(),
        })
}

fn get_number_arg(vm: &Vm, key: StackKey, nth: usize) -> Result<Number, ErrorKind> {
    let arg = &vm.local_stack(key)[nth];
    arg.as_number().ok_or_else(|| ErrorKind::ArgumentTypeError {
        nth,
        expected_type: Type::Number,
        got_type: arg.ty(),
    })
}

fn error_obj_to_error_kind(error_obj: Value) -> ErrorKind {
    let msg = if let Some(lua_str) = error_obj.as_lua_string() {
        String::from_utf8_lossy(&lua_str).to_string()
    } else {
        format!("(error object is a {} value)", error_obj.ty())
    };
    ErrorKind::ExplicitError(msg)
}
