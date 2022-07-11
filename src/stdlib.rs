mod io;
mod math;
mod os;
mod string;

use crate::{
    gc::{GcCell, GcHeap},
    types::{LuaString, NativeClosure, Number, StackKey, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use std::{borrow::Cow, io::Write};

pub fn create_global_table(heap: &GcHeap) -> GcCell<Table> {
    let mut table = Table::new();
    table.set(
        heap.allocate(LuaString::from("assert")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            if stack[1].as_boolean() {
                stack.copy_within(1..stack.len(), 0);
                Ok(stack.len() - 1)
            } else if stack.len() > 2 {
                Err(error_obj_to_error_kind(stack[2]))
            } else {
                Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
            }
        })),
    );
    table.set(
        heap.allocate(LuaString::from("error")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let error_obj = vm.local_stack(key)[1];
            Err(error_obj_to_error_kind(error_obj))
        })),
    );
    table.set(
        heap.allocate(LuaString::from("print")),
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
        })),
    );
    table.set(
        heap.allocate(LuaString::from("rawequal")),
        heap.allocate(NativeClosure::new(|_, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = (stack[1] == stack[2]).into();
            Ok(1)
        })),
    );
    table.set(
        heap.allocate(LuaString::from("tonumber")),
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
        })),
    );
    table.set(
        heap.allocate(LuaString::from("tostring")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = heap.allocate(LuaString::from(stack[1].to_string())).into();
            Ok(1)
        })),
    );
    table.set(
        heap.allocate(LuaString::from("type")),
        heap.allocate(NativeClosure::new(|heap, vm, key| {
            let stack = vm.local_stack_mut(key);
            stack[0] = heap
                .allocate(LuaString::from(stack[1].ty().to_string()))
                .into();
            Ok(1)
        })),
    );
    table.set(
        heap.allocate(LuaString::from("_VERSION")),
        heap.allocate(LuaString::from("Lua 5.4")),
    );

    table.set(
        heap.allocate(LuaString::from("require")),
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

            let name = get_string_arg(vm, key.clone(), 1)?;
            let filename = format!("./{}.lua", name.as_str()?);
            let loaded_value = if maybe_loaded_value == Value::Nil {
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

            let stack = vm.local_stack_mut(key);
            stack[0] = loaded_value;
            stack[1] = heap.allocate(LuaString::from(filename)).into();
            Ok(2)
        })),
    );
    let mut package = Table::new();
    package.set(
        heap.allocate(LuaString::from("loaded")),
        heap.allocate_cell(Table::new()),
    );
    table.set(
        heap.allocate(LuaString::from("package")),
        heap.allocate_cell(package),
    );

    table.set(
        heap.allocate(LuaString::from("string")),
        heap.allocate_cell(string::create_table(heap)),
    );
    table.set(
        heap.allocate(LuaString::from("math")),
        heap.allocate_cell(math::create_table(heap)),
    );
    table.set(
        heap.allocate(LuaString::from("io")),
        heap.allocate_cell(io::create_table(heap)),
    );
    table.set(
        heap.allocate(LuaString::from("os")),
        heap.allocate_cell(os::create_table(heap)),
    );

    let global = heap.allocate_cell(table);
    global
        .borrow_mut(heap)
        .set(heap.allocate(LuaString::from("_G")), global);
    global
}

fn get_string_arg<'gc>(
    vm: &'gc Vm,
    key: StackKey,
    nth: usize,
) -> Result<Cow<'gc, LuaString>, ErrorKind> {
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
