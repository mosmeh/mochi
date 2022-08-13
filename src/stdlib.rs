mod helpers;
mod io;
mod math;
mod os;
mod string;
mod table;

use crate::{
    gc::{GcCell, GcHeap},
    types::{NativeFunction, Number, StackWindow, Table, Value},
    vm::{ErrorKind, Vm},
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use helpers::StackExt;
use std::io::Write;

pub fn create_global_table(heap: &GcHeap) -> GcCell<Table> {
    let mut table = Table::new();

    // base
    table.set_field(
        heap.allocate_string(B("assert")),
        NativeFunction::new(assert),
    );
    table.set_field(
        heap.allocate_string(B("collectgarbage")),
        NativeFunction::new(collectgarbage),
    );
    table.set_field(heap.allocate_string(B("error")), NativeFunction::new(error));
    table.set_field(
        heap.allocate_string(B("getmetatable")),
        NativeFunction::new(getmetatable),
    );
    table.set_field(
        heap.allocate_string(B("ipairs")),
        NativeFunction::new(ipairs),
    );
    table.set_field(heap.allocate_string(B("print")), NativeFunction::new(print));
    table.set_field(
        heap.allocate_string(B("rawequal")),
        NativeFunction::new(rawequal),
    );
    table.set_field(
        heap.allocate_string(B("setmetatable")),
        NativeFunction::new(setmetatable),
    );
    table.set_field(
        heap.allocate_string(B("tonumber")),
        NativeFunction::new(tonumber),
    );
    table.set_field(
        heap.allocate_string(B("tostring")),
        NativeFunction::new(tostring),
    );
    table.set_field(heap.allocate_string(B("type")), NativeFunction::new(ty));
    table.set_field(
        heap.allocate_string(B("_VERSION")),
        heap.allocate_string(format!("Lua {}.{}", LUA_VERSION.0, LUA_VERSION.1).into_bytes()),
    );

    // package
    table.set_field(
        heap.allocate_string(B("require")),
        NativeFunction::new(require),
    );
    let mut package = Table::new();
    package.set_field(
        heap.allocate_string(B("loaded")),
        heap.allocate_cell(Table::new()),
    );
    table.set_field(
        heap.allocate_string(B("package")),
        heap.allocate_cell(package),
    );

    // others
    table.set_field(
        heap.allocate_string(B("string")),
        heap.allocate_cell(string::create_table(heap)),
    );
    table.set_field(
        heap.allocate_string(B("table")),
        heap.allocate_cell(table::create_table(heap)),
    );
    table.set_field(
        heap.allocate_string(B("math")),
        heap.allocate_cell(math::create_table(heap)),
    );
    table.set_field(
        heap.allocate_string(B("io")),
        heap.allocate_cell(io::create_table(heap)),
    );
    table.set_field(
        heap.allocate_string(B("os")),
        heap.allocate_cell(os::create_table(heap)),
    );

    let global = heap.allocate_cell(table);
    global
        .borrow_mut(heap)
        .set_field(heap.allocate_string(B("_G")), global);
    global
}

fn error_obj_to_error_kind(error_obj: Value) -> ErrorKind {
    let msg = if let Some(s) = error_obj.to_string() {
        String::from_utf8_lossy(&s).to_string()
    } else {
        format!("(error object is a {} value", error_obj.ty().name())
    };
    ErrorKind::ExplicitError(msg)
}

fn assert(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    if stack.arg(0).to_value()?.to_boolean() {
        stack.copy_within(1..stack.len(), 0);
        Ok(stack.args().len())
    } else if let Some(error_obj) = stack.arg(1).get() {
        Err(error_obj_to_error_kind(error_obj))
    } else {
        Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
    }
}

fn collectgarbage(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    stack[0] = match stack.arg(0).to_string()?.as_ref() {
        b"count" => ((heap.total_bytes() as Number) / 1024.0).into(),
        opt => todo!("{}", opt.as_bstr()),
    };
    Ok(1)
}

fn error(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window);
    let error_obj = stack.arg(0).get().unwrap_or_default();
    Err(error_obj_to_error_kind(error_obj))
}

fn getmetatable(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = if let Value::Table(table) = stack.arg(0).to_value()? {
        table
            .borrow()
            .metatable()
            .map(Value::from)
            .unwrap_or_default()
    } else {
        Value::Nil
    };
    Ok(1)
}

fn ipairs(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    fn iterate(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
        let stack = vm.stack_mut(window);
        let i = stack.arg(1).to_integer()? + 1;
        let value = stack.arg(0).as_table()?.get(i);

        if value.is_nil() {
            stack[0] = Value::Nil;
            Ok(1)
        } else {
            stack[0] = i.into();
            stack[1] = value;
            Ok(2)
        }
    }

    let window = vm.ensure_stack(window, 3);
    let stack = vm.stack_mut(window);
    stack[0] = NativeFunction::new(iterate).into();
    stack[2] = 0.into();
    Ok(3)
}

fn print(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window);
    let mut stdout = std::io::stdout().lock();
    if let Some((last, xs)) = stack.args().split_last() {
        for x in xs {
            x.fmt_bytes(&mut stdout)?;
            stdout.write_all(b"\t")?;
        }
        last.fmt_bytes(&mut stdout)?;
    }
    stdout.write_all(b"\n")?;
    Ok(0)
}

fn rawequal(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = (stack.arg(0).to_value()? == stack.arg(1).to_value()?).into();
    Ok(1)
}

fn setmetatable(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let table = {
        let table_arg = stack.arg(0);
        let mut table = table_arg.as_table_mut(heap)?;
        let metatable = match stack.arg(1).get() {
            Some(Value::Nil) => None,
            Some(Value::Table(table)) => Some(table),
            value => {
                return Err(ErrorKind::ArgumentTypeError {
                    nth: 1,
                    expected_type: "nil or table",
                    got_type: value.map(|value| value.ty().name()),
                })
            }
        };
        table.set_metatable(metatable);
        table_arg.to_value()?
    };
    stack[0] = table;
    Ok(1)
}

fn tonumber(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack.arg(0).to_value()? {
        Value::Integer(x) => Value::Integer(x),
        Value::Number(x) => Value::Number(x),
        Value::String(x) => x
            .as_str()
            .ok()
            .map(|s| {
                if let Ok(i) = s.parse() {
                    Value::Integer(i)
                } else if let Ok(f) = s.parse() {
                    Value::Number(f)
                } else {
                    Value::Nil
                }
            })
            .unwrap_or(Value::Nil),
        _ => Value::Nil,
    };
    Ok(1)
}

fn tostring(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let mut string = Vec::new();
    stack.arg(0).to_value()?.fmt_bytes(&mut string)?;
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn ty(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let string = stack.arg(0).to_value()?.ty().name().as_bytes();
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn require(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window.clone());

    let module_name = stack.arg(0);
    let module_name = module_name.to_string()?;
    let module_name = heap.allocate_string(module_name);

    let package_str = heap.allocate_string(B("package"));
    let package_table = vm.global_table().borrow().get_field(package_str);
    let package_table = package_table.as_table().unwrap();

    let loaded_str = heap.allocate_string(B("loaded"));
    let loaded_table = package_table.get_field(loaded_str);
    let maybe_loaded_value = loaded_table.as_table().unwrap().get_field(module_name);

    let filename = format!("./{}.lua", module_name.as_bstr());
    let filename_value = heap.allocate_string(filename.clone().into_bytes()).into();

    let loaded_value = if maybe_loaded_value.is_nil() {
        let mut closure = crate::load_file(heap, &filename)
            .unwrap()
            .into_lua_closure(heap);
        closure
            .upvalues
            .push(heap.allocate_cell(Value::Table(vm.global_table()).into()));

        let callee = heap.allocate(closure).into();
        let value = vm.execute_inner(callee, &[module_name.into(), filename_value])?;

        loaded_table
            .as_table_mut(heap)
            .unwrap()
            .set_field(module_name, value);

        value
    } else {
        maybe_loaded_value
    };

    let stack = vm.stack_mut(window);
    stack[0] = loaded_value;
    stack[1] = filename_value;
    Ok(2)
}
