mod io;
mod math;
mod os;
mod string;
mod table;

use crate::{
    gc::{GcCell, GcHeap},
    types::{Integer, NativeFunction, Number, StackWindow, Table, Type, Value},
    vm::{ErrorKind, Vm},
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use std::{borrow::Cow, io::Write};

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

fn get_string_arg<'a, 'gc>(
    stack: &'a [Value<'gc>],
    nth: usize,
) -> Result<Cow<'a, [u8]>, ErrorKind> {
    let arg = &stack[nth];
    arg.to_string().ok_or_else(|| ErrorKind::ArgumentTypeError {
        nth,
        expected_type: Type::String,
        got_type: arg.ty(),
    })
}

fn get_integer_arg(stack: &[Value], nth: usize) -> Result<Integer, ErrorKind> {
    let arg = stack[nth];
    arg.to_integer()
        .ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth,
            expected_type: Type::Number,
            got_type: arg.ty(),
        })
}

fn get_number_arg(stack: &[Value], nth: usize) -> Result<Number, ErrorKind> {
    let arg = stack[nth];
    arg.to_number().ok_or_else(|| ErrorKind::ArgumentTypeError {
        nth,
        expected_type: Type::Number,
        got_type: arg.ty(),
    })
}

fn error_obj_to_error_kind(error_obj: Value) -> ErrorKind {
    let msg = if let Some(s) = error_obj.to_string() {
        String::from_utf8_lossy(&s).to_string()
    } else {
        format!(
            "(error object is a {} value",
            error_obj.ty().display_bytes().as_bstr()
        )
    };
    ErrorKind::ExplicitError(msg)
}

fn assert(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    if stack[1].to_boolean() {
        stack.copy_within(1..stack.len(), 0);
        Ok(stack.len() - 1)
    } else if stack.len() > 2 {
        Err(error_obj_to_error_kind(stack[2]))
    } else {
        Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
    }
}

fn collectgarbage(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    if stack.len() != 2 {
        unimplemented!();
    }
    let opt = get_string_arg(stack, 1)?;
    stack[0] = match opt.as_ref() {
        b"count" => ((heap.total_bytes() as Number) / 1024.0).into(),
        _ => unimplemented!(),
    };
    Ok(1)
}

fn error(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let error_obj = vm.stack(window)[1];
    Err(error_obj_to_error_kind(error_obj))
}

fn getmetatable(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = stack[1]
        .as_table()
        .and_then(|table| table.metatable())
        .map(Value::from)
        .unwrap_or_default();
    Ok(1)
}

fn ipairs(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    fn iterate(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
        let stack = vm.stack_mut(window);
        let i = get_integer_arg(stack, 2)? + 1;
        let value = {
            let table = stack[1]
                .as_table()
                .ok_or_else(|| ErrorKind::ArgumentTypeError {
                    nth: 1,
                    expected_type: Type::Table,
                    got_type: stack[1].ty(),
                })?;
            table.get(i)
        };

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
    if let Some((last, xs)) = stack[1..].split_last() {
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
    stack[0] = (stack[1] == stack[2]).into();
    Ok(1)
}

fn setmetatable(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    {
        let mut table =
            stack[1]
                .as_table_mut(heap)
                .ok_or_else(|| ErrorKind::ArgumentTypeError {
                    nth: 1,
                    expected_type: Type::Table,
                    got_type: stack[1].ty(),
                })?;
        let metatable = match stack[2] {
            Value::Table(table) => Some(table),
            Value::Nil => None,
            _ => {
                return Err(ErrorKind::ArgumentTypeError {
                    nth: 2,
                    expected_type: Type::Table,
                    got_type: stack[2].ty(),
                })
            }
        };
        table.set_metatable(metatable);
    }
    stack[0] = stack[1];
    Ok(1)
}

fn tonumber(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack[1] {
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
    stack[1].fmt_bytes(&mut string)?;
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn ty(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let string = stack[1].ty().display_bytes();
    stack[0] = heap.allocate_string(string).into();
    Ok(1)
}

fn require(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window.clone());

    let module_name = get_string_arg(stack, 1)?;
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
