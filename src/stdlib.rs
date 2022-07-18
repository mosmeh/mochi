mod io;
mod math;
mod os;
mod string;
mod table;

use crate::{
    gc::{GcCell, GcHeap},
    types::{Integer, LuaString, NativeFunction, Number, StackWindow, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use bstr::{ByteSlice, B};
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
        heap.allocate_string(B("Lua 5.4")),
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

fn get_string_arg<'a, 'gc: 'a>(
    vm: &'a Vm<'gc>,
    window: StackWindow,
    nth: usize,
) -> Result<LuaString<'gc>, ErrorKind> {
    let arg = &vm.stack(window)[nth];
    arg.as_lua_string(vm.heap())
        .ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth,
            expected_type: Type::String,
            got_type: arg.ty(),
        })
}

fn get_integer_arg(vm: &Vm, window: StackWindow, nth: usize) -> Result<Integer, ErrorKind> {
    let arg = &vm.stack(window)[nth];
    arg.as_integer()
        .ok_or_else(|| ErrorKind::ArgumentTypeError {
            nth,
            expected_type: Type::Number,
            got_type: arg.ty(),
        })
}

fn get_number_arg(vm: &Vm, window: StackWindow, nth: usize) -> Result<Number, ErrorKind> {
    let arg = &vm.stack(window)[nth];
    arg.as_number().ok_or_else(|| ErrorKind::ArgumentTypeError {
        nth,
        expected_type: Type::Number,
        got_type: arg.ty(),
    })
}

fn error_obj_to_error_kind<'gc>(heap: &'gc GcHeap, error_obj: Value<'gc>) -> ErrorKind {
    let msg = if let Some(lua_str) = error_obj.as_lua_string(heap) {
        String::from_utf8_lossy(&lua_str).to_string()
    } else {
        format!("(error object is a {} value)", error_obj.ty())
    };
    ErrorKind::ExplicitError(msg)
}

fn assert(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack(window.clone());
    if stack[1].as_boolean() {
        let stack = vm.stack_mut(window);
        stack.copy_within(1..stack.len(), 0);
        Ok(stack.len() - 1)
    } else if stack.len() > 2 {
        Err(error_obj_to_error_kind(vm.heap(), stack[2]))
    } else {
        Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
    }
}

fn collectgarbage(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    if vm.stack(window.clone()).len() != 2 {
        unimplemented!();
    }
    let opt = get_string_arg(vm, window.clone(), 1)?;
    vm.stack_mut(window)[0] = match opt.as_ref() {
        b"count" => ((vm.heap().total_bytes() as Number) / 1024.0).into(),
        _ => unimplemented!(),
    };
    Ok(1)
}

fn error(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let error_obj = vm.stack(window)[1];
    Err(error_obj_to_error_kind(vm.heap(), error_obj))
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
        let stack = vm.stack(window.clone());
        let i = get_integer_arg(vm, window.clone(), 2)? + 1;
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

        let stack = vm.stack_mut(window);
        if value == Value::Nil {
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
            write!(stdout, "{}\t", x)?;
        }
        writeln!(stdout, "{}", last)?;
    } else {
        writeln!(stdout)?;
    }
    Ok(0)
}

fn rawequal(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = (stack[1] == stack[2]).into();
    Ok(1)
}

fn setmetatable(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    {
        let stack = vm.stack(window.clone());
        let mut table =
            stack[1]
                .as_table_mut(vm.heap())
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
    let stack = vm.stack_mut(window);
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
    let string = vm.stack(window.clone())[1].to_string().into_bytes();
    vm.stack_mut(window)[0] = vm.heap().allocate_string(string).into();
    Ok(1)
}

fn ty(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let string = vm.stack(window.clone())[1].ty().to_string().into_bytes();
    vm.stack_mut(window)[0] = vm.heap().allocate_string(string).into();
    Ok(1)
}

fn require(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();

    let package_str = heap.allocate_string(B("package"));
    let package_table = vm.global_table().borrow().get_field(package_str);
    let package_table = package_table.as_table().unwrap();

    let loaded_str = heap.allocate_string(B("loaded"));
    let maybe_loaded_value = {
        let loaded_table = package_table.get_field(loaded_str);
        let loaded_table = loaded_table.as_table().unwrap();
        let module_name = get_string_arg(vm, window.clone(), 1)?;
        loaded_table.get_field(module_name)
    };

    let module_name = get_string_arg(vm, window.clone(), 1)?;
    let filename = format!("./{}.lua", module_name.as_bstr());
    let filename_value = heap.allocate_string(filename.clone().into_bytes()).into();

    let loaded_value = if maybe_loaded_value == Value::Nil {
        let mut closure = crate::load_file(heap, &filename).unwrap();
        assert!(closure.upvalues.is_empty());
        closure
            .upvalues
            .push(heap.allocate_cell(Value::Table(vm.global_table()).into()));

        let callee = heap.allocate(closure).into();
        let module_name = get_string_arg(vm, window.clone(), 1)?;
        let value = vm.execute_inner(callee, &[module_name.into(), filename_value])?;

        let loaded_table = package_table.get_field(loaded_str);
        let mut loaded_table = loaded_table.as_table_mut(heap).unwrap();
        loaded_table.set_field(module_name, value);

        value
    } else {
        maybe_loaded_value
    };

    let stack = vm.stack_mut(window);
    stack[0] = loaded_value;
    stack[1] = filename_value;
    Ok(2)
}
