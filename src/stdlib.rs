mod helpers;
mod io;
mod math;
mod os;
mod string;
mod table;

use crate::{
    gc::{root_gc, GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Integer, LuaClosure, LuaThread, NativeFunction, Number, StackWindow, Table, Value},
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use helpers::StackExt;
use std::io::Write;

pub fn load<'gc>(gc: &'gc GcContext, global_table: GcCell<'gc, Table<'gc>>) {
    let mut table = global_table.borrow_mut(gc);
    table.set_field(gc.allocate_string(B("_G")), global_table);

    // base
    table.set_field(gc.allocate_string(B("assert")), NativeFunction::new(assert));
    table.set_field(
        gc.allocate_string(B("collectgarbage")),
        NativeFunction::new(collectgarbage),
    );
    table.set_field(gc.allocate_string(B("error")), NativeFunction::new(error));
    table.set_field(
        gc.allocate_string(B("getmetatable")),
        NativeFunction::new(getmetatable),
    );
    table.set_field(gc.allocate_string(B("ipairs")), NativeFunction::new(ipairs));
    table.set_field(gc.allocate_string(B("print")), NativeFunction::new(print));
    table.set_field(
        gc.allocate_string(B("rawequal")),
        NativeFunction::new(rawequal),
    );
    table.set_field(gc.allocate_string(B("rawget")), NativeFunction::new(rawget));
    table.set_field(gc.allocate_string(B("rawlen")), NativeFunction::new(rawlen));
    table.set_field(gc.allocate_string(B("rawset")), NativeFunction::new(rawset));
    table.set_field(gc.allocate_string(B("select")), NativeFunction::new(select));
    table.set_field(
        gc.allocate_string(B("setmetatable")),
        NativeFunction::new(setmetatable),
    );
    table.set_field(
        gc.allocate_string(B("tonumber")),
        NativeFunction::new(tonumber),
    );
    table.set_field(
        gc.allocate_string(B("tostring")),
        NativeFunction::new(tostring),
    );
    table.set_field(gc.allocate_string(B("type")), NativeFunction::new(ty));
    table.set_field(
        gc.allocate_string(B("_VERSION")),
        gc.allocate_string(format!("Lua {}.{}", LUA_VERSION.0, LUA_VERSION.1).into_bytes()),
    );

    // package
    table.set_field(
        gc.allocate_string(B("require")),
        NativeFunction::new(require),
    );
    let mut package = Table::new();
    package.set_field(
        gc.allocate_string(B("loaded")),
        gc.allocate_cell(Table::new()),
    );
    table.set_field(gc.allocate_string(B("package")), gc.allocate_cell(package));

    // others
    table.set_field(
        gc.allocate_string(B("string")),
        gc.allocate_cell(string::create_table(gc)),
    );
    table.set_field(
        gc.allocate_string(B("table")),
        gc.allocate_cell(table::create_table(gc)),
    );
    table.set_field(
        gc.allocate_string(B("math")),
        gc.allocate_cell(math::create_table(gc)),
    );
    table.set_field(
        gc.allocate_string(B("io")),
        gc.allocate_cell(io::create_table(gc)),
    );
    table.set_field(
        gc.allocate_string(B("os")),
        gc.allocate_cell(os::create_table(gc)),
    );
}

fn error_obj_to_error_kind(error_obj: Value) -> ErrorKind {
    let msg = if let Some(s) = error_obj.to_string() {
        String::from_utf8_lossy(&s).to_string()
    } else {
        format!("(error object is a {} value", error_obj.ty().name())
    };
    ErrorKind::ExplicitError(msg)
}

fn assert<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    if stack.arg(0).to_value()?.to_boolean() {
        stack.copy_within(1..stack.len(), 0);
        Ok(stack.args().len())
    } else if let Some(error_obj) = stack.arg(1).get() {
        Err(error_obj_to_error_kind(error_obj))
    } else {
        Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
    }
}

fn collectgarbage<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = match stack.arg(0).to_string()?.as_ref() {
        b"count" => ((gc.total_bytes() as Number) / 1024.0).into(),
        opt => todo!("{}", opt.as_bstr()),
    };
    Ok(1)
}

fn error<'gc>(
    _: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(window);
    let error_obj = stack.arg(0).get().unwrap_or_default();
    Err(error_obj_to_error_kind(error_obj))
}

fn getmetatable<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
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

fn ipairs<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    fn iterate<'gc>(
        gc: &'gc GcContext,
        _: &Vm<'gc>,
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
    ) -> Result<usize, ErrorKind> {
        let mut thread = thread.borrow_mut(gc);
        let stack = thread.stack_mut(window);
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

    let mut thread = thread.borrow_mut(gc);
    let window = thread.ensure_stack(window, 3);
    let stack = thread.stack_mut(window);
    stack[0] = NativeFunction::new(iterate).into();
    stack[2] = 0.into();
    Ok(3)
}

fn print<'gc>(
    _: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(window);
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

fn rawequal<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = (stack.arg(0).to_value()? == stack.arg(1).to_value()?).into();
    Ok(1)
}

fn rawget<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let index = stack.arg(1).to_value()?;
    stack[0] = stack.arg(0).as_table()?.get(index);
    Ok(1)
}

fn rawlen<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let len = match stack.arg(0).get() {
        Some(Value::Table(t)) => t.borrow().lua_len(),
        Some(Value::String(s)) => s.len() as Integer,
        value => {
            return Err(ErrorKind::ArgumentTypeError {
                nth: 0,
                expected_type: "table or string",
                got_type: value.map(|value| value.ty().name()),
            })
        }
    };
    stack[0] = len.into();
    Ok(1)
}

fn rawset<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);

    let table = stack.arg(0);
    let index = stack.arg(1).to_value()?;
    let value = stack.arg(2).to_value()?;

    table.as_table_mut(gc)?.set(index, value);

    stack[0] = table.to_value()?;
    Ok(1)
}

fn select<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);

    let num_args = stack.args().len() as Integer;
    let index = stack.arg(0);

    if let Some(Value::String(s)) = index.get() {
        if s.as_ref() == b"#" {
            stack[0] = (num_args - 1).into();
            return Ok(1);
        }
    }

    let index = match index.to_integer()? {
        i if i < 0 => i + num_args,
        i if i > num_args => num_args,
        i => i,
    };
    if index < 1 {
        return Err(ErrorKind::ArgumentError {
            nth: 0,
            message: "index out of range",
        });
    }
    stack.copy_within((index + 1) as usize.., 0);
    Ok((num_args - index) as usize)
}

fn setmetatable<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let table = {
        let table_arg = stack.arg(0);
        let mut table = table_arg.as_table_mut(gc)?;
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

fn tonumber<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = match stack.arg(0).to_value()? {
        Value::Integer(x) => Value::Integer(x),
        Value::Number(x) => Value::Number(x),
        Value::String(s) => {
            let base = stack.arg(1);
            let maybe_value = if base.get().is_some() {
                let base = base.to_integer()?;
                if !(2..=36).contains(&base) {
                    return Err(ErrorKind::ArgumentError {
                        nth: 1,
                        message: "base out of range",
                    });
                }
                s.as_str()
                    .ok()
                    .and_then(|s| Integer::from_str_radix(s, base as u32).ok())
                    .map(|i| i.into())
            } else {
                s.as_str().ok().map(|s| {
                    if let Ok(i) = s.parse() {
                        Value::Integer(i)
                    } else if let Ok(f) = s.parse() {
                        Value::Number(f)
                    } else {
                        Value::Nil
                    }
                })
            };
            maybe_value.unwrap_or(Value::Nil)
        }
        _ => Value::Nil,
    };
    Ok(1)
}

fn tostring<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let mut string = Vec::new();
    stack.arg(0).to_value()?.fmt_bytes(&mut string)?;
    stack[0] = gc.allocate_string(string).into();
    Ok(1)
}

fn ty<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let string = stack.arg(0).to_value()?.ty().name().as_bytes();
    stack[0] = gc.allocate_string(string).into();
    Ok(1)
}

fn require<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(window.clone());

    let module_name = stack.arg(0);
    let module_name = module_name.to_string()?;
    let module_name = gc.allocate_string(module_name);

    let package_key = gc.allocate_string(B("package"));
    let package_table = vm.global_table().borrow().get_field(package_key);
    let package_table = package_table.as_table().unwrap();

    let loaded_key = gc.allocate_string(B("loaded"));
    let loaded_table = package_table.get_field(loaded_key);
    let maybe_loaded_value = loaded_table.as_table().unwrap().get_field(module_name);

    if !maybe_loaded_value.is_nil() {
        stack[0] = maybe_loaded_value;
        return Ok(1);
    }
    drop(thread_ref);

    let filename = format!("./{}.lua", module_name.as_bstr());
    let lua_filename = gc.allocate_string(filename.clone().into_bytes());

    let proto = match crate::load_file(gc, &filename) {
        Ok(proto) => proto,
        Err(err) => return Err(ErrorKind::ExplicitError(err.to_string())),
    };
    let mut closure = LuaClosure::from(gc.allocate(proto));
    closure
        .upvalues
        .push(gc.allocate_cell(Value::Table(vm.global_table()).into()));

    root_gc!(gc, module_name.0);
    root_gc!(gc, lua_filename.0);

    let value = unsafe {
        vm.execute_value(
            gc,
            thread,
            gc.allocate(closure).into(),
            &[module_name.into(), lua_filename.into()],
        )?
    };

    let value = if value.is_nil() {
        Value::Boolean(true)
    } else {
        value
    };

    loaded_table
        .as_table_mut(gc)
        .unwrap()
        .set_field(module_name, value);

    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(window);
    stack[0] = value;
    stack[1] = lua_filename.into();
    Ok(2)
}
