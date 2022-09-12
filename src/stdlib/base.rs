use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{
        Action, Integer, LuaClosure, LuaThread, NativeFunction, Number, StackWindow, Table, Value,
    },
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use std::io::Write;

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let globals = vm.globals();
    let mut globals = globals.borrow_mut(gc);
    globals.set_field(
        gc.allocate_string(B("assert")),
        NativeFunction::new(base_assert),
    );
    globals.set_field(
        gc.allocate_string(B("collectgarbage")),
        NativeFunction::new(base_collectgarbage),
    );
    globals.set_field(
        gc.allocate_string(B("error")),
        NativeFunction::new(base_error),
    );
    globals.set_field(
        gc.allocate_string(B("getmetatable")),
        NativeFunction::new(base_getmetatable),
    );
    globals.set_field(
        gc.allocate_string(B("ipairs")),
        NativeFunction::new(base_ipairs),
    );
    globals.set_field(
        gc.allocate_string(B("load")),
        NativeFunction::new(base_load),
    );
    globals.set_field(
        gc.allocate_string(B("next")),
        NativeFunction::new(base_next),
    );
    globals.set_field(
        gc.allocate_string(B("pairs")),
        NativeFunction::new(base_pairs),
    );
    globals.set_field(
        gc.allocate_string(B("print")),
        NativeFunction::new(base_print),
    );
    globals.set_field(
        gc.allocate_string(B("rawequal")),
        NativeFunction::new(base_rawequal),
    );
    globals.set_field(
        gc.allocate_string(B("rawget")),
        NativeFunction::new(base_rawget),
    );
    globals.set_field(
        gc.allocate_string(B("rawlen")),
        NativeFunction::new(base_rawlen),
    );
    globals.set_field(
        gc.allocate_string(B("rawset")),
        NativeFunction::new(base_rawset),
    );
    globals.set_field(
        gc.allocate_string(B("select")),
        NativeFunction::new(base_select),
    );
    globals.set_field(
        gc.allocate_string(B("setmetatable")),
        NativeFunction::new(base_setmetatable),
    );
    globals.set_field(
        gc.allocate_string(B("tonumber")),
        NativeFunction::new(base_tonumber),
    );
    globals.set_field(
        gc.allocate_string(B("tostring")),
        NativeFunction::new(base_tostring),
    );
    globals.set_field(
        gc.allocate_string(B("type")),
        NativeFunction::new(base_type),
    );
    globals.set_field(
        gc.allocate_string(B("_VERSION")),
        gc.allocate_string(format!("Lua {}.{}", LUA_VERSION.0, LUA_VERSION.1).into_bytes()),
    );
    vm.globals()
}

fn base_assert<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    if stack.arg(0).to_value()?.to_boolean() {
        stack.copy_within(1..stack.len(), 0);
        Ok(Action::Return {
            num_results: stack.args().len(),
        })
    } else if let Some(error_obj) = stack.arg(1).get() {
        Err(ErrorKind::from_error_object(error_obj))
    } else {
        Err(ErrorKind::ExplicitError("assertion failed!".to_owned()))
    }
}

fn base_collectgarbage<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack[0] = match stack.arg(0).to_string()?.as_ref() {
        b"count" => ((gc.total_bytes() as Number) / 1024.0).into(),
        opt => todo!("{}", opt.as_bstr()),
    };
    Ok(Action::Return { num_results: 1 })
}

fn base_error<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let error_obj = stack.arg(0).get().unwrap_or_default();
    Err(ErrorKind::from_error_object(error_obj))
}

fn base_getmetatable<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let object = stack.arg(0).to_value()?;
    stack[0] = vm
        .metatable_of_object(object)
        .map(Value::from)
        .unwrap_or_default();
    Ok(Action::Return { num_results: 1 })
}

fn base_ipairs<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    fn iterate<'gc>(
        gc: &'gc GcContext,
        _: &mut Vm<'gc>,
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
    ) -> Result<Action, ErrorKind> {
        let mut thread = thread.borrow_mut(gc);
        let stack = thread.stack_mut(&window);
        let i = stack.arg(1).to_integer()? + 1;
        let value = stack.arg(0).borrow_as_table()?.get(i);

        if value.is_nil() {
            stack[0] = Value::Nil;
            Ok(Action::Return { num_results: 1 })
        } else {
            stack[0] = i.into();
            stack[1] = value;
            Ok(Action::Return { num_results: 2 })
        }
    }

    let mut thread = thread.borrow_mut(gc);
    thread.stack(&window).arg(0).to_value()?;

    thread.ensure_stack(&mut window, 3);
    let stack = thread.stack_mut(&window);
    stack[0] = NativeFunction::new(iterate).into();
    stack[2] = 0.into();
    Ok(Action::Return { num_results: 3 })
}

fn base_load<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let mode = stack.arg(2);
    let mode = mode.to_string_or(B("bt"))?;
    if mode.as_ref() != b"bt" {
        todo!("mode != \"bt\"")
    }

    let proto = if let Some(Value::String(bytes)) = stack.arg(0).get() {
        let chunk_name = stack.arg(1);
        let chunk_name = chunk_name.to_string_or(&*bytes)?;
        match crate::load(gc, bytes, chunk_name) {
            Ok(proto) => proto,
            Err(err) => {
                stack[0] = Value::Nil;
                stack[1] = gc.allocate_string(err.to_string().into_bytes()).into();
                return Ok(Action::Return { num_results: 2 });
            }
        }
    } else {
        todo!("load from function")
    };

    let mut closure = LuaClosure::from(gc.allocate(proto));
    let upvalue = if let Some(upvalue) = stack.arg(3).get() {
        upvalue.into()
    } else {
        Value::Table(vm.globals()).into()
    };
    closure.upvalues.push(gc.allocate_cell(upvalue));

    stack[0] = gc.allocate(closure).into();
    Ok(Action::Return { num_results: 1 })
}

fn base_next<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let table = stack.arg(0);
    let table = table.borrow_as_table()?;
    let index = stack.arg(1).get().unwrap_or_default();

    if let Some((key, value)) = table.next(index)? {
        stack[0] = key;
        stack[1] = value;
        Ok(Action::Return { num_results: 2 })
    } else {
        stack[0] = Value::Nil;
        Ok(Action::Return { num_results: 1 })
    }
}

fn base_pairs<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    thread.stack(&window).arg(0).to_value()?;

    thread.ensure_stack(&mut window, 3);
    let stack = thread.stack_mut(&window);
    stack[0] = NativeFunction::new(base_next).into();
    stack[2] = Value::Nil;
    Ok(Action::Return { num_results: 3 })
}

fn base_print<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let mut stdout = std::io::stdout().lock();
    if let Some((last, xs)) = stack.args().split_last() {
        for x in xs {
            x.fmt_bytes(&mut stdout)?;
            stdout.write_all(b"\t")?;
        }
        last.fmt_bytes(&mut stdout)?;
    }
    stdout.write_all(b"\n")?;
    Ok(Action::Return { num_results: 0 })
}

fn base_rawequal<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack[0] = (stack.arg(0).to_value()? == stack.arg(1).to_value()?).into();
    Ok(Action::Return { num_results: 1 })
}

fn base_rawget<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let index = stack.arg(1).to_value()?;
    stack[0] = stack.arg(0).borrow_as_table()?.get(index);
    Ok(Action::Return { num_results: 1 })
}

fn base_rawlen<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
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
    Ok(Action::Return { num_results: 1 })
}

fn base_rawset<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let table = stack.arg(0);
    let index = stack.arg(1).to_value()?;
    let value = stack.arg(2).to_value()?;

    table.borrow_as_table_mut(gc)?.set(index, value)?;

    stack[0] = table.to_value()?;
    Ok(Action::Return { num_results: 1 })
}

fn base_select<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let num_args = stack.args().len() as Integer;
    let index = stack.arg(0);

    if let Some(Value::String(s)) = index.get() {
        if s.as_ref() == b"#" {
            stack[0] = (num_args - 1).into();
            return Ok(Action::Return { num_results: 1 });
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
    Ok(Action::Return {
        num_results: (num_args - index) as usize,
    })
}

fn base_setmetatable<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let table = {
        let table_arg = stack.arg(0);
        let mut table = table_arg.borrow_as_table_mut(gc)?;
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
    Ok(Action::Return { num_results: 1 })
}

fn base_tonumber<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
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
    Ok(Action::Return { num_results: 1 })
}

fn base_tostring<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let mut string = Vec::new();
    stack.arg(0).to_value()?.fmt_bytes(&mut string)?;
    stack[0] = gc.allocate_string(string).into();
    Ok(Action::Return { num_results: 1 })
}

fn base_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let string = stack.arg(0).to_value()?.ty().name().as_bytes();
    stack[0] = gc.allocate_string(string).into();
    Ok(Action::Return { num_results: 1 })
}
