use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{
        Action, Integer, LuaClosure, LuaThread, NativeFunction, Number, StackWindow, Table, Value,
    },
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use std::io::{Read, Write};

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let globals = vm.globals();
    let mut globals = globals.borrow_mut(gc);
    set_functions_to_table(
        gc,
        &mut globals,
        &[
            (B("assert"), base_assert),
            (B("collectgarbage"), base_collectgarbage),
            (B("dofile"), base_dofile),
            (B("error"), base_error),
            (B("getmetatable"), base_getmetatable),
            (B("ipairs"), base_ipairs),
            (B("load"), base_load),
            (B("next"), base_next),
            (B("pairs"), base_pairs),
            (B("print"), base_print),
            (B("rawequal"), base_rawequal),
            (B("rawget"), base_rawget),
            (B("rawlen"), base_rawlen),
            (B("rawset"), base_rawset),
            (B("select"), base_select),
            (B("setmetatable"), base_setmetatable),
            (B("tonumber"), base_tonumber),
            (B("tostring"), base_tostring),
            (B("type"), base_type),
        ],
    );
    globals.set_field(
        gc.allocate_string(B("_VERSION")),
        gc.allocate_string(format!("Lua {}.{}", LUA_VERSION.0, LUA_VERSION.1).into_bytes()),
    );
    vm.globals()
}

fn base_assert<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    if stack.arg(1).as_value()?.to_boolean() {
        Ok(Action::Return(stack.args().to_vec()))
    } else if let Some(error_obj) = stack.arg(2).get() {
        Err(ErrorKind::from_error_object(error_obj))
    } else {
        Err(ErrorKind::other("assertion failed!"))
    }
}

fn base_collectgarbage<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let opt = stack.arg(1);
    let opt = opt.to_string_or(B("collect"))?;

    let result = match opt.as_ref() {
        b"collect" => {
            return Ok(Action::MutateGc {
                mutator: Box::new(|heap| {
                    heap.full_gc();
                }),
                continuation: Box::new(|_, _, _, _| Ok(Action::Return(vec![0.into()]))),
            })
        }
        b"stop" => {
            gc.stop();
            0.into()
        }
        b"restart" => {
            gc.restart();
            0.into()
        }
        b"count" => ((gc.total_bytes() as Number) / 1024.0).into(),
        b"step" => {
            let step = stack.arg(2).to_integer_or(0)?;
            thread.resize_stack(&mut window, 0);
            return Ok(Action::MutateGc {
                mutator: Box::new(move |heap| {
                    let finished_cycle = heap.force_step(step as isize);
                    heap.with(|gc, vm| {
                        vm.borrow()
                            .current_thread()
                            .unwrap()
                            .borrow_mut(gc)
                            .stack
                            .push(finished_cycle.into());
                    });
                }),
                continuation: Box::new(|_, _, thread, window| {
                    let result = thread.borrow().stack(&window).arg(0).as_value()?;
                    Ok(Action::Return(vec![result]))
                }),
            });
        }
        b"isrunning" => gc.is_running().into(),
        b"incremental" => {
            let pause = stack.arg(2).to_integer_or(0)?;
            let step_multiplier = stack.arg(3).to_integer_or(0)?;
            let step_size = stack.arg(4).to_integer_or(0)?;
            if pause != 0 {
                gc.set_pause(pause as usize);
            }
            if step_multiplier != 0 {
                gc.set_step_multiplier(step_multiplier as usize);
            }
            if step_size != 0 {
                gc.set_step_size(step_size as usize);
            }
            gc.allocate_string(B("incremental")).into()
        }
        b"generational" => Value::Nil,
        b"setpause" => {
            let pause = stack.arg(2).to_integer_or(0)?;
            let prev_pause = gc.pause();
            gc.set_pause(pause as usize);
            (prev_pause as Integer).into()
        }
        b"setstepmul" => {
            let step_multiplier = stack.arg(2).to_integer_or(0)?;
            let prev_step_multiplier = gc.step_multiplier();
            gc.set_step_multiplier(step_multiplier as usize);
            (prev_step_multiplier as Integer).into()
        }
        _ => {
            return Err(ErrorKind::ArgumentError {
                nth: 1,
                message: "invalid option",
            })
        }
    };
    Ok(Action::Return(vec![result]))
}

fn base_dofile<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let filename = thread.borrow().stack(&window).arg(1);
    let closure = if filename.is_present() {
        let filename = filename.to_string()?;
        let path = filename
            .to_path()
            .map_err(|e| ErrorKind::Other(e.to_string()))?;
        vm.load_file(gc, path)
            .map_err(|e| ErrorKind::Other(e.to_string()))?
    } else {
        let mut bytes = Vec::new();
        std::io::stdin().read_to_end(&mut bytes)?;
        vm.load(gc, &bytes, B("=stdin"))
            .map_err(|e| ErrorKind::Other(e.to_string()))?
    };

    Ok(Action::TailCall {
        callee: gc.allocate(closure).into(),
        args: Vec::new(),
    })
}

fn base_error<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let error_obj = thread
        .borrow()
        .stack(&window)
        .arg(1)
        .get()
        .unwrap_or_default();
    Err(ErrorKind::from_error_object(error_obj))
}

fn base_getmetatable<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let object = thread.borrow().stack(&window).arg(1).as_value()?;
    let metatable = vm
        .metatable_of_object(object)
        .map(Value::from)
        .unwrap_or_default();
    Ok(Action::Return(vec![metatable]))
}

fn base_ipairs<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    fn iterate<'gc>(
        _: &'gc GcContext,
        _: &mut Vm<'gc>,
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
    ) -> Result<Action<'gc>, ErrorKind> {
        let thread = thread.borrow();
        let stack = thread.stack(&window);
        let i = stack.arg(2).to_integer()? + 1;
        let value = stack.arg(1).borrow_as_table()?.get(i);

        Ok(Action::Return(if value.is_nil() {
            vec![Value::Nil]
        } else {
            vec![i.into(), value]
        }))
    }

    let thread = thread.borrow();
    let table = thread.stack(&window).arg(1).as_value()?;

    Ok(Action::Return(vec![
        NativeFunction::new(iterate).into(),
        table,
        0.into(),
    ]))
}

fn base_load<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let mode = stack.arg(3);
    let mode = mode.to_string_or(B("bt"))?;
    if mode.as_ref() != b"bt" {
        todo!("mode != \"bt\"")
    }

    let proto = if let Some(Value::String(bytes)) = stack.arg(1).get() {
        let chunk_name = stack.arg(2);
        let chunk_name = chunk_name.to_string_or(&*bytes)?;
        match crate::load(gc, bytes, chunk_name) {
            Ok(proto) => proto,
            Err(err) => {
                return Ok(Action::Return(vec![
                    Value::Nil,
                    gc.allocate_string(err.to_string().into_bytes()).into(),
                ]))
            }
        }
    } else {
        todo!("load from function")
    };

    let mut closure = LuaClosure::from(gc.allocate(proto));
    let upvalue = if let Some(upvalue) = stack.arg(4).get() {
        upvalue.into()
    } else {
        Value::Table(vm.globals()).into()
    };
    closure.upvalues.push(gc.allocate_cell(upvalue));

    Ok(Action::Return(vec![gc.allocate(closure).into()]))
}

fn base_next<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let table = stack.arg(1);
    let table = table.borrow_as_table()?;
    let index = stack.arg(2).get().unwrap_or_default();

    Ok(Action::Return(
        if let Some((key, value)) = table.next(index)? {
            vec![key, value]
        } else {
            vec![Value::Nil]
        },
    ))
}

fn base_pairs<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let table = thread.borrow().stack(&window).arg(1).as_value()?;

    Ok(Action::Return(vec![
        NativeFunction::new(base_next).into(),
        table,
        Value::Nil,
    ]))
}

fn base_print<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let mut stdout = std::io::stdout().lock();
    if let Some((last, xs)) = thread.borrow().stack(&window).args().split_last() {
        for x in xs {
            x.fmt_bytes(&mut stdout)?;
            stdout.write_all(b"\t")?;
        }
        last.fmt_bytes(&mut stdout)?;
    }
    stdout.write_all(b"\n")?;
    Ok(Action::Return(Vec::new()))
}

fn base_rawequal<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let v1 = stack.arg(1).as_value()?;
    let v2 = stack.arg(2).as_value()?;
    Ok(Action::Return(vec![(v1 == v2).into()]))
}

fn base_rawget<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let index = stack.arg(2).as_value()?;
    let value = stack.arg(1).borrow_as_table()?.get(index);
    Ok(Action::Return(vec![value]))
}

fn base_rawlen<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let len = match thread.borrow().stack(&window).arg(1).get() {
        Some(Value::Table(t)) => t.borrow().lua_len(),
        Some(Value::String(s)) => s.len() as Integer,
        value => {
            return Err(ErrorKind::ArgumentTypeError {
                nth: 1,
                expected_type: "table or string",
                got_type: value.map(|value| value.ty().name()),
            })
        }
    };
    Ok(Action::Return(vec![len.into()]))
}

fn base_rawset<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let table = stack.arg(1);
    let index = stack.arg(2).as_value()?;
    let value = stack.arg(3).as_value()?;

    table.borrow_as_table_mut(gc)?.set(index, value)?;

    Ok(Action::Return(vec![table.as_value()?]))
}

fn base_select<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let num_args = stack.args().len() as Integer;
    let index = stack.arg(1);

    if let Some(Value::String(s)) = index.get() {
        if s.as_ref() == b"#" {
            return Ok(Action::Return(vec![(num_args - 1).into()]));
        }
    }

    let index = match index.to_integer()? {
        i if i < 0 => i + num_args,
        i if i > num_args => num_args,
        i => i,
    };
    if index < 1 {
        return Err(ErrorKind::ArgumentError {
            nth: 1,
            message: "index out of range",
        });
    }
    Ok(Action::Return(stack.args()[index as usize..].to_vec()))
}

fn base_setmetatable<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let table = {
        let table_arg = stack.arg(1);
        let mut table = table_arg.borrow_as_table_mut(gc)?;
        let metatable = match stack.arg(2).get() {
            Some(Value::Nil) => None,
            Some(Value::Table(table)) => Some(table),
            value => {
                return Err(ErrorKind::ArgumentTypeError {
                    nth: 2,
                    expected_type: "nil or table",
                    got_type: value.map(|value| value.ty().name()),
                })
            }
        };
        table.set_metatable(metatable);
        table_arg.as_value()?
    };
    Ok(Action::Return(vec![table]))
}

fn base_tonumber<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    let result = match stack.arg(1).as_value()? {
        Value::Integer(x) => Value::Integer(x),
        Value::Number(x) => Value::Number(x),
        Value::String(s) => {
            let base = stack.arg(2);
            let maybe_value = if base.is_present() {
                let base = base.to_integer()?;
                if !(2..=36).contains(&base) {
                    return Err(ErrorKind::ArgumentError {
                        nth: 2,
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
    Ok(Action::Return(vec![result]))
}

fn base_tostring<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let mut string = Vec::new();
    thread
        .borrow()
        .stack(&window)
        .arg(1)
        .as_value()?
        .fmt_bytes(&mut string)?;
    Ok(Action::Return(vec![gc.allocate_string(string).into()]))
}

fn base_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let string = thread
        .borrow()
        .stack(&window)
        .arg(1)
        .as_value()?
        .ty()
        .name()
        .as_bytes();
    Ok(Action::Return(vec![gc.allocate_string(string).into()]))
}
