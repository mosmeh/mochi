use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, Continuation, ErrorKind, Vm},
    string,
    types::{Integer, LuaClosure, NativeClosure, NativeFunction, Number, Table, Value},
    LUA_VERSION,
};
use bstr::{ByteSlice, B};
use std::{
    cell::Cell,
    io::{Read, Write},
    rc::Rc,
};

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
            (B("loadfile"), base_loadfile),
            (B("next"), base_next),
            (B("pairs"), base_pairs),
            (B("pcall"), base_pcall),
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

    let warning_is_on = Rc::new(Cell::new(false));
    globals.set_field(
        gc.allocate_string(B("warn")),
        gc.allocate(NativeClosure::new(move |_, _, args| {
            let first_message = args.nth(1);
            let first_message = first_message.to_string()?;
            if args.without_callee().len() == 1 {
                if let Some(control) = first_message.strip_prefix(b"@") {
                    match control {
                        b"on" => warning_is_on.set(true),
                        b"off" => warning_is_on.set(false),
                        _ => (),
                    }
                    return Ok(Action::Return(Vec::new()));
                }
            }

            let mut concatenated = first_message.to_vec();
            for i in 2..args.len() {
                concatenated.extend_from_slice(&args.nth(i).to_string()?);
            }

            if warning_is_on.get() {
                eprintln!("Lua warning: {}", concatenated.as_bstr());
            }
            Ok(Action::Return(Vec::new()))
        })),
    );

    vm.globals()
}

fn base_assert<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    if args.nth(1).as_value()?.to_boolean() {
        Ok(Action::Return(args.without_callee().to_vec()))
    } else if let Some(error_obj) = args.nth(2).get() {
        Err(ErrorKind::from_error_object(error_obj))
    } else {
        Err(ErrorKind::other("assertion failed!"))
    }
}

fn base_collectgarbage<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let opt = args.nth(1);
    let opt = opt.to_string_or(B("collect"))?;

    let result = match opt.as_ref() {
        b"collect" => {
            return Ok(Action::MutateGc {
                mutator: Box::new(|heap| {
                    heap.full_gc();
                }),
                continuation: Continuation::new(|_, _, _| Ok(Action::Return(vec![0.into()]))),
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
            let step = args.nth(2).to_integer_or(0)?;
            return Ok(Action::MutateGc {
                mutator: Box::new(move |heap| {
                    let finished_cycle = heap.force_step(step as isize);
                    heap.with(|gc, vm| {
                        vm.borrow()
                            .current_thread()
                            .borrow_mut(gc)
                            .stack
                            .push(finished_cycle.into());
                    });
                }),
                continuation: Continuation::new(|gc, vm, _| {
                    let result = vm.current_thread().borrow_mut(gc).stack.pop().unwrap();
                    Ok(Action::Return(vec![result]))
                }),
            });
        }
        b"isrunning" => gc.is_running().into(),
        b"incremental" => {
            let pause = args.nth(2).to_integer_or(0)?;
            let step_multiplier = args.nth(3).to_integer_or(0)?;
            let step_size = args.nth(4).to_integer_or(0)?;
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
            let pause = args.nth(2).to_integer_or(0)?;
            let prev_pause = gc.pause();
            gc.set_pause(pause as usize);
            (prev_pause as Integer).into()
        }
        b"setstepmul" => {
            let step_multiplier = args.nth(2).to_integer_or(0)?;
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let filename = args.nth(1);
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let error_obj = args.nth(1).get().unwrap_or_default();
    Err(ErrorKind::from_error_object(error_obj))
}

fn base_getmetatable<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let object = args.nth(1).as_value()?;
    let metatable = vm
        .metatable_of_object(object)
        .map(|metatable| {
            let value = metatable
                .borrow()
                .get_field(gc.allocate_string(B("__metatable")));
            if value.is_nil() {
                metatable.into()
            } else {
                value
            }
        })
        .unwrap_or_default();
    Ok(Action::Return(vec![metatable]))
}

fn base_ipairs<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    fn iterate<'gc>(
        _: &'gc GcContext,
        _: &mut Vm<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<Action<'gc>, ErrorKind> {
        let i = args.nth(2).to_integer()?.wrapping_add(1);
        let value = args.nth(1).as_table()?.borrow().get(i);

        Ok(Action::Return(if value.is_nil() {
            vec![Value::Nil]
        } else {
            vec![i.into(), value]
        }))
    }

    let table = args.nth(1).as_value()?;

    Ok(Action::Return(vec![
        NativeFunction::new(iterate).into(),
        table,
        0.into(),
    ]))
}

fn base_load<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let mode = args.nth(3);
    let mode = mode.to_string_or(B("bt"))?;
    if !mode.contains(&b'b') || !mode.contains(&b't') {
        todo!("mode != \"bt\"")
    }

    let proto = if let Some(Value::String(bytes)) = args.nth(1).get() {
        let chunk_name = args.nth(2);
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
    let upvalue = if let Some(upvalue) = args.nth(4).get() {
        upvalue.into()
    } else {
        Value::Table(vm.globals()).into()
    };
    closure.upvalues.push(gc.allocate_cell(upvalue));

    Ok(Action::Return(vec![gc.allocate(closure).into()]))
}

fn base_loadfile<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let mode = args.nth(2);
    let mode = mode.to_string_or(B("bt"))?;
    if !mode.contains(&b'b') || !mode.contains(&b't') {
        todo!("mode != \"bt\"")
    }

    let proto = if let Some(Value::String(filename)) = args.nth(1).get() {
        filename
            .to_path()
            .map_err(|err| err.to_string())
            .and_then(|path| crate::load_file(gc, path).map_err(|err| err.to_string()))
    } else {
        let mut bytes = Vec::new();
        std::io::stdin()
            .read_to_end(&mut bytes)
            .map_err(Into::into)
            .and_then(|_| crate::load(gc, bytes, b"=stdin"))
            .map_err(|err| err.to_string())
    };
    let proto = match proto {
        Ok(proto) => proto,
        Err(err) => {
            return Ok(Action::Return(vec![
                Value::Nil,
                gc.allocate_string(err.into_bytes()).into(),
            ]))
        }
    };

    let mut closure = LuaClosure::from(gc.allocate(proto));
    let upvalue = if let Some(upvalue) = args.nth(3).get() {
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let table = table.borrow();
    let index = args.nth(2).get().unwrap_or_default();

    Ok(Action::Return(
        if let Some((key, value)) = table.next(index)? {
            vec![key, value]
        } else {
            vec![Value::Nil]
        },
    ))
}

fn base_pairs<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_value()?;
    let metamethod = vm.metatable_of_object(table).and_then(|metatable| {
        let value = metatable
            .borrow()
            .get_field(gc.allocate_string(B("__pairs")));
        (!value.is_nil()).then_some(value)
    });
    match metamethod {
        Some(metamethod) => Ok(Action::TailCall {
            callee: metamethod,
            args: vec![table],
        }),
        None => Ok(Action::Return(vec![
            NativeFunction::new(base_next).into(),
            table,
            Value::Nil,
        ])),
    }
}

fn base_pcall<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let f = args.nth(1).as_value()?;
    Ok(Action::ProtectedCall {
        callee: f,
        args: args.without_callee()[1..].to_vec(),
        continuation: Continuation::new(|gc, _, result: Result<Vec<Value>, ErrorKind>| {
            Ok(Action::Return(match result {
                Ok(mut results) => {
                    results.insert(0, true.into());
                    results
                }
                Err(err) => {
                    vec![
                        false.into(),
                        gc.allocate_string(err.to_string().into_bytes()).into(),
                    ]
                }
            }))
        }),
    })
}

fn base_print<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let mut stdout = std::io::stdout().lock();
    if let Some((last, xs)) = args.without_callee().split_last() {
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let v1 = args.nth(1).as_value()?;
    let v2 = args.nth(2).as_value()?;
    Ok(Action::Return(vec![(v1 == v2).into()]))
}

fn base_rawget<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let index = args.nth(2).as_value()?;
    let value = args.nth(1).as_table()?.borrow().get(index);
    Ok(Action::Return(vec![value]))
}

fn base_rawlen<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let len = match args.nth(1).get() {
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let index = args.nth(2).as_value()?;
    let value = args.nth(3).as_value()?;

    table.borrow_mut(gc).set(index, value)?;

    Ok(Action::Return(vec![table.into()]))
}

fn base_select<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let num_args = args.without_callee().len() as Integer;
    let index = args.nth(1);

    match index.get() {
        Some(Value::String(s)) if s.as_ref() == b"#" => {
            return Ok(Action::Return(vec![(num_args - 1).into()]))
        }
        _ => (),
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
    Ok(Action::Return(
        args.without_callee()[index as usize..].to_vec(),
    ))
}

fn base_setmetatable<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let new_metatable = match args.nth(2).get() {
        Some(Value::Nil) => None,
        Some(Value::Table(t)) => Some(t),
        value => {
            return Err(ErrorKind::ArgumentTypeError {
                nth: 2,
                expected_type: "nil or table",
                got_type: value.map(|value| value.ty().name()),
            })
        }
    };
    match table.borrow().metatable() {
        Some(metatable)
            if !metatable
                .borrow()
                .get_field(gc.allocate_string(B("__metatable")))
                .is_nil() =>
        {
            return Err(ErrorKind::other("cannot change a protected metatable"))
        }
        _ => (),
    }
    table.borrow_mut(gc).set_metatable(new_metatable);
    Ok(Action::Return(vec![table.into()]))
}

fn base_tonumber<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let result = match args.nth(1).as_value()? {
        Value::Integer(x) => Value::Integer(x),
        Value::Number(x) => Value::Number(x),
        value @ Value::String(s) => {
            let base = args.nth(2);
            let maybe_value = if base.is_present() {
                let base = base.to_integer()?;
                if !(2..=36).contains(&base) {
                    return Err(ErrorKind::ArgumentError {
                        nth: 2,
                        message: "base out of range",
                    });
                }
                string::trim_whitespaces(&s)
                    .to_str()
                    .ok()
                    .and_then(|s| Integer::from_str_radix(s, base as u32).ok())
                    .map(|i| i.into())
            } else {
                value
                    .to_integer()
                    .map(Value::from)
                    .or_else(|| value.to_number().map(Value::from))
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
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let mut string = Vec::new();
    args.nth(1).as_value()?.fmt_bytes(&mut string)?;
    Ok(Action::Return(vec![gc.allocate_string(string).into()]))
}

fn base_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let string = args.nth(1).as_value()?.ty().name().as_bytes();
    Ok(Action::Return(vec![gc.allocate_string(string).into()]))
}
