use super::{
    file::{self, FileError, FileHandle, FullyBufferedFile, LineBufferedFile, LuaFile},
    helpers::{set_functions_to_table, Argument, ArgumentsExt},
    process::{self, Process},
};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, ErrorKind, Metamethod, Vm},
    types::{Integer, Number, Table, Type, UserData, Value},
};
use bstr::{ByteSlice, B};
use std::{
    fs::OpenOptions,
    io::{Read, Seek, SeekFrom, Write},
    process::Stdio,
};

const LUA_FILEHANDLE: &[u8] = b"FILE*";
const IO_INPUT: &[u8] = b"_IO_input";
const IO_OUTPUT: &[u8] = b"_IO_output";

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("close"), io_close),
            (B("flush"), io_flush),
            (B("input"), io_input),
            (B("open"), io_open),
            (B("output"), io_output),
            (B("popen"), io_popen),
            (B("read"), io_read),
            (B("type"), io_type),
            (B("write"), io_write),
        ],
    );

    let mut methods = Table::new();
    set_functions_to_table(
        gc,
        &mut methods,
        &[
            (B("close"), file_close),
            (B("flush"), file_flush),
            (B("read"), file_read),
            (B("seek"), file_seek),
            (B("setvbuf"), file_setvbuf),
            (B("write"), file_write),
        ],
    );

    let mut metatable = Table::new();
    metatable.set_field(
        vm.metamethod_name(Metamethod::Index),
        gc.allocate_cell(methods),
    );
    let metatable = gc.allocate_cell(metatable);

    let registry = vm.registry();
    let mut registry = registry.borrow_mut(gc);
    registry.set_field(gc.allocate_string(LUA_FILEHANDLE), metatable);

    let stdin = gc.allocate_cell(create_file_handle(gc, &registry, LuaFile::stdin()));
    table.set_field(gc.allocate_string(B("stdin")), stdin);
    registry.set_field(gc.allocate_string(IO_INPUT), stdin);

    let stdout = gc.allocate_cell(create_file_handle(gc, &registry, LuaFile::stdout()));
    table.set_field(gc.allocate_string(B("stdout")), stdout);
    registry.set_field(gc.allocate_string(IO_OUTPUT), stdout);

    let stderr = gc.allocate_cell(create_file_handle(gc, &registry, LuaFile::stderr()));
    table.set_field(gc.allocate_string(B("stderr")), stderr);

    gc.allocate_cell(table)
}

fn io_close<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let file = args.nth(1);
    process::translate_and_return_error(gc, || {
        if file.is_none() {
            vm.registry()
                .borrow(gc)
                .get_field(gc.allocate_string(IO_OUTPUT))
                .borrow_as_userdata_mut::<FileHandle>(gc)
                .unwrap()
                .close()
        } else {
            file.borrow_as_userdata_mut::<FileHandle>(gc)?.close()
        }
    })
}

fn io_flush<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    _: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let output = vm
        .registry()
        .borrow(gc)
        .get_field(gc.allocate_string(IO_OUTPUT));
    let mut output = output.borrow_as_userdata_mut::<FileHandle>(gc).unwrap();
    file::translate_and_return_error(gc, || {
        if let Some(output) = output.get_mut() {
            output.flush()?;
            Ok(vec![true.into()])
        } else {
            Err(FileError::DefaultFileClosed { kind: "output" })
        }
    })
}

fn io_input<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    common_io_input_or_output(gc, vm, args, IO_INPUT, OpenOptions::new().read(true))
}

fn io_open<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let filename = args.nth(1);
    let filename = filename.to_string()?;
    let mode = args.nth(2);
    let mode = mode.to_string_or(B("r"))?;

    let mut options = OpenOptions::new();
    match mode.strip_suffix(b"b").unwrap_or(&mode) {
        b"r" => options.read(true),
        b"w" => options.write(true).truncate(true).create(true),
        b"a" => options.read(true).append(true).create(true),
        b"r+" => options.read(true).append(true),
        b"w+" => options.read(true).write(true).truncate(true).create(true),
        b"a+" => options.read(true).write(true).append(true).create(true),
        _ => {
            return Err(ErrorKind::ArgumentError {
                nth: 2,
                message: "invalid mode",
            })
        }
    };

    file::translate_and_return_error(gc, || {
        let handle = open_file(gc, &vm.registry().borrow(gc), &options, filename)?;
        Ok(vec![gc.allocate_cell(handle).into()])
    })
}

fn io_output<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    common_io_input_or_output(
        gc,
        vm,
        args,
        IO_OUTPUT,
        OpenOptions::new().write(true).truncate(true).create(true),
    )
}

fn io_popen<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let prog = args.nth(1);
    let prog = prog.to_string()?;
    let mode = args.nth(2);
    let mode = mode.to_string_or(B("r"))?;

    #[allow(unused_mut)]
    let mut mode = mode.as_ref();
    #[cfg(windows)]
    if let [_, b'b' | b't'] = mode {
        mode = &mode[..1];
    }

    file::translate_and_return_error(gc, || {
        let mut command = process::system(prog.to_os_str()?);
        match mode {
            b"r" => command.stdout(Stdio::piped()),
            b"w" => command.stdin(Stdio::piped()),
            _ => {
                return Err(ErrorKind::ArgumentError {
                    nth: 2,
                    message: "invalid mode",
                }
                .into())
            }
        };

        std::io::stdout().flush()?;
        let child = command.spawn()?;
        let registry = vm.registry();
        let registry = registry.borrow(gc);
        let handle = create_file_handle(gc, &registry, Process::from(child));
        Ok(vec![gc.allocate_cell(handle).into()])
    })
}

fn io_read<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let input = vm
        .registry()
        .borrow(gc)
        .get_field(gc.allocate_string(IO_INPUT));
    let mut input = input.borrow_as_userdata_mut::<FileHandle>(gc).unwrap();

    file::translate_and_return_error(gc, || {
        if let Some(input) = input.get_mut() {
            common_read(gc, input, &args, 1)
        } else {
            Err(FileError::DefaultFileClosed { kind: "input" })
        }
    })
}

fn io_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1).as_value()?;
    let result = if let Some(handle) = handle.borrow_as_userdata::<FileHandle>(gc) {
        let s = if handle.is_open() {
            B("file")
        } else {
            B("closed file")
        };
        gc.allocate_string(s).into()
    } else {
        Value::Nil
    };
    Ok(Action::Return(vec![result]))
}

fn io_write<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let output = vm
        .registry()
        .borrow(gc)
        .get_field(gc.allocate_string(IO_OUTPUT));
    let mut output_ref = output.borrow_as_userdata_mut::<FileHandle>(gc).unwrap();

    file::translate_and_return_error(gc, || {
        if let Some(output_ref) = output_ref.get_mut() {
            for i in 1..args.len() {
                write_arg(output_ref, &args.nth(i))?;
            }
            Ok(vec![output])
        } else {
            Err(FileError::DefaultFileClosed { kind: "output" })
        }
    })
}

fn file_close<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;
    process::translate_and_return_error(gc, || handle.close())
}

fn file_flush<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;
    file::translate_and_return_error(gc, || {
        if let Some(file) = handle.get_mut() {
            file.flush()?;
            Ok(vec![true.into()])
        } else {
            Err(FileError::Closed)
        }
    })
}

fn file_read<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;

    file::translate_and_return_error(gc, || {
        let file = if let Some(file) = handle.get_mut() {
            file
        } else {
            return Err(FileError::Closed);
        };
        common_read(gc, file, &args, 2)
    })
}

fn file_seek<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;

    let whence = args.nth(2);
    let whence = whence.to_string_or(B("cur"))?;
    let offset = args.nth(3).to_integer_or(0)?;

    file::translate_and_return_error(gc, || {
        let pos = match whence.as_ref() {
            b"set" => {
                if let Ok(offset) = offset.try_into() {
                    SeekFrom::Start(offset)
                } else {
                    return Err(FileError::InvalidOffset);
                }
            }
            b"cur" => SeekFrom::Current(offset),
            b"end" => SeekFrom::End(offset),
            _ => {
                return Err(ErrorKind::ArgumentError {
                    nth: 2,
                    message: "invalid option",
                }
                .into())
            }
        };

        if let Some(file) = handle.get_mut() {
            let new_pos = file.seek(pos)?;
            Ok(vec![(new_pos as Integer).into()])
        } else {
            Err(FileError::Closed)
        }
    })
}

fn file_setvbuf<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;

    let mode = args.nth(2);
    let mode = mode.to_string()?;

    let size = args.nth(3);
    let size = if size.is_present() {
        size.to_integer()?.try_into().ok()
    } else {
        None
    };

    file::translate_and_return_error(gc, || {
        match mode.as_ref() {
            b"no" => handle.replace_with(LuaFile::NonBuffered)?,
            b"full" => handle.replace_with(|file| {
                LuaFile::FullyBuffered(Box::new(match size {
                    Some(size) => FullyBufferedFile::with_capacity(size, file),
                    None => FullyBufferedFile::new(file),
                }))
            })?,
            b"line" => handle.replace_with(|file| {
                LuaFile::LineBuffered(Box::new(match size {
                    Some(size) => LineBufferedFile::with_capacity(size, file),
                    None => LineBufferedFile::new(file),
                }))
            })?,
            _ => {
                return Err(ErrorKind::ArgumentError {
                    nth: 2,
                    message: "invalid option",
                }
                .into())
            }
        };
        Ok(vec![true.into()])
    })
}

fn file_write<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let handle = args.nth(1);
    let mut handle_ref = handle.borrow_as_userdata_mut::<FileHandle>(gc)?;

    file::translate_and_return_error(gc, || {
        if let Some(file) = handle_ref.get_mut() {
            for i in 2..args.len() {
                write_arg(file, &args.nth(i))?;
            }
            Ok(vec![handle.as_value()?])
        } else {
            Err(FileError::Closed)
        }
    })
}

fn common_io_input_or_output<'gc, K: AsRef<[u8]>>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
    key: K,
    options: &OpenOptions,
) -> Result<Action<'gc>, ErrorKind> {
    let file = args.nth(1);
    let registry = vm.registry();
    let key = gc.allocate_string(key.as_ref());
    file::translate_and_raise_error(|| {
        let handle = match file.get() {
            None | Some(Value::Nil) => return Ok(vec![registry.borrow(gc).get_field(key)]),
            Some(Value::String(filename)) => {
                let handle = open_file(gc, &registry.borrow(gc), options, filename)?;
                gc.allocate_cell(handle).into()
            }
            Some(value) => {
                file.as_userdata::<FileHandle>(gc)?;
                value
            }
        };
        registry.borrow_mut(gc).set_field(key, handle);
        Ok(vec![handle])
    })
}

fn common_read<'gc>(
    gc: &'gc GcContext,
    file: &mut LuaFile,
    args: &[Value<'gc>],
    first_arg_index: usize,
) -> Result<Vec<Value<'gc>>, FileError> {
    fn read_line<'gc>(
        gc: &'gc GcContext,
        file: &mut LuaFile,
        chop: bool,
    ) -> Result<Option<Value<'gc>>, FileError> {
        let mut buf = Vec::new();
        let num_read = file.read_until(b'\n', &mut buf)?;
        if num_read > 0 {
            if chop && buf.last() == Some(&b'\n') {
                buf.pop().unwrap();
            }
            Ok(Some(gc.allocate_string(buf).into()))
        } else {
            Ok(None)
        }
    }

    if first_arg_index >= args.len() {
        let value = read_line(gc, file, true)?.unwrap_or_default();
        return Ok(vec![value]);
    }

    let mut values = Vec::new();
    for i in first_arg_index..args.len() {
        let arg = args.nth(i);
        if arg.as_value()?.ty() == Type::Number {
            let l = arg.to_integer()?;
            let mut buf = vec![0; l as usize];
            match file.read_exact(&mut buf) {
                Ok(()) => {
                    values.push(gc.allocate_string(buf).into());
                    continue;
                }
                Err(err) if err.kind() == std::io::ErrorKind::UnexpectedEof => {
                    values.push(Value::Nil);
                    break;
                }
                Err(err) => return Err(err.into()),
            }
        }

        let p = arg.to_string()?;
        let p = p.strip_prefix(B("*")).unwrap_or(&p);
        match p.first() {
            Some(b'n') => todo!("read number"),
            Some(b'a') => {
                let mut buf = Vec::new();
                file.read_to_end(&mut buf)?;
                values.push(gc.allocate_string(buf).into());
            }
            Some(b'l') => match read_line(gc, file, true)? {
                Some(line) => values.push(line),
                None => {
                    values.push(Value::Nil);
                    break;
                }
            },
            Some(b'L') => match read_line(gc, file, false)? {
                Some(line) => values.push(line),
                None => {
                    values.push(Value::Nil);
                    break;
                }
            },
            _ => {
                return Err(ErrorKind::ArgumentError {
                    nth: i,
                    message: "invalid format",
                }
                .into())
            }
        }
    }

    Ok(values)
}

fn create_file_handle<'gc, I>(gc: &'gc GcContext, registry: &Table<'gc>, inner: I) -> UserData<'gc>
where
    I: Into<LuaFile>,
{
    let mut handle = UserData::new(FileHandle::from(inner.into()));
    handle.set_metatable(
        registry
            .get_field(gc.allocate_string(LUA_FILEHANDLE))
            .as_table(),
    );
    handle
}

fn open_file<'gc, P: AsRef<[u8]>>(
    gc: &'gc GcContext,
    registry: &Table<'gc>,
    options: &OpenOptions,
    path: P,
) -> Result<UserData<'gc>, FileError> {
    let path = path.as_ref().to_path()?;
    let file = options.open(path)?;
    Ok(create_file_handle(
        gc,
        registry,
        FullyBufferedFile::new(file),
    ))
}

fn write_arg<W: std::io::Write>(writer: &mut W, arg: &Argument) -> Result<(), FileError> {
    match arg.get() {
        Some(Value::Integer(i)) => write!(writer, "{i}")?,
        Some(Value::Number(x)) => write_number(writer, x)?,
        _ => writer.write_all(&arg.to_string()?)?,
    }
    Ok(())
}

// sprintf("%.14g")
fn write_number<W: std::io::Write>(writer: &mut W, x: Number) -> std::io::Result<()> {
    const PRECISION: usize = 14;

    if x == 0.0 {
        return writer.write_all(b"0");
    } else if x.is_nan() {
        if x.is_sign_negative() {
            writer.write_all(b"-")?;
        }
        return writer.write_all(b"nan");
    }

    let log_x = x.abs().log10();
    let mut precision = PRECISION - 1;
    if log_x < -3.0 || (precision as Number) < log_x {
        return write!(writer, "{x:.precision$e}");
    }

    precision = (precision as isize - log_x.trunc() as isize) as usize;
    if log_x < 0.0 {
        precision += 1
    }
    let s = format!("{x:.precision$}");
    let mut s = s.as_str();
    if s.contains('.') {
        s = s.trim_end_matches('0').trim_end_matches('.');
    }
    writer.write_all(s.as_bytes())
}
