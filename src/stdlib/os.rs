use super::{
    file,
    helpers::{set_functions_to_table, ArgumentsExt},
};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, ErrorKind, Vm},
    types::{Integer, Table, Value},
};
use bstr::{ByteSlice, ByteVec, B};
use chrono::{DateTime, Datelike, Local, NaiveDateTime, TimeZone, Timelike, Utc};
use std::{
    ffi::OsStr,
    process::{Command, ExitStatus},
};

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("clock"), os_clock),
            (B("date"), os_date),
            (B("difftime"), os_difftime),
            (B("execute"), os_execute),
            (B("exit"), os_exit),
            (B("getenv"), os_getenv),
            (B("remove"), os_remove),
            (B("rename"), os_rename),
            (B("setlocale"), os_setlocale),
            (B("time"), os_time),
        ],
    );
    gc.allocate_cell(table)
}

fn os_clock<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    _: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let clock = cpu_time::ProcessTime::now()
        .as_duration()
        .as_secs_f64()
        .into();
    Ok(Action::Return(vec![clock]))
}

fn os_date<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let format = args.nth(1);
    let format = format.to_string_or(B("%c"))?;

    let time = args.nth(2).to_integer_or_else(|| Utc::now().timestamp())?;
    if NaiveDateTime::from_timestamp_opt(time, 0).is_none() {
        return Err(ErrorKind::ArgumentError {
            nth: 2,
            message: "time out-of-bounds",
        });
    }

    let (format, is_utc) = match format.strip_prefix(b"!".as_ref()) {
        Some(format) => (format, true),
        None => (format.as_ref(), false),
    };

    if format == b"*t" {
        let mut table = Table::new();
        if is_utc {
            let datetime = Utc.timestamp(time, 0);
            set_datetime_to_table(gc, &mut table, &datetime);
            table.set_field(gc.allocate_string(B("isdst")), false);
        } else {
            let datetime = Local.timestamp(time, 0);
            set_datetime_to_table(gc, &mut table, &datetime);
        }
        return Ok(Action::Return(vec![gc.allocate_cell(table).into()]));
    }

    const L_STRFTIME: &[u8] = b"aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%";
    // TODO: support E and O modifiers
    let mut format_iter = format.iter();
    while let Some(ch) = format_iter.next() {
        if *ch != b'%' {
            continue;
        }
        let invalid_spec = match format_iter.next() {
            Some(ch) if L_STRFTIME.contains(ch) => continue,
            Some(ch) => char::from(*ch).to_string(),
            None => "".to_owned(),
        };
        return Err(ErrorKind::Other(format!(
            "invalid conversion specifier '%{}'",
            invalid_spec
        )));
    }

    let format = format.to_str_lossy();
    let formatted = if is_utc {
        Utc.timestamp(time, 0).format(&format)
    } else {
        Local.timestamp(time, 0).format(&format)
    };
    Ok(Action::Return(vec![gc
        .allocate_string(formatted.to_string().into_bytes())
        .into()]))
}

fn os_difftime<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let t2 = args.nth(1).to_number()?;
    let t1 = args.nth(2).to_number()?;
    Ok(Action::Return(vec![(t2 - t1).into()]))
}

fn os_execute<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    fn system<S: AsRef<OsStr>>(line: S) -> std::io::Result<ExitStatus> {
        let mut command = {
            #[cfg(windows)]
            {
                let mut command = Command::new("cmd");
                command.arg("/C");
                command
            }
            #[cfg(not(windows))]
            {
                let mut command = Command::new("/bin/sh");
                command.arg("-c");
                #[cfg(unix)]
                {
                    use std::os::unix::process::CommandExt;
                    command.arg0("sh");
                }
                command
            }
        };
        command.arg(line).status()
    }

    let command = args.nth(1);
    match command.get() {
        None | Some(Value::Nil) => {
            let shell_exists = system("exit 0")
                .map(|status| status.success())
                .unwrap_or_default();
            Ok(Action::Return(vec![shell_exists.into()]))
        }
        Some(_) => {
            let command = command.to_string()?;
            file::translate_and_return_error(gc, || {
                let status = system(command.to_os_str()?)?;
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    if let Some(signal) = status.signal() {
                        return Ok(vec![
                            Value::Nil,
                            gc.allocate_string(B("signal")).into(),
                            (signal as Integer).into(),
                        ]);
                    }
                }
                Ok(vec![
                    if status.success() {
                        true.into()
                    } else {
                        Value::Nil
                    },
                    gc.allocate_string(B("exit")).into(),
                    status
                        .code()
                        .map(|code| (code as Integer).into())
                        .unwrap_or_default(),
                ])
            })
        }
    }
}

fn os_exit<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    // TODO: use std::process::ExitCode::exit_process once stabilized
    const EXIT_SUCCESS: i32 = 0;
    const EXIT_FAILURE: i32 = 1;

    let code = args.nth(1);
    let code = match code.get() {
        Some(Value::Boolean(success)) => {
            if success {
                EXIT_SUCCESS
            } else {
                EXIT_FAILURE
            }
        }
        None => EXIT_SUCCESS,
        _ => code.to_integer()? as i32,
    };

    std::process::exit(code)
}

fn os_getenv<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let env = args
        .nth(1)
        .to_string()?
        .to_os_str()
        .ok()
        .and_then(std::env::var_os)
        .and_then(|s| Vec::from_os_string(s).ok())
        .map(|s| gc.allocate_string(s).into())
        .unwrap_or_default();
    Ok(Action::Return(vec![env]))
}

fn os_remove<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let filename = args.nth(1);
    let filename = filename.to_string()?;
    file::translate_and_return_error(gc, || {
        let path = filename.to_path()?;
        match std::fs::remove_file(path) {
            Ok(()) => (),
            Err(_) => {
                // FIXME: should try remove_dir() only when kind() is
                // - IsADirectory on Linux
                // - PermissionDenied on POSIX
                // TODO: do this once IsADirectory gets stabilized
                std::fs::remove_dir(path)?;
            }
        }
        Ok(vec![true.into()])
    })
}

fn os_rename<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let old_name = args.nth(1);
    let old_name = old_name.to_string()?;
    let new_name = args.nth(2);
    let new_name = new_name.to_string()?;

    file::translate_and_return_error(gc, || {
        let old_path = old_name.to_path()?;
        let new_path = new_name.to_path()?;
        std::fs::rename(old_path, new_path)?;
        Ok(vec![true.into()])
    })
}

fn os_setlocale<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let locale = args.nth(1);
    let locale = locale.to_string_or(B("C"))?;
    let category = args.nth(2);
    let category = category.to_string_or(B("all"))?;
    if !matches!(
        category.as_ref(),
        b"all" | b"collate" | b"ctype" | b"monetary" | b"numeric" | b"time",
    ) {
        return Err(ErrorKind::ArgumentError {
            nth: 2,
            message: "invalid option",
        });
    }

    Ok(Action::Return(vec![
        if matches!(locale.as_ref(), b"C" | b"") {
            gc.allocate_string(B("C")).into()
        } else {
            Value::Nil
        },
    ]))
}

fn os_time<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    fn get_field<'gc, T, D>(
        gc: &'gc GcContext,
        table: &Table<'gc>,
        field: &[u8],
        default: D,
    ) -> Result<T, ErrorKind>
    where
        T: TryFrom<Integer>,
        D: Into<Option<T>>,
    {
        let value = table.get_field(gc.allocate_string(field));
        let field = field.as_bstr();
        if value.is_nil() {
            default
                .into()
                .ok_or_else(|| ErrorKind::Other(format!("field '{field}' missing in date table")))
        } else if let Some(i) = value.to_integer() {
            i.try_into()
                .map_err(|_| ErrorKind::Other(format!("field '{field}' is out-of-bound")))
        } else {
            Err(ErrorKind::Other(format!(
                "field '{}' is not an integer",
                field
            )))
        }
    }

    let table = args.nth(1);
    if !table.is_present() {
        return Ok(Action::Return(vec![Utc::now().timestamp().into()]));
    }

    let table = table.as_table()?;
    let mut table = table.borrow_mut(gc);
    let datetime = Local
        .ymd_opt(
            get_field(gc, &table, b"year", None)?,
            get_field(gc, &table, b"month", None)?,
            get_field(gc, &table, b"day", None)?,
        )
        .and_hms_opt(
            get_field(gc, &table, b"hour", 12)?,
            get_field(gc, &table, b"min", 0)?,
            get_field(gc, &table, b"sec", 0)?,
        )
        .earliest()
        .ok_or_else(|| {
            ErrorKind::other("time result cannot be represented in this installation")
        })?;
    set_datetime_to_table(gc, &mut table, &datetime);

    Ok(Action::Return(vec![datetime.timestamp().into()]))
}

fn set_datetime_to_table<'gc, Tz: TimeZone>(
    gc: &'gc GcContext,
    table: &mut Table<'gc>,
    datetime: &DateTime<Tz>,
) {
    table.set_field(gc.allocate_string(B("year")), datetime.year() as Integer);
    table.set_field(gc.allocate_string(B("month")), datetime.month() as Integer);
    table.set_field(gc.allocate_string(B("day")), datetime.day() as Integer);
    table.set_field(gc.allocate_string(B("hour")), datetime.hour() as Integer);
    table.set_field(gc.allocate_string(B("min")), datetime.minute() as Integer);
    table.set_field(gc.allocate_string(B("sec")), datetime.second() as Integer);
    table.set_field(gc.allocate_string(B("yday")), datetime.ordinal() as Integer);
    table.set_field(
        gc.allocate_string(B("wday")),
        datetime.weekday().number_from_sunday() as Integer,
    );
}
