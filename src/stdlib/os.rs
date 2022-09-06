use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Integer, LuaThread, NativeFunction, StackWindow, Table, Value},
};
use bstr::{ByteSlice, ByteVec, B};
use chrono::{DateTime, Datelike, Local, NaiveDateTime, TimeZone, Timelike, Utc};

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("clock")), NativeFunction::new(clock));
    table.set_field(gc.allocate_string(B("date")), NativeFunction::new(date));
    table.set_field(
        gc.allocate_string(B("difftime")),
        NativeFunction::new(difftime),
    );
    table.set_field(gc.allocate_string(B("exit")), NativeFunction::new(exit));
    table.set_field(gc.allocate_string(B("getenv")), NativeFunction::new(getenv));
    table.set_field(gc.allocate_string(B("time")), NativeFunction::new(time));
    vm.globals()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(B("os")), gc.allocate_cell(table));
}

fn clock<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    thread.borrow_mut(gc).stack_mut(&window)[0] = cpu_time::ProcessTime::now()
        .as_duration()
        .as_secs_f64()
        .into();
    Ok(1)
}

fn date<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let format = stack.arg(0);
    let format = format.to_string_or(B("%c"))?;

    let time = stack.arg(1).to_integer_or_else(|| Utc::now().timestamp())?;
    if NaiveDateTime::from_timestamp_opt(time, 0).is_none() {
        return Err(ErrorKind::ArgumentError {
            nth: 1,
            message: "time out-of-bounds",
        });
    }

    let (format, is_utc) = if let Some(format) = format.strip_prefix(b"!".as_ref()) {
        (format, true)
    } else {
        (format.as_ref(), false)
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
        stack[0] = gc.allocate_cell(table).into();
        return Ok(1);
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
        return Err(ErrorKind::ExplicitError(format!(
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
    stack[0] = gc
        .allocate_string(formatted.to_string().into_bytes())
        .into();
    Ok(1)
}

fn difftime<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack[0] = (stack.arg(0).to_number()? - stack.arg(1).to_number()?).into();
    Ok(1)
}

fn exit<'gc>(
    _: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    // TODO: use std::process::ExitCode::exit_process once stabilized
    const EXIT_SUCCESS: i32 = 0;
    const EXIT_FAILURE: i32 = 1;

    let code = thread.borrow().stack(&window).arg(0);
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

fn getenv<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack[0] = stack
        .arg(0)
        .to_string()?
        .to_os_str()
        .ok()
        .and_then(std::env::var_os)
        .and_then(|s| Vec::from_os_string(s).ok())
        .map(|s| gc.allocate_string(s).into())
        .unwrap_or_default();
    Ok(1)
}

fn time<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
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
            default.into().ok_or_else(|| {
                ErrorKind::ExplicitError(format!("field '{}' missing in date table", field))
            })
        } else if let Some(i) = value.to_integer() {
            i.try_into()
                .map_err(|_| ErrorKind::ExplicitError(format!("field '{}' is out-of-bound", field)))
        } else {
            Err(ErrorKind::ExplicitError(format!(
                "field '{}' is not an integer",
                field
            )))
        }
    }

    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let table = stack.arg(0);
    if table.get().is_none() {
        stack[0] = Utc::now().timestamp().into();
        return Ok(1);
    }

    let mut table = table.borrow_as_table_mut(gc)?;
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
            ErrorKind::ExplicitError(
                "time result cannot be represented in this installation".to_owned(),
            )
        })?;
    set_datetime_to_table(gc, &mut table, &datetime);

    stack[0] = datetime.timestamp().into();
    Ok(1)
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
