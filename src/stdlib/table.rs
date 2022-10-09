use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, ErrorKind, Vm},
    types::{Integer, Table, Value},
};
use bstr::B;

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("concat"), table_concat),
            (B("insert"), table_insert),
            (B("move"), table_move),
            (B("pack"), table_pack),
            (B("remove"), table_remove),
            (B("unpack"), table_unpack),
        ],
    );
    gc.allocate_cell(table)
}

fn table_concat<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let table = table.borrow();
    let sep = args.nth(2);
    let sep = sep.to_string_or(B(""))?;
    let i = args.nth(3).to_integer_or(1)?;
    let j = args.nth(4).to_integer_or_else(|| table.lua_len())?;

    let mut strings = Vec::new();
    for index in i..=j {
        let value = table.get(index);
        if let Some(string) = value.to_string() {
            strings.push(string.to_vec());
        } else {
            return Err(ErrorKind::Other(format!(
                "invalid value ({}) at index {} in table for 'concat'",
                value.ty().name(),
                index
            )));
        }
    }

    let concatenated = strings.join(sep.as_ref());
    Ok(Action::Return(vec![gc
        .allocate_string(concatenated)
        .into()]))
}

fn table_insert<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let mut table = table.borrow_mut(gc);
    let end = table.lua_len().wrapping_add(1);

    match *args.without_callee() {
        [_, value] => table.set(end, value)?,
        [_, _, value] => {
            let pos = args.nth(2).to_integer()?;
            if pos < 1 || end < pos {
                return Err(ErrorKind::ArgumentError {
                    nth: 2,
                    message: "position out of bounds",
                });
            }
            for i in (pos + 1..=end).rev() {
                let v = table.get(i - 1);
                table.set(i, v)?;
            }
            table.set(pos, value)?;
        }
        _ => return Err(ErrorKind::other("wrong number of arguments to 'insert'")),
    };
    Ok(Action::Return(Vec::new()))
}

fn table_move<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let f = args.nth(2).to_integer()?;
    let e = args.nth(3).to_integer()?;
    let t = args.nth(4).to_integer()?;
    let a1 = args.nth(1).as_table()?;
    let a2 = args.nth(5);
    let a2 = if a2.is_present() { a2.as_table()? } else { a1 };

    if f > e {
        return Ok(Action::Return(vec![a2.into()]));
    }

    let n = e
        .checked_sub(f)
        .and_then(|x| x.checked_add(1))
        .ok_or(ErrorKind::ArgumentError {
            nth: 3,
            message: "too many elements to move",
        })?;
    if t.checked_add(n - 1).is_none() {
        return Err(ErrorKind::ArgumentError {
            nth: 4,
            message: "destination wrap around",
        });
    }

    if GcCell::ptr_eq(&a1, &a2) {
        let mut table = a1.borrow_mut(gc);
        if t <= f || e < t {
            for i in 0..n {
                let value = table.get(f + i);
                table.set(t + i, value)?;
            }
        } else {
            for i in (0..n).rev() {
                let value = table.get(f + i);
                table.set(t + i, value)?;
            }
        }
    } else {
        let a1 = a1.borrow();
        let mut a2 = a2.borrow_mut(gc);
        for i in 0..n {
            a2.set(t + i, a1.get(f + i))?;
        }
    }
    Ok(Action::Return(vec![a2.into()]))
}

fn table_pack<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let mut table = Table::from(args.without_callee().to_vec());
    table.set_field(
        gc.allocate_string(B("n")),
        args.without_callee().len() as Integer,
    );
    Ok(Action::Return(vec![gc.allocate_cell(table).into()]))
}

fn table_remove<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let mut table = table.borrow_mut(gc);
    let len = table.lua_len();

    let pos = args.nth(2).to_integer_or(len)?;
    if pos != len && (pos < 1 || len + 1 < pos) {
        return Err(ErrorKind::ArgumentError {
            nth: 2,
            message: "position out of bounds",
        });
    }

    let removed = table.get(pos);
    for i in pos..len {
        let value = table.get(i + 1);
        table.set(i, value)?;
    }
    table.set(pos.max(len), Value::Nil)?;
    Ok(Action::Return(vec![removed]))
}

fn table_unpack<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1).as_table()?;
    let table = table.borrow();
    let start = args.nth(2).to_integer_or(1)?;
    let end = args.nth(3).to_integer_or_else(|| table.lua_len())?;

    if start > end {
        return Ok(Action::Return(Vec::new()));
    }

    match end.checked_sub(start) {
        Some(n) if n < i32::MAX as Integer => (),
        _ => return Err(ErrorKind::other("too many results to unpack")),
    }

    Ok(Action::Return(
        (start..=end).map(|key| table.get(key)).collect(),
    ))
}
