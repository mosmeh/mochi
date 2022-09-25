use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Action, Integer, Table, Value},
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
    let table = args.nth(1);
    let table = table.borrow_as_table()?;
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
    let table = args.nth(1);
    let mut table = table.borrow_as_table_mut(gc)?;
    let len = table.lua_len();

    match args.without_callee().len() {
        2 => table.set(len + 1, args.without_callee()[1])?,
        3 => {
            let pos = args.nth(2).to_integer()?;
            if pos > len + 1 {
                return Err(ErrorKind::ArgumentError {
                    nth: 2,
                    message: "position out of bounds",
                });
            }
            for i in (1..=len).rev() {
                let value = table.get(i);
                table.set(i + 1, value)?;
            }
            table.set(pos, args.without_callee()[2])?;
        }
        _ => return Err(ErrorKind::other("wrong number of arguments to 'insert'")),
    };
    Ok(Action::Return(Vec::new()))
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
    let table = args.nth(1);
    let mut table = table.borrow_as_table_mut(gc)?;
    let len = table.lua_len();

    let pos = args.nth(2).to_integer_or(len)?;
    if pos > len + 1 {
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
    table.set(len, Value::Nil)?;
    Ok(Action::Return(vec![removed]))
}

fn table_unpack<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let table = args.nth(1);
    let table = table.borrow_as_table()?;
    let start = args.nth(2).to_integer_or(1)?;
    let end = args.nth(3).to_integer_or_else(|| table.lua_len())?;

    Ok(Action::Return(
        (start..=end).map(|key| table.get(key)).collect(),
    ))
}
