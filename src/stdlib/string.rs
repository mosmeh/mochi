mod format;

use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    binary_chunk,
    gc::{GcCell, GcContext, RootSet},
    runtime::{ErrorKind, Metamethod, Vm},
    types::{Integer, Table, Type, Value},
};
use bstr::{ByteSlice, B};
use std::ops::Range;

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("byte"), string_byte),
            (B("char"), string_char),
            (B("dump"), string_dump),
            (B("find"), string_find),
            (B("format"), format::string_format),
            (B("len"), string_len),
            (B("lower"), string_lower),
            (B("sub"), string_sub),
            (B("rep"), string_rep),
            (B("reverse"), string_reverse),
            (B("upper"), string_upper),
        ],
    );

    let string = gc.allocate_cell(table);

    let mut metatable = Table::new();
    metatable.set_field(vm.metamethod_name(Metamethod::Index), string);
    vm.set_metatable_of_type(Type::String, gc.allocate_cell(metatable));

    string
}

fn string_byte<'gc>(
    _: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;

    let i = args.nth(2).to_integer_or(1)?;
    let j = args.nth(3).to_integer_or(i)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    Ok(if range.is_empty() {
        Vec::new()
    } else {
        s[range].iter().map(|b| (*b as Integer).into()).collect()
    })
}

fn string_char<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let len = args.without_callee().len();
    let mut bytes = Vec::with_capacity(len);
    for nth in 1..=len {
        let ch = args.nth(nth).to_integer()?;
        if let Ok(ch) = ch.try_into() {
            bytes.push(ch);
        } else {
            return Err(ErrorKind::ArgumentError {
                nth,
                message: "value out of range",
            });
        }
    }

    Ok(vec![gc.allocate_string(bytes).into()])
}

fn string_dump<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    match args.nth(1).get() {
        Some(Value::LuaClosure(closure)) => {
            let mut binary = Vec::new();
            binary_chunk::dump(gc, &mut binary, &closure.proto)?;
            Ok(vec![gc.allocate_string(binary).into()])
        }
        Some(value) if value.ty() == Type::Function => {
            Err(ErrorKind::other("unable to dump given function"))
        }
        value => Err(ErrorKind::ArgumentTypeError {
            nth: 1,
            expected_type: "function",
            got_type: value.map(|value| value.ty().name()),
        }),
    }
}

fn string_find<'gc>(
    _: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;
    let pattern = args.nth(2);
    let pattern = pattern.to_string()?;
    let init = args.nth(3).to_integer_or(1)?;
    let plain = args.nth(4).to_boolean().unwrap_or_default();

    let len = s.len();
    let start = match init {
        1.. => init - 1,
        0 => 0,
        _ if init < -(len as Integer) => 0,
        _ => len as Integer + init,
    } as usize;
    if start >= len {
        return Ok(vec![Value::Nil]);
    }

    const SPECIALS: &[u8] = b"^$*+?.([%-";
    if !plain && pattern.find_byteset(SPECIALS).is_some() {
        todo!("regex")
    }

    Ok(if let Some(pos) = &s[start..].find(&pattern) {
        let i = *pos + start;
        vec![
            ((i + 1) as Integer).into(),
            ((i + pattern.len()) as Integer).into(),
        ]
    } else {
        vec![Value::Nil]
    })
}

fn string_len<'gc>(
    _: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let len = args.nth(1).to_string()?.len() as Integer;
    Ok(vec![len.into()])
}

fn string_lower<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let lower = args.nth(1).to_string()?.to_ascii_lowercase();
    Ok(vec![gc.allocate_string(lower).into()])
}

fn string_sub<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;

    let i = args.nth(2).to_integer()?;
    let j = args.nth(3).to_integer_or(-1)?;
    let range = indices_to_range(i, j, s.len() as Integer);

    Ok(vec![gc
        .allocate_string(if range.is_empty() { b"" } else { &s[range] })
        .into()])
}

fn string_rep<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;
    let n = args.nth(2).to_integer()?;
    let sep = args.nth(3);
    let sep = sep.to_string_or(B(""))?;

    let string = if n > 0 {
        let is_too_large = match s.len().checked_add(sep.len()) {
            Some(l) if l > (Integer::MAX / n) as usize => true,
            Some(_) => false,
            None => true,
        };
        if is_too_large {
            return Err(ErrorKind::other("resulting string too large"));
        }

        let count = n as usize;
        let mut string = Vec::with_capacity(count * s.len() + (count - 1) * sep.len());
        for _ in 0..count - 1 {
            string.extend_from_slice(s.as_ref());
            string.extend_from_slice(sep.as_ref());
        }
        string.extend_from_slice(s.as_ref());
        string
    } else {
        Vec::new()
    };

    Ok(vec![gc.allocate_string(string).into()])
}

fn string_reverse<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let mut string = args.nth(1).to_string()?.to_vec();
    string.reverse();
    Ok(vec![gc.allocate_string(string).into()])
}

fn string_upper<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let upper = args.nth(1).to_string()?.to_ascii_uppercase();
    Ok(vec![gc.allocate_string(upper).into()])
}

fn indices_to_range(i: Integer, j: Integer, len: Integer) -> Range<usize> {
    let start = match i {
        1.. => i - 1,
        0 => 0,
        _ if i < -len => 0,
        _ => len + i,
    } as usize;
    let end = match j {
        _ if j > len => len,
        0.. => j,
        _ if j < -len => 0,
        _ => len + j + 1,
    } as usize;
    start..end
}
