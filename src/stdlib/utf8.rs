use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext, RootSet},
    runtime::{ErrorKind, Vm},
    string,
    types::{Integer, NativeFunction, Table, Value},
};
use bstr::B;

pub fn load<'gc, 'a>(
    gc: &'a GcContext<'gc>,
    _: &mut Vm<'gc, 'a>,
) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("char"), utf8_char),
            (B("codes"), utf8_codes),
            (B("codepoint"), utf8_codepoint),
            (B("len"), utf8_len),
            (B("offset"), utf8_offset),
        ],
    );
    table.set_field(
        gc.allocate_string(B("charpattern")),
        gc.allocate_string(B(b"[\0-\x7F\xc2-\xfd][\x80-\xbf]*")),
    );
    gc.allocate_cell(table)
}

fn utf8_char<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let mut string = Vec::new();
    for i in 1..args.len() {
        let code = args.nth(i).to_integer()? as u32;
        if !string::encode_utf8(code, &mut string) {
            return Err(ErrorKind::ArgumentError {
                nth: i,
                message: "value out of range",
            });
        }
    }
    Ok(vec![gc.allocate_string(string).into()])
}

fn utf8_codes<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    fn iterate<'gc, 'a>(
        args: &[Value<'gc, '_>],
        lax: bool,
    ) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
        let s = args.nth(1);
        let s = s.to_string()?;
        let n = args.nth(2).to_integer()?;

        let mut i = n as usize;
        loop {
            match s.get(i) {
                Some(ch) if !string::is_utf8_continuation_byte(*ch) => break,
                Some(_) => i += 1,
                None => return Ok(Vec::new()),
            }
        }
        match string::decode_utf8(&s[i..]) {
            Some((ch, len))
                if (lax || is_valid_unicode_char(ch))
                    && !s
                        .get(i + len)
                        .copied()
                        .map(string::is_utf8_continuation_byte)
                        .unwrap_or_default() =>
            {
                Ok(vec![((i + 1) as Integer).into(), (ch as Integer).into()])
            }
            _ => Err(ErrorKind::other("invalid UTF-8 code")),
        }
    }

    fn iterate_lax<'gc, 'a>(
        _: &'a mut GcContext<'gc>,
        _: &RootSet<'gc>,
        _: GcCell<'gc, '_, Vm<'gc, '_>>,
        args: &[Value<'gc, '_>],
    ) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
        iterate(args, true)
    }

    fn iterate_strict<'gc, 'a>(
        _: &'a mut GcContext<'gc>,
        _: &RootSet<'gc>,
        _: GcCell<'gc, '_, Vm<'gc, '_>>,
        args: &[Value<'gc, '_>],
    ) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
        iterate(args, false)
    }

    let s = args.nth(1);
    if s.to_string()?
        .first()
        .copied()
        .map(string::is_utf8_continuation_byte)
        .unwrap_or_default()
    {
        return Err(ErrorKind::ArgumentError {
            nth: 1,
            message: "invalid UTF-8 code",
        });
    }

    let lax = args.nth(2).to_boolean().unwrap_or_default();

    Ok(vec![
        NativeFunction::new(if lax { iterate_lax } else { iterate_strict }).into(),
        s.as_value()?,
        0.into(),
    ])
}

fn utf8_codepoint<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;
    let i = args.nth(2).to_integer_or(1)?;
    let j = args.nth(3).to_integer_or(i)?;
    let lax = args.nth(4).to_boolean().unwrap_or_default();
    if i < 1 {
        return Err(ErrorKind::ArgumentError {
            nth: 2,
            message: "out of bounds",
        });
    }
    if j > s.len() as Integer {
        return Err(ErrorKind::ArgumentError {
            nth: 3,
            message: "out of bounds",
        });
    }

    let start = relative_to_absolute_pos(i - 1, s.len());
    let end = relative_to_absolute_pos(j, s.len());
    let mut slice = &s[start..];
    let mut values = Vec::new();
    let mut pos = start;
    while pos < end {
        match string::decode_utf8(slice) {
            Some((ch, len)) if lax || is_valid_unicode_char(ch) => {
                slice = &slice[len..];
                pos += len;
                values.push((ch as Integer).into());
            }
            _ => return Err(ErrorKind::other("invalid UTF-8 code")),
        }
    }

    Ok(values)
}

fn utf8_len<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;
    let i = args.nth(2).to_integer_or(1)?;
    let j = args.nth(3).to_integer_or(-1)?;
    let lax = args.nth(4).to_boolean().unwrap_or_default();
    if i < 1 || (s.len() as Integer) + 1 < i {
        return Err(ErrorKind::ArgumentError {
            nth: 2,
            message: "initial position out of bounds",
        });
    }
    if j > s.len() as Integer {
        return Err(ErrorKind::ArgumentError {
            nth: 3,
            message: "final position out of bounds",
        });
    }

    let start = relative_to_absolute_pos(i - 1, s.len());
    let end = relative_to_absolute_pos(j, s.len());
    let mut slice = &s[start..];
    let mut n = 0 as Integer;
    let mut pos = start;
    while pos < end {
        match string::decode_utf8(slice) {
            Some((ch, len)) if lax || is_valid_unicode_char(ch) => {
                slice = &slice[len..];
                pos += len;
                n += 1;
            }
            _ => return Ok(vec![Value::Nil, ((pos + 1) as Integer).into()]),
        }
    }

    Ok(vec![n.into()])
}

fn utf8_offset<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let s = args.nth(1);
    let s = s.to_string()?;
    let mut n = args.nth(2).to_integer()?;
    let i = args
        .nth(3)
        .to_integer_or(if n >= 0 { 1 } else { s.len() as Integer + 1 })?;
    if i < 1 || (s.len() as Integer) + 1 < i {
        return Err(ErrorKind::ArgumentError {
            nth: 3,
            message: "position out of bounds",
        });
    }

    let mut pos = relative_to_absolute_pos(i - 1, s.len());
    if n == 0 {
        while let Some(b) = s.get(pos) {
            if !string::is_utf8_continuation_byte(*b) {
                break;
            }
            pos -= 1;
        }
        return Ok(vec![((pos + 1) as Integer).into()]);
    }

    match s.get(pos) {
        Some(b) if string::is_utf8_continuation_byte(*b) => {
            return Err(ErrorKind::other("initial position is a continuation byte"))
        }
        _ => (),
    }
    if n < 0 {
        while n < 0 && pos > 0 {
            loop {
                pos -= 1;
                if pos == 0 {
                    break;
                }
                match s.get(pos) {
                    Some(b) if !string::is_utf8_continuation_byte(*b) => break,
                    Some(_) => (),
                    None => break,
                }
            }
            n += 1;
        }
    } else {
        n -= 1;
        while n > 0 && pos < s.len() {
            loop {
                pos += 1;
                match s.get(pos) {
                    Some(b) if !string::is_utf8_continuation_byte(*b) => break,
                    Some(_) => (),
                    None => break,
                }
            }
            n -= 1;
        }
    }
    let result = if n == 0 {
        ((pos + 1) as Integer).into()
    } else {
        Value::Nil
    };
    Ok(vec![result])
}

fn is_valid_unicode_char(i: u32) -> bool {
    char::from_u32(i).is_some()
}

fn relative_to_absolute_pos(pos: Integer, len: usize) -> usize {
    if pos >= 0 {
        pos as usize
    } else {
        (len as Integer + pos + 1).try_into().unwrap_or_default()
    }
}
