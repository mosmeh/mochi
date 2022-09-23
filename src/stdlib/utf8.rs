use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Action, Integer, LuaThread, NativeFunction, StackWindow, Table, Value},
};
use bstr::B;

const MAXUTF: u32 = 0x7fffffff;

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
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

fn utf8_char<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let mut string = Vec::new();
    for i in 1..stack.len() {
        let mut code = stack.arg(i).to_integer()? as u32;
        if code > MAXUTF {
            return Err(ErrorKind::ArgumentError {
                nth: i,
                message: "value out of range",
            });
        }

        if code < 0x80 {
            string.push(code as u8);
            continue;
        }

        const BUF_LEN: usize = 6;
        let mut buf = [0; BUF_LEN];
        let mut len = 1;
        let mut mfb = 0x3f;
        loop {
            buf[BUF_LEN - len] = 0x80 | (code as u8 & 0x3f);
            len += 1;
            code >>= 6;
            mfb >>= 1;
            if code <= mfb as u32 {
                break;
            }
        }
        buf[BUF_LEN - len] = (!mfb << 1) | code as u8;
        string.extend_from_slice(&buf[BUF_LEN - len..]);
    }

    Ok(Action::Return(vec![gc.allocate_string(string).into()]))
}

fn utf8_codes<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    fn iterate<'gc>(
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
        lax: bool,
    ) -> Result<Action<'gc>, ErrorKind> {
        let thread = thread.borrow();
        let stack = thread.stack(&window);

        let s = stack.arg(1);
        let s = s.to_string()?;
        let n = stack.arg(2).to_integer()?;

        let mut i = n as usize;
        loop {
            if let Some(ch) = s.get(i) {
                if !is_continuation_byte(*ch) {
                    break;
                }
                i += 1;
            } else {
                return Ok(Action::Return(Vec::new()));
            }
        }
        if let Some((ch, len)) = decode_utf8(&s[i..]) {
            if (lax || is_valid_unicode_char(ch))
                && !s
                    .get(i + len)
                    .copied()
                    .map(is_continuation_byte)
                    .unwrap_or_default()
            {
                return Ok(Action::Return(vec![
                    ((i + 1) as Integer).into(),
                    (ch as Integer).into(),
                ]));
            }
        }
        Err(ErrorKind::other("invalid UTF-8 code"))
    }

    fn iterate_lax<'gc>(
        _: &'gc GcContext,
        _: &mut Vm<'gc>,
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
    ) -> Result<Action<'gc>, ErrorKind> {
        iterate(thread, window, true)
    }

    fn iterate_strict<'gc>(
        _: &'gc GcContext,
        _: &mut Vm<'gc>,
        thread: GcCell<LuaThread<'gc>>,
        window: StackWindow,
    ) -> Result<Action<'gc>, ErrorKind> {
        iterate(thread, window, false)
    }

    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let s = stack.arg(1);
    if s.to_string()?
        .first()
        .copied()
        .map(is_continuation_byte)
        .unwrap_or_default()
    {
        return Err(ErrorKind::ArgumentError {
            nth: 1,
            message: "invalid UTF-8 code",
        });
    }

    let lax = stack
        .arg(2)
        .get()
        .map(|value| value.to_boolean())
        .unwrap_or_default();

    Ok(Action::Return(vec![
        NativeFunction::new(if lax { iterate_lax } else { iterate_strict }).into(),
        s.as_value()?,
        0.into(),
    ]))
}

fn utf8_codepoint<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let s = stack.arg(1);
    let s = s.to_string()?;
    let i = stack.arg(2).to_integer_or(1)?;
    let j = stack.arg(3).to_integer_or(i)?;
    let lax = stack
        .arg(4)
        .get()
        .map(|value| value.to_boolean())
        .unwrap_or_default();
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
        if let Some((ch, len)) = decode_utf8(slice) {
            if lax || is_valid_unicode_char(ch) {
                slice = &slice[len..];
                pos += len;
                values.push((ch as Integer).into());
                continue;
            }
        }
        return Err(ErrorKind::other("invalid UTF-8 code"));
    }

    Ok(Action::Return(values))
}

fn utf8_len<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let s = stack.arg(1);
    let s = s.to_string()?;
    let i = stack.arg(2).to_integer_or(1)?;
    let j = stack.arg(3).to_integer_or(-1)?;
    let lax = stack
        .arg(4)
        .get()
        .map(|value| value.to_boolean())
        .unwrap_or_default();
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
        if let Some((ch, len)) = decode_utf8(slice) {
            if lax || is_valid_unicode_char(ch) {
                slice = &slice[len..];
                pos += len;
                n += 1;
                continue;
            }
        }
        return Ok(Action::Return(vec![
            Value::Nil,
            ((pos + 1) as Integer).into(),
        ]));
    }

    Ok(Action::Return(vec![n.into()]))
}

fn utf8_offset<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let s = stack.arg(1);
    let s = s.to_string()?;
    let mut n = stack.arg(2).to_integer()?;
    let i = stack
        .arg(3)
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
            if !is_continuation_byte(*b) {
                break;
            }
            pos -= 1;
        }
        return Ok(Action::Return(vec![((pos + 1) as Integer).into()]));
    }

    if let Some(b) = s.get(pos) {
        if is_continuation_byte(*b) {
            return Err(ErrorKind::other("initial position is a continuation byte"));
        }
    }
    if n < 0 {
        while n < 0 && pos > 0 {
            loop {
                pos -= 1;
                if pos == 0 {
                    break;
                }
                if let Some(b) = s.get(pos) {
                    if !is_continuation_byte(*b) {
                        break;
                    }
                } else {
                    break;
                }
            }
            n += 1;
        }
    } else {
        n -= 1;
        while n > 0 && pos < s.len() {
            loop {
                pos += 1;
                if let Some(b) = s.get(pos) {
                    if !is_continuation_byte(*b) {
                        break;
                    }
                } else {
                    break;
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
    Ok(Action::Return(vec![result]))
}

fn is_valid_unicode_char(i: u32) -> bool {
    char::from_u32(i).is_some()
}

fn is_continuation_byte(b: u8) -> bool {
    b & 0xc0 == 0x80
}

fn decode_utf8<B: AsRef<[u8]>>(bytes: B) -> Option<(u32, usize)> {
    let bytes = bytes.as_ref();
    let mut ch = match bytes.first() {
        None => return None,
        Some(&ch) if ch < 0x80 => return Some((ch as u32, 1)),
        Some(&ch) => ch as u32,
    };

    const LIMITS: [u32; 6] = [!0, 0x80, 0x800, 0x10000, 0x200000, 0x4000000];
    let mut count = 0;
    let mut res = 0;
    while ch & 0x40 != 0 {
        count += 1;
        let cc = *bytes.get(count)?;
        if !is_continuation_byte(cc) {
            return None;
        }
        res = (res << 6) | ((cc & 0x3f) as u32);
        ch <<= 1;
    }
    res |= (ch & 0x7f) << (count * 5);
    (res <= MAXUTF && res >= LIMITS[count]).then_some((res, count + 1))
}

fn relative_to_absolute_pos(pos: Integer, len: usize) -> usize {
    if pos >= 0 {
        pos as usize
    } else {
        (len as Integer + pos + 1).try_into().unwrap_or_default()
    }
}
