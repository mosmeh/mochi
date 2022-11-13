use crate::{
    gc::{GcCell, GcContext, RootSet},
    math,
    runtime::{ErrorKind, Vm},
    stdlib::helpers::ArgumentsExt,
    types::{Integer, Number, Value},
};
use bstr::{ByteSlice, ByteVec};
use byteorder::WriteBytesExt;

pub fn string_format<'gc>(
    gc: &'gc mut GcContext,
    _: &RootSet,
    _: GcCell<Vm>,
    args: &[Value<'gc>],
) -> Result<Vec<Value<'gc>>, ErrorKind> {
    let format_string = args.nth(1);
    let format_string = format_string.to_string()?;

    let mut format_iter = format_string.iter();
    let mut arg_nth = 1;

    let mut output = Vec::new();
    'for_spec: while let Some(&ch) = format_iter.next() {
        if ch != b'%' {
            output.push(ch);
            continue;
        }

        let mut spec = Specification::default();
        let mut raw_spec = vec![b'%'];
        let mut specifier = None;
        for &ch in format_iter.by_ref() {
            raw_spec.push(ch);
            match ch {
                b'%' => {
                    output.push(b'%');
                    continue 'for_spec;
                }
                b'.' => {
                    spec.has_precision = true;
                    spec.precision = 0;
                }
                b'-' => spec.left_justify = true,
                b'+' => spec.always_sign = true,
                b'0' if !spec.zero_pad && spec.width == 0 && !spec.has_precision => {
                    spec.zero_pad = true
                }
                b'0'..=b'9' if spec.has_precision => {
                    spec.precision *= 10;
                    spec.precision += (ch - b'0') as usize;
                }
                b'0'..=b'9' => {
                    spec.width *= 10;
                    spec.width += (ch - b'0') as usize;
                }
                b'#' => spec.alternative_form = true,
                b' ' => todo!("space modifier"),
                _ => {
                    specifier = Some(ch);
                    break;
                }
            }
            spec.has_modifier = true;
        }

        arg_nth += 1;
        let arg = args.nth(arg_nth);
        if arg.is_none() {
            return Err(ErrorKind::ArgumentError {
                nth: arg_nth,
                message: "no value",
            });
        }

        match specifier {
            Some(b'c') => {
                let byte = &[arg.to_integer()? as u8];
                spec.fmt_bytes(&mut output, byte)?
            }
            Some(b'd' | b'i') => {
                let value = arg.to_integer()?;
                if spec.width == 0 && spec.precision == 0 {
                    continue;
                }
                if spec.has_precision {
                    let mut width = spec.precision;
                    if value < 0 || spec.always_sign {
                        width += 1;
                    }
                    let mut s = Vec::new();
                    Specification {
                        zero_pad: true,
                        width,
                        ..spec
                    }
                    .fmt_display(&mut s, value)?;
                    spec.fmt_bytes(&mut output, s)?
                } else {
                    spec.fmt_display(&mut output, value)?
                }
            }
            Some(b'u') => {
                let value = arg.to_integer()? as u64;
                if spec.width == 0 && spec.precision == 0 {
                    continue;
                }
                if spec.has_precision {
                    let mut s = Vec::new();
                    Specification {
                        zero_pad: true,
                        width: spec.precision,
                        ..spec
                    }
                    .fmt_display(&mut s, value)?;
                    spec.fmt_bytes(&mut output, s)?
                } else {
                    spec.fmt_display(&mut output, value)?
                }
            }
            Some(b'o') => spec.fmt_octal(&mut output, arg.to_integer()?)?,
            Some(b'x') => spec.fmt_lower_hex(&mut output, arg.to_integer()?)?,
            Some(b'X') => {
                let mut f = Vec::new();
                spec.fmt_upper_hex(&mut f, arg.to_integer()?)?;
                f.make_ascii_uppercase();
                output.append(&mut f);
            }
            Some(b'a') => {
                if spec.has_modifier {
                    return Err(ErrorKind::other(
                        "modifiers for format '%a'/'%A' not implemented",
                    ));
                }
                let mut f = Vec::new();
                sprintf_a(&mut f, arg.to_number()?)?;
                f.make_ascii_lowercase();
                output.append(&mut f);
            }
            Some(b'A') => {
                if spec.has_modifier {
                    return Err(ErrorKind::other(
                        "modifiers for format '%a'/'%A' not implemented",
                    ));
                }
                let mut f = Vec::new();
                sprintf_a(&mut f, arg.to_number()?)?;
                f.make_ascii_uppercase();
                output.append(&mut f);
            }
            Some(b'f') => {
                let number = arg.to_number()?;
                if !number.is_finite() {
                    spec.zero_pad = false;
                }
                let mut f = Vec::new();
                spec.fmt_display(&mut f, number)?;
                f.make_ascii_lowercase();
                output.append(&mut f);
            }
            Some(b'e') => {
                let number = arg.to_number()?;
                if !number.is_finite() {
                    spec.zero_pad = false;
                }
                let mut f = Vec::new();
                spec.fmt_lower_exp(&mut f, number)?;
                f.make_ascii_lowercase();
                output.append(&mut f);
            }
            Some(b'E') => {
                let number = arg.to_number()?;
                if !number.is_finite() {
                    spec.zero_pad = false;
                }
                let mut f = Vec::new();
                spec.fmt_upper_exp(&mut f, number)?;
                f.make_ascii_uppercase();
                output.append(&mut f);
            }
            Some(b'g') => {
                let mut f = Vec::new();
                sprintf_g(&mut f, arg.to_number()?, spec.precision)?;
                f.make_ascii_lowercase();
                output.append(&mut f);
            }
            Some(b'G') => {
                let mut f = Vec::new();
                sprintf_g(&mut f, arg.to_number()?, spec.precision)?;
                f.make_ascii_uppercase();
                spec.fmt_bytes(&mut output, &f)?;
            }
            Some(b'p') => {
                if let Some(ptr) = arg.as_value()?.as_ptr() {
                    spec.fmt_ptr(&mut output, ptr)?;
                } else {
                    spec.fmt_bytes(&mut output, b"(null)")?;
                }
            }
            Some(b'q') => {
                if spec.has_modifier {
                    return Err(ErrorKind::other("specifier '%q' cannot have modifiers"));
                }
                if !fmt_literal(&mut output, arg.as_value()?)? {
                    return Err(ErrorKind::ArgumentError {
                        nth: arg_nth,
                        message: "value has no literal form",
                    });
                }
            }
            Some(b's') => {
                let s = arg.to_string()?;
                let mut s = s.as_ref();
                if !spec.has_modifier {
                    output.push_str(s);
                    continue;
                }
                if s.contains(&0) {
                    return Err(ErrorKind::ArgumentError {
                        nth: arg_nth,
                        message: "string contains zeros",
                    });
                }
                if !spec.has_precision && s.len() >= 100 {
                    output.push_str(s);
                    continue;
                }
                if spec.has_precision && s.len() > spec.precision {
                    s = &s[..spec.precision];
                }
                spec.fmt_bytes(&mut output, s)?;
            }
            _ => {
                return Err(ErrorKind::Other(format!(
                    "invalid conversion '{}' to 'format'",
                    raw_spec.as_bstr()
                )))
            }
        }
    }

    Ok(vec![gc.allocate_string(output).into()])
}

struct Specification {
    has_modifier: bool,
    has_precision: bool,
    left_justify: bool,
    zero_pad: bool,
    width: usize,
    precision: usize,
    alternative_form: bool,
    always_sign: bool,
}

impl Default for Specification {
    fn default() -> Self {
        Self {
            has_modifier: false,
            has_precision: false,
            left_justify: false,
            zero_pad: false,
            width: 0,
            precision: 6,
            alternative_form: false,
            always_sign: false,
        }
    }
}

macro_rules! fmt_with_specifier {
    ($name:ident, $tr:path, $spec:literal) => {
        fn $name<W, T>(&self, f: &mut W, value: T) -> std::io::Result<()>
        where
            W: std::io::Write,
            T: $tr,
        {
            macro_rules! f {
                ($params:literal) => {
                    write!(
                        f,
                        concat!("{value:", $params, "width$.precision$", $spec, "}"),
                        value = value,
                        width = self.width,
                        precision = self.precision
                    )
                };
            }
            match (
                self.left_justify,     // < or >
                self.always_sign,      // +
                self.alternative_form, // #
                self.zero_pad,         // 0
            ) {
                (true, true, true, true) => f!("<+#0"),
                (true, true, true, false) => f!("<+#"),
                (true, true, false, true) => f!("<+0"),
                (true, true, false, false) => f!("<+"),
                (true, false, true, true) => f!("<+#0"),
                (true, false, true, false) => f!("<#"),
                (true, false, false, true) => f!("<0"),
                (true, false, false, false) => f!("<"),
                (false, true, true, true) => f!(">+#0"),
                (false, true, true, false) => f!(">+#"),
                (false, true, false, true) => f!(">+0"),
                (false, true, false, false) => f!(">+"),
                (false, false, true, true) => f!(">#0"),
                (false, false, true, false) => f!(">#"),
                (false, false, false, true) => f!(">0"),
                (false, false, false, false) => f!(">"),
            }
        }
    };
}

impl Specification {
    fmt_with_specifier!(fmt_display, std::fmt::Display, "");
    fmt_with_specifier!(fmt_octal, std::fmt::Octal, "o");
    fmt_with_specifier!(fmt_lower_hex, std::fmt::LowerHex, "x");
    fmt_with_specifier!(fmt_upper_hex, std::fmt::UpperHex, "X");
    fmt_with_specifier!(fmt_lower_exp, std::fmt::LowerExp, "e");
    fmt_with_specifier!(fmt_upper_exp, std::fmt::UpperExp, "E");
    fmt_with_specifier!(fmt_ptr, std::fmt::Pointer, "p");

    fn fmt_bytes<W, T>(&self, f: &mut W, value: T) -> std::io::Result<()>
    where
        W: std::io::Write,
        T: AsRef<[u8]>,
    {
        let s = value.as_ref();
        if !self.left_justify {
            for _ in s.len()..self.width {
                f.write_all(b" ")?;
            }
        }
        f.write_all(s)?;
        if self.left_justify {
            for _ in s.len()..self.width {
                f.write_all(b" ")?;
            }
        }
        Ok(())
    }
}

fn fmt_literal<W: std::io::Write>(f: &mut W, value: Value) -> Result<bool, ErrorKind> {
    match value {
        Value::Nil | Value::Boolean(_) => value.fmt_bytes(f)?,
        Value::Integer(Integer::MIN) => f.write_all(b"0x8000000000000000")?,
        Value::Integer(i) => write!(f, "{i}")?,
        Value::Number(x) => match x {
            x if x == Number::INFINITY => f.write_all(b"1e9999")?,
            x if x == Number::NEG_INFINITY => f.write_all(b"-1e9999")?,
            x if x.is_nan() => f.write_all(b"(0/0)")?,
            _ => sprintf_a(f, x)?,
        },
        Value::String(s) => {
            f.write_u8(b'"')?;
            let mut iter = s.iter().peekable();
            while let Some(ch) = iter.next() {
                match *ch {
                    b'"' | b'\\' | b'\n' => {
                        f.write_all(b"\\")?;
                        f.write_u8(*ch)?
                    }
                    ch if ch.is_ascii_control() => match iter.peek() {
                        Some(next_ch) if next_ch.is_ascii_digit() => write!(f, "\\{ch:03}")?,
                        _ => write!(f, "\\{ch}")?,
                    },
                    ch => f.write_u8(ch)?,
                }
            }
            f.write_u8(b'"')?;
        }
        _ => return Ok(false),
    }
    Ok(true)
}

// sprintf("%.Pg") where P is precision
fn sprintf_g<W: std::io::Write>(f: &mut W, x: Number, precision: usize) -> std::io::Result<()> {
    if x == 0.0 {
        return f.write_all(b"0");
    }

    let log_x = x.abs().log10();
    let mut precision = precision - 1;
    if log_x < -3.0 || (precision as Number) < log_x {
        return write!(f, "{x:.precision$e}");
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
    f.write_all(s.as_bytes())
}

// sprintf("%a")
fn sprintf_a<W: std::io::Write>(f: &mut W, mut x: Number) -> std::io::Result<()> {
    fn write_digit<W: std::io::Write>(f: &mut W, frac: &mut f64) -> std::io::Result<()> {
        let digit = *frac as u8;
        f.write_u8(if digit < 10 {
            digit + b'0'
        } else {
            digit - 10 + b'a'
        })?;
        *frac -= digit as Number;
        Ok(())
    }

    match x {
        x if !x.is_finite() => return write!(f, "{x}"),
        x if x == 0.0 => return write!(f, "{x}x0p+0"), // 0 or -0
        _ if x < 0.0 => {
            f.write_all(b"-")?;
            x = -x;
        }
        _ => (),
    }

    f.write_all(b"0x")?;

    let (mut frac, mut exp) = math::frexp(x);
    if exp >= Number::MIN_EXP - 1 {
        frac *= 2.0; // [0.5, 1) -> [1, 2)
        exp -= 1;
    } else {
        // subnormal
        while exp < Number::MIN_EXP - 1 {
            frac /= 2.0;
            exp += 1;
        }
    }
    write_digit(f, &mut frac)?;

    if frac > 0.0 {
        f.write_u8(b'.')?;
        loop {
            frac *= 16.0;
            write_digit(f, &mut frac)?;
            if frac <= 0.0 {
                break;
            }
        }
    }

    write!(f, "p{exp:+}")
}
