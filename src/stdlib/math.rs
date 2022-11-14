use super::helpers::ArgumentsExt;
use crate::{
    gc::{GcCell, GcContext, RootSet},
    number_is_valid_integer,
    runtime::{ErrorKind, Vm},
    stdlib::helpers::set_functions_to_table,
    types::{Integer, NativeClosure, Number, Table, Value},
};
use bstr::B;
use rand::{rngs::OsRng, Rng, RngCore, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use std::{cell::RefCell, ops::DerefMut, rc::Rc, time::SystemTime};

pub fn load<'gc, 'a>(
    gc: &'a GcContext<'gc>,
    _: &mut Vm<'gc, '_>,
) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("abs"), math_abs),
            (B("acos"), math_acos),
            (B("asin"), math_asin),
            (B("atan"), math_atan),
            (B("ceil"), math_ceil),
            (B("cos"), math_cos),
            (B("deg"), math_deg),
            (B("exp"), math_exp),
            (B("floor"), math_floor),
            (B("fmod"), math_fmod),
            (B("log"), math_log),
            (B("modf"), math_modf),
            (B("rad"), math_rad),
            (B("sin"), math_sin),
            (B("sqrt"), math_sqrt),
            (B("tan"), math_tan),
            (B("tointeger"), math_tointeger),
            (B("type"), math_type),
            (B("ult"), math_ult),
            // LUA_COMPAT_MATHLIB
            (B("atan2"), math_atan),
            (B("cosh"), math_cosh),
            (B("frexp"), math_frexp),
            (B("ldexp"), math_ldexp),
            (B("log10"), math_log10),
            (B("pow"), math_pow),
            (B("sinh"), math_sinh),
            (B("tanh"), math_tanh),
        ],
    );
    table.set_field(gc.allocate_string(B("huge")), Number::INFINITY);
    table.set_field(gc.allocate_string(B("maxinteger")), Integer::MAX);
    table.set_field(gc.allocate_string(B("mininteger")), Integer::MIN);
    table.set_field(gc.allocate_string(B("pi")), std::f64::consts::PI);

    fn seed1() -> i64 {
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64
    }
    let seed2 = OsRng.gen();

    let rng = rng_from_seeds(seed1(), seed2);
    let rng = Rc::new(RefCell::new(rng));
    {
        let rng = rng.clone();
        table.set_field(
            gc.allocate_string(B("random")),
            gc.allocate(NativeClosure::new(move |_, _, _, args| {
                let mut rng = rng.borrow_mut();
                let (lower, upper) = match args.without_callee().len() {
                    0 => return Ok(vec![rng.gen::<Number>().into()]),
                    1 => {
                        let upper = args.nth(1).to_integer()?;
                        if upper == 0 {
                            return Ok(vec![rng.gen::<Integer>().into()]);
                        } else {
                            (1, upper)
                        }
                    }
                    2 => {
                        let lower = args.nth(1).to_integer()?;
                        let upper = args.nth(2).to_integer()?;
                        (lower, upper)
                    }
                    _ => return Err(ErrorKind::other("wrong number of arguments")),
                };
                if lower <= upper {
                    let random = random_in_range(rng.deref_mut(), lower as u64, upper as u64);
                    Ok(vec![(random as Integer).into()])
                } else {
                    Err(ErrorKind::ArgumentError {
                        nth: 1,
                        message: "interval is empty",
                    })
                }
            })),
        );
    }
    table.set_field(
        gc.allocate_string(B("randomseed")),
        gc.allocate(NativeClosure::new(move |_, _, _, args| {
            let (x, y) = if args.without_callee().is_empty() {
                (seed1(), seed2)
            } else {
                let x = args.nth(1).to_integer()?;
                let y = args.nth(2).to_integer_or(0)?;
                (x, y)
            };
            *rng.borrow_mut() = rng_from_seeds(x, y);

            Ok(vec![x.into(), y.into()])
        })),
    );

    gc.allocate_cell(table)
}

fn math_abs<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.wrapping_abs().into()
    } else {
        arg.to_number()?.abs().into()
    };
    Ok(vec![result])
}

fn math_acos<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::acos)
}

fn math_asin<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::asin)
}

fn math_atan<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let y = args.nth(1).to_number()?;
    let x = args.nth(2);
    let result = if x.is_present() {
        y.atan2(x.to_number()?)
    } else {
        y.atan()
    };
    Ok(vec![result.into()])
}

fn math_ceil<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let ceil = arg.to_number()?.ceil();
        number_to_value(ceil)
    };
    Ok(vec![result])
}

fn math_cos<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::cos)
}

fn math_deg<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::to_degrees)
}

fn math_exp<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::exp)
}

fn math_floor<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let floor = arg.to_number()?.floor();
        number_to_value(floor)
    };
    Ok(vec![result])
}

fn math_fmod<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1);
    let y = args.nth(2);
    let result = match (x.as_value()?, y.as_value()?) {
        (Value::Integer(_), Value::Integer(0)) => {
            return Err(ErrorKind::ArgumentError {
                nth: 2,
                message: "zero",
            })
        }
        (Value::Integer(x), Value::Integer(y)) => (x % y).into(),
        _ => (x.to_number()? % y.to_number()?).into(),
    };
    Ok(vec![result])
}

fn math_log<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let base = args.nth(2);
    let result = if base.is_present() {
        x.log(base.to_number()?)
    } else {
        x.ln()
    };
    Ok(vec![result.into()])
}

fn math_modf<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1);
    let (trunc, fract) = if let Value::Integer(x) = x.as_value()? {
        (x.into(), 0.0.into())
    } else {
        let x = x.to_number()?;
        let trunc = number_to_value(x.trunc());
        let fract = if x.is_infinite() { 0.0 } else { x.fract() };
        (trunc, fract.into())
    };
    Ok(vec![trunc, fract])
}

fn math_rad<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::to_radians)
}

fn math_sin<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::sin)
}

fn math_sqrt<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::sqrt)
}

fn math_tan<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::tan)
}

fn math_tointeger<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let result = args
        .nth(1)
        .as_value()?
        .to_integer()
        .map(|i| i.into())
        .unwrap_or_default();
    Ok(vec![result])
}

fn math_type<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let result = match args.nth(1).as_value()? {
        Value::Integer(_) => gc.allocate_string(B("integer")).into(),
        Value::Number(_) => gc.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    Ok(vec![result])
}

fn math_ult<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let m = args.nth(1).to_integer()?;
    let n = args.nth(2).to_integer()?;
    Ok(vec![((m as u64) < (n as u64)).into()])
}

fn math_cosh<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::cosh)
}

fn math_frexp<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let (fr, exp) = crate::math::frexp(x);
    Ok(vec![fr.into(), (exp as Integer).into()])
}

fn math_ldexp<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let exp = args.nth(2).to_integer()?;
    Ok(vec![crate::math::ldexp(x, exp as i32).into()])
}

fn math_log10<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::log10)
}

fn math_pow<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let y = args.nth(2).to_number()?;
    Ok(vec![Number::powf(x, y).into()])
}

fn math_sinh<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::sinh)
}

fn math_tanh<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    unary_func(args, Number::tanh)
}

fn unary_func<'gc, 'a, F>(args: &[Value<'gc, 'a>], f: F) -> Result<Vec<Value<'gc, 'a>>, ErrorKind>
where
    F: Fn(Number) -> Number,
{
    let x = args.nth(1).to_number()?;
    Ok(vec![f(x).into()])
}

fn number_to_value<'gc, 'a>(x: Number) -> Value<'gc, 'a> {
    if number_is_valid_integer(x) {
        Value::Integer(x as Integer)
    } else {
        Value::Number(x)
    }
}

fn rng_from_seeds(n1: i64, n2: i64) -> Xoshiro256StarStar {
    let mut seed = [0u8; 32];
    seed[..8].copy_from_slice(&n1.to_le_bytes());
    seed[8..16].copy_from_slice(&0xffi64.to_le_bytes());
    seed[16..24].copy_from_slice(&n2.to_le_bytes());
    let mut rng = Xoshiro256StarStar::from_seed(seed);
    for _ in 0..16 {
        rng.next_u64();
    }
    rng
}

fn random_in_range<R: Rng>(rng: &mut R, lower: u64, upper: u64) -> u64 {
    fn project<R: Rng>(rng: &mut R, range: u64) -> u64 {
        if range & (range.wrapping_add(1)) == 0 {
            return rng.gen::<u64>() & range;
        }

        let mut mask = range;
        mask |= mask >> 1;
        mask |= mask >> 2;
        mask |= mask >> 4;
        mask |= mask >> 8;
        mask |= mask >> 16;
        mask |= mask >> 32;

        loop {
            let rand = rng.gen::<u64>() & mask;
            if rand <= range {
                return rand;
            }
        }
    }
    lower.wrapping_add(project(rng, upper.wrapping_sub(lower)))
}
