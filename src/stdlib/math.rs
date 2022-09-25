use super::helpers::ArgumentsExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, ErrorKind, Vm},
    stdlib::helpers::set_functions_to_table,
    types::{Integer, NativeClosure, Number, Table, Value},
};
use bstr::B;
use rand::{Rng, RngCore, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use std::{cell::RefCell, ops::DerefMut, rc::Rc, time::SystemTime};

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
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
    let seed2 = rand::thread_rng().gen_range(1..=i64::MAX);

    let rng = rng_from_seeds(seed1(), seed2);
    let rng = Rc::new(RefCell::new(rng));
    {
        let rng = rng.clone();
        table.set_field(
            gc.allocate_string(B("random")),
            gc.allocate(NativeClosure::new(move |_, _, args| {
                let mut rng = rng.borrow_mut();
                let result = match args.without_callee().len() {
                    0 => rng.gen::<Number>().into(),
                    1 => {
                        let upper = args.nth(1).to_integer()?;
                        if upper == 0 {
                            rng.gen::<Integer>().into()
                        } else {
                            random_in_range(rng.deref_mut(), 1, upper).into()
                        }
                    }
                    2 => {
                        let lower = args.nth(1).to_integer()?;
                        let upper = args.nth(2).to_integer()?;
                        random_in_range(rng.deref_mut(), lower, upper).into()
                    }
                    _ => return Err(ErrorKind::other("wrong number of arguments")),
                };
                Ok(Action::Return(vec![result]))
            })),
        );
    }
    table.set_field(
        gc.allocate_string(B("randomseed")),
        gc.allocate(NativeClosure::new(move |_, _, args| {
            let (x, y) = if args.without_callee().is_empty() {
                (seed1(), seed2)
            } else {
                let x = args.nth(1).to_integer()?;
                let y = args.nth(2).to_integer_or(0)?;
                (x, y)
            };
            *rng.borrow_mut() = rng_from_seeds(x, y);

            Ok(Action::Return(vec![x.into(), y.into()]))
        })),
    );

    gc.allocate_cell(table)
}

fn math_abs<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.abs().into()
    } else {
        arg.to_number()?.abs().into()
    };
    Ok(Action::Return(vec![result]))
}

fn math_acos<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::acos)
}

fn math_asin<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::asin)
}

fn math_atan<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let y = args.nth(1).to_number()?;
    let x = args.nth(2);
    let result = if x.is_present() {
        y.atan2(x.to_number()?)
    } else {
        y.atan()
    };
    Ok(Action::Return(vec![result.into()]))
}

fn math_ceil<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let ceil = arg.to_number()?.ceil();
        number_to_value(ceil)
    };
    Ok(Action::Return(vec![result]))
}

fn math_cos<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::cos)
}

fn math_deg<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::to_degrees)
}

fn math_exp<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::exp)
}

fn math_floor<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let arg = args.nth(1);
    let result = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let floor = arg.to_number()?.floor();
        number_to_value(floor)
    };
    Ok(Action::Return(vec![result]))
}

fn math_fmod<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let x = args.nth(1);
    let y = args.nth(2);
    let result = if let (Value::Integer(x), Value::Integer(y)) = (x.as_value()?, y.as_value()?) {
        if y == 0 {
            return Err(ErrorKind::ArgumentError {
                nth: 2,
                message: "zero",
            });
        }
        (x % y).into()
    } else {
        (x.to_number()? % y.to_number()?).into()
    };
    Ok(Action::Return(vec![result]))
}

fn math_log<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let base = args.nth(2);
    let result = if base.is_present() {
        x.log(base.to_number()?)
    } else {
        x.ln()
    };
    Ok(Action::Return(vec![result.into()]))
}

fn math_modf<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let x = args.nth(1);
    let (trunc, fract) = if let Value::Integer(x) = x.as_value()? {
        (x.into(), 0.0.into())
    } else {
        let x = x.to_number()?;
        (number_to_value(x.trunc()), x.fract().into())
    };
    Ok(Action::Return(vec![trunc, fract]))
}

fn math_rad<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::to_radians)
}

fn math_sin<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::sin)
}

fn math_sqrt<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::sqrt)
}

fn math_tan<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::tan)
}

fn math_tointeger<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let result = args
        .nth(1)
        .as_value()?
        .to_integer()
        .map(|i| i.into())
        .unwrap_or_default();
    Ok(Action::Return(vec![result]))
}

fn math_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let result = match args.nth(1).as_value()? {
        Value::Integer(_) => gc.allocate_string(B("integer")).into(),
        Value::Number(_) => gc.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    Ok(Action::Return(vec![result]))
}

fn math_ult<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let m = args.nth(1).to_integer()?;
    let n = args.nth(2).to_integer()?;
    Ok(Action::Return(vec![((m as u64) < (n as u64)).into()]))
}

fn math_cosh<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::cosh)
}

fn math_log10<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::log10)
}

fn math_pow<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let x = args.nth(1).to_number()?;
    let y = args.nth(2).to_number()?;
    Ok(Action::Return(vec![Number::powf(x, y).into()]))
}

fn math_sinh<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::sinh)
}

fn math_tanh<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    unary_func(vm, args, Number::tanh)
}

fn unary_func<'gc, F>(
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
    f: F,
) -> Result<Action<'gc>, ErrorKind>
where
    F: Fn(Number) -> Number,
{
    let x = args.nth(1).to_number()?;
    Ok(Action::Return(vec![f(x).into()]))
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

fn random_in_range<R: Rng>(rng: &mut R, lower: Integer, upper: Integer) -> Integer {
    fn project<R: Rng>(rng: &mut R, range: Integer) -> Integer {
        if range & (range + 1) == 0 {
            return rng.gen::<Integer>() & range;
        }

        let mut lim = range;
        lim |= lim >> 1;
        lim |= lim >> 2;
        lim |= lim >> 4;
        lim |= lim >> 8;
        lim |= lim >> 16;
        lim |= lim >> 32;

        loop {
            let rand = rng.gen::<Integer>() & lim;
            if rand <= range {
                return rand;
            }
        }
    }
    lower + project(rng, upper - lower)
}

fn number_to_value<'gc>(x: Number) -> Value<'gc> {
    let int = x as Integer;
    if x == int as Number {
        Value::Integer(int)
    } else {
        Value::Number(x)
    }
}
