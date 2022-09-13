use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    stdlib::helpers::set_functions_to_table,
    types::{Action, Integer, LuaThread, NativeClosure, Number, StackWindow, Table, Value},
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
            gc.allocate(NativeClosure::new(move |gc, _, thread, window| {
                let mut thread = thread.borrow_mut(gc);
                let stack = thread.stack_mut(&window);
                let mut rng = rng.borrow_mut();
                stack[0] = match stack.args().len() {
                    0 => rng.gen::<Number>().into(),
                    1 => {
                        let upper = stack.arg(0).to_integer()?;
                        if upper == 0 {
                            rng.gen::<Integer>().into()
                        } else {
                            random_in_range(rng.deref_mut(), 1, upper).into()
                        }
                    }
                    2 => {
                        let lower = stack.arg(0).to_integer()?;
                        let upper = stack.arg(1).to_integer()?;
                        random_in_range(rng.deref_mut(), lower, upper).into()
                    }
                    _ => {
                        return Err(ErrorKind::ExplicitError(
                            "wrong number of arguments".to_owned(),
                        ))
                    }
                };
                Ok(Action::Return { num_results: 1 })
            })),
        );
    }
    table.set_field(
        gc.allocate_string(B("randomseed")),
        gc.allocate(NativeClosure::new(move |gc, _, thread, mut window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack(&window);
            let (x, y) = if stack.args().is_empty() {
                (seed1(), seed2)
            } else {
                let x = stack.arg(0).to_integer()?;
                let y = stack.arg(1).to_integer_or(0)?;
                (x, y)
            };
            *rng.borrow_mut() = rng_from_seeds(x, y);

            thread.ensure_stack(&mut window, 2);
            let stack = thread.stack_mut(&window);
            stack[0] = x.into();
            stack[1] = y.into();
            Ok(Action::Return { num_results: 2 })
        })),
    );

    gc.allocate_cell(table)
}

fn math_abs<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.abs().into()
    } else {
        arg.to_number()?.abs().into()
    };
    Ok(Action::Return { num_results: 1 })
}

fn math_acos<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::acos)
}

fn math_asin<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::asin)
}

fn math_atan<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let y = stack.arg(0).to_number()?;
    let x = stack.arg(1);
    let result = if x.get().is_some() {
        y.atan2(x.to_number()?)
    } else {
        y.atan()
    };
    stack[0] = result.into();
    Ok(Action::Return { num_results: 1 })
}

fn math_ceil<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let ceil = arg.to_number()?.ceil();
        number_to_value(ceil)
    };
    Ok(Action::Return { num_results: 1 })
}

fn math_cos<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::cos)
}

fn math_deg<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::to_degrees)
}

fn math_exp<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::exp)
}

fn math_floor<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let floor = arg.to_number()?.floor();
        number_to_value(floor)
    };
    Ok(Action::Return { num_results: 1 })
}

fn math_fmod<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let x = stack.arg(0);
    let y = stack.arg(1);
    let result = if let (Value::Integer(x), Value::Integer(y)) = (x.as_value()?, y.as_value()?) {
        if y == 0 {
            return Err(ErrorKind::ArgumentError {
                nth: 1,
                message: "zero",
            });
        }
        (x % y).into()
    } else {
        (x.to_number()? % y.to_number()?).into()
    };
    stack[0] = result;
    Ok(Action::Return { num_results: 1 })
}

fn math_log<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let x = stack.arg(0).to_number()?;
    let base = stack.arg(1);
    let result = if base.get().is_some() {
        x.log(base.to_number()?)
    } else {
        x.ln()
    };
    stack[0] = result.into();
    Ok(Action::Return { num_results: 1 })
}

fn math_modf<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let x = stack.arg(0);
    let (trunc, fract) = if let Value::Integer(x) = x.as_value()? {
        (x.into(), 0.0.into())
    } else {
        let x = x.to_number()?;
        (number_to_value(x.trunc()), x.fract().into())
    };
    stack[0] = trunc;
    stack[1] = fract;
    Ok(Action::Return { num_results: 2 })
}

fn math_rad<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::to_radians)
}

fn math_sin<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::sin)
}

fn math_sqrt<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::sqrt)
}

fn math_tan<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::tan)
}

fn math_tointeger<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack[0] = stack
        .arg(0)
        .as_value()?
        .to_integer()
        .map(|i| i.into())
        .unwrap_or_default();
    Ok(Action::Return { num_results: 1 })
}

fn math_type<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let result = match stack.arg(0).as_value()? {
        Value::Integer(_) => gc.allocate_string(B("integer")).into(),
        Value::Number(_) => gc.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    stack[0] = result;
    Ok(Action::Return { num_results: 1 })
}

fn math_ult<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let m = stack.arg(0).to_integer()?;
    let n = stack.arg(1).to_integer()?;
    stack[0] = ((m as u64) < (n as u64)).into();
    Ok(Action::Return { num_results: 1 })
}

fn math_cosh<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::cosh)
}

fn math_log10<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::log10)
}

fn math_pow<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let x = stack.arg(0).to_number()?;
    let y = stack.arg(1).to_number()?;
    stack[0] = Number::powf(x, y).into();
    Ok(Action::Return { num_results: 1 })
}

fn math_sinh<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::sinh)
}

fn math_tanh<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    unary_func(gc, thread, window, Number::tanh)
}

fn unary_func<'gc, F>(
    gc: &'gc GcContext,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
    f: F,
) -> Result<Action, ErrorKind>
where
    F: Fn(Number) -> Number,
{
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    let x = stack.arg(0).to_number()?;
    stack[0] = f(x).into();
    Ok(Action::Return { num_results: 1 })
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
