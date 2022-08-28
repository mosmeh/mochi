use super::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Integer, LuaThread, NativeClosure, NativeFunction, Number, StackWindow, Table, Value},
};
use bstr::B;
use rand::{Rng, RngCore, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use std::{cell::RefCell, ops::DerefMut, rc::Rc, time::SystemTime};

pub fn create_table(gc: &GcContext) -> Table {
    let mut table = Table::new();
    table.set_field(gc.allocate_string(B("abs")), NativeFunction::new(abs));
    table.set_field(gc.allocate_string(B("acos")), NativeFunction::new(acos));
    table.set_field(gc.allocate_string(B("asin")), NativeFunction::new(asin));
    table.set_field(gc.allocate_string(B("atan")), NativeFunction::new(atan));
    table.set_field(gc.allocate_string(B("ceil")), NativeFunction::new(ceil));
    table.set_field(gc.allocate_string(B("cos")), NativeFunction::new(cos));
    table.set_field(gc.allocate_string(B("deg")), NativeFunction::new(deg));
    table.set_field(gc.allocate_string(B("exp")), NativeFunction::new(exp));
    table.set_field(gc.allocate_string(B("floor")), NativeFunction::new(floor));
    table.set_field(gc.allocate_string(B("fmod")), NativeFunction::new(fmod));
    table.set_field(gc.allocate_string(B("huge")), Number::INFINITY);
    table.set_field(gc.allocate_string(B("log")), NativeFunction::new(log));
    table.set_field(gc.allocate_string(B("maxinteger")), Integer::MAX);
    table.set_field(gc.allocate_string(B("mininteger")), Integer::MIN);
    table.set_field(gc.allocate_string(B("modf")), NativeFunction::new(modf));
    table.set_field(gc.allocate_string(B("pi")), std::f64::consts::PI as Number);
    table.set_field(gc.allocate_string(B("rad")), NativeFunction::new(rad));

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
                let stack = thread.stack_mut(window);
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
                Ok(1)
            })),
        );
    }
    table.set_field(
        gc.allocate_string(B("randomseed")),
        gc.allocate(NativeClosure::new(move |gc, _, thread, window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack(window.clone());
            let (x, y) = if stack.args().is_empty() {
                (seed1(), seed2)
            } else {
                let x = stack.arg(0).to_integer()?;
                let y = stack.arg(1).to_integer_or(0)?;
                (x, y)
            };
            *rng.borrow_mut() = rng_from_seeds(x, y);

            let window = thread.ensure_stack(window, 2);
            let stack = thread.stack_mut(window);
            stack[0] = x.into();
            stack[1] = y.into();
            Ok(2)
        })),
    );

    table.set_field(gc.allocate_string(B("sin")), NativeFunction::new(sin));
    table.set_field(gc.allocate_string(B("sqrt")), NativeFunction::new(sqrt));
    table.set_field(gc.allocate_string(B("tan")), NativeFunction::new(tan));
    table.set_field(
        gc.allocate_string(B("tointeger")),
        NativeFunction::new(tointeger),
    );
    table.set_field(gc.allocate_string(B("type")), NativeFunction::new(ty));
    table.set_field(gc.allocate_string(B("ult")), NativeFunction::new(ult));
    table
}

fn abs<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.abs().into()
    } else {
        arg.to_number()?.abs().into()
    };
    Ok(1)
}

fn acos<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.acos().into();
    Ok(1)
}

fn asin<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.asin().into();
    Ok(1)
}

fn atan<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let y = stack.arg(0).to_number()?;
    let x = stack.arg(1);
    let result = if x.get().is_some() {
        y.atan2(x.to_number()?)
    } else {
        y.atan()
    };
    stack[0] = result.into();
    Ok(1)
}

fn ceil<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let ceil = arg.to_number()?.ceil();
        number_to_value(ceil)
    };
    Ok(1)
}

fn cos<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.cos().into();
    Ok(1)
}

fn deg<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.to_degrees().into();
    Ok(1)
}

fn exp<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.exp().into();
    Ok(1)
}

fn floor<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let arg = stack.arg(0);
    stack[0] = if let Some(Value::Integer(x)) = arg.get() {
        x.into()
    } else {
        let floor = arg.to_number()?.floor();
        number_to_value(floor)
    };
    Ok(1)
}

fn fmod<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let x = stack.arg(0);
    let y = stack.arg(1);
    let result = if let (Value::Integer(x), Value::Integer(y)) = (x.to_value()?, y.to_value()?) {
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
    Ok(1)
}

fn log<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let x = stack.arg(0).to_number()?;
    let base = stack.arg(1);
    let result = if base.get().is_some() {
        x.log(base.to_number()?)
    } else {
        x.ln()
    };
    stack[0] = result.into();
    Ok(1)
}

fn modf<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let x = stack.arg(0);
    let (trunc, fract) = if let Value::Integer(x) = x.to_value()? {
        (x.into(), 0.0.into())
    } else {
        let x = x.to_number()?;
        (number_to_value(x.trunc()), x.fract().into())
    };
    stack[0] = trunc;
    stack[1] = fract;
    Ok(2)
}

fn rad<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.to_radians().into();
    Ok(1)
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

fn sin<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.sin().into();
    Ok(1)
}

fn sqrt<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.sqrt().into();
    Ok(1)
}

fn tan<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack.arg(0).to_number()?.tan().into();
    Ok(1)
}

fn tointeger<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    stack[0] = stack
        .arg(0)
        .to_value()?
        .to_integer()
        .map(|i| i.into())
        .unwrap_or_default();
    Ok(1)
}

fn ty<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let result = match stack.arg(0).to_value()? {
        Value::Integer(_) => gc.allocate_string(B("integer")).into(),
        Value::Number(_) => gc.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    stack[0] = result;
    Ok(1)
}

fn ult<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(window);
    let m = stack.arg(0).to_integer()?;
    let n = stack.arg(1).to_integer()?;
    stack[0] = ((m as u64) < (n as u64)).into();
    Ok(1)
}

fn number_to_value<'gc>(x: Number) -> Value<'gc> {
    let int = x as Integer;
    if x == int as Number {
        Value::Integer(int)
    } else {
        Value::Number(x)
    }
}
