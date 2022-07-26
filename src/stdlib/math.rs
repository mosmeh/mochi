use super::{get_integer_arg, get_number_arg};
use crate::{
    gc::GcHeap,
    types::{Integer, NativeClosure, NativeFunction, Number, StackWindow, Table, Type, Value},
    vm::{ErrorKind, Vm},
};
use bstr::B;
use rand::{Rng, RngCore, SeedableRng};
use rand_xoshiro::Xoshiro256StarStar;
use std::{cell::RefCell, ops::DerefMut, rc::Rc, time::SystemTime};

pub fn create_table(heap: &GcHeap) -> Table {
    let mut table = Table::new();
    table.set_field(heap.allocate_string(B("abs")), NativeFunction::new(abs));
    table.set_field(heap.allocate_string(B("acos")), NativeFunction::new(acos));
    table.set_field(heap.allocate_string(B("asin")), NativeFunction::new(asin));
    table.set_field(heap.allocate_string(B("floor")), NativeFunction::new(floor));
    table.set_field(heap.allocate_string(B("cos")), NativeFunction::new(cos));
    table.set_field(heap.allocate_string(B("deg")), NativeFunction::new(deg));
    table.set_field(heap.allocate_string(B("exp")), NativeFunction::new(exp));
    table.set_field(heap.allocate_string(B("huge")), Number::INFINITY);
    table.set_field(heap.allocate_string(B("log")), NativeFunction::new(log));
    table.set_field(heap.allocate_string(B("maxinteger")), Integer::MAX);
    table.set_field(heap.allocate_string(B("mininteger")), Integer::MIN);
    table.set_field(
        heap.allocate_string(B("pi")),
        std::f64::consts::PI as Number,
    );
    table.set_field(heap.allocate_string(B("rad")), NativeFunction::new(rad));

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
            heap.allocate_string(B("random")),
            heap.allocate(NativeClosure::new(move |vm, window| {
                let stack = vm.stack_mut(window);
                let mut rng = rng.borrow_mut();
                stack[0] = match stack.len() {
                    1 => rng.gen::<Number>().into(),
                    2 => {
                        let upper = get_integer_arg(stack, 1)?;
                        if upper == 0 {
                            rng.gen::<Integer>().into()
                        } else {
                            random_in_range(rng.deref_mut(), 1, upper).into()
                        }
                    }
                    3 => {
                        let lower = get_integer_arg(stack, 1)?;
                        let upper = get_integer_arg(stack, 2)?;
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
        heap.allocate_string(B("randomseed")),
        heap.allocate(NativeClosure::new(move |vm, window| {
            let stack = vm.stack(window.clone());
            let (x, y) = match stack.len() {
                1 => (seed1(), seed2),
                2 => {
                    let x = get_integer_arg(stack, 1)?;
                    (x, 0)
                }
                3.. => {
                    let x = get_integer_arg(stack, 1)?;
                    let y = get_integer_arg(stack, 2)?;
                    (x, y)
                }
                _ => unreachable!(),
            };
            *rng.borrow_mut() = rng_from_seeds(x, y);

            let window = vm.ensure_stack(window, 2);
            let stack = vm.stack_mut(window);
            stack[0] = x.into();
            stack[1] = y.into();
            Ok(2)
        })),
    );

    table.set_field(heap.allocate_string(B("sin")), NativeFunction::new(sin));
    table.set_field(heap.allocate_string(B("sqrt")), NativeFunction::new(sqrt));
    table.set_field(heap.allocate_string(B("tan")), NativeFunction::new(tan));
    table.set_field(heap.allocate_string(B("type")), NativeFunction::new(ty));
    table
}

fn abs(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack[1] {
        Value::Integer(x) => x.abs().into(),
        Value::Number(x) => x.abs().into(),
        _ => {
            return Err(ErrorKind::ArgumentTypeError {
                nth: 1,
                expected_type: Type::Number,
                got_type: stack[1].ty(),
            })
        }
    };
    Ok(1)
}

fn acos(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.acos().into();
    Ok(1)
}

fn asin(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.asin().into();
    Ok(1)
}

fn floor(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = match stack[1] {
        value @ Value::Integer(_) => value,
        value => {
            let x = value
                .to_number()
                .ok_or_else(|| ErrorKind::ArgumentTypeError {
                    nth: 1,
                    expected_type: Type::Number,
                    got_type: stack[1].ty(),
                })?;
            let float_floor = x.floor();
            let int_floor = float_floor as Integer;
            if float_floor == int_floor as Number {
                Value::Integer(int_floor)
            } else {
                Value::Number(float_floor)
            }
        }
    };
    Ok(1)
}

fn cos(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.cos().into();
    Ok(1)
}

fn deg(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.to_degrees().into();
    Ok(1)
}

fn exp(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.exp().into();
    Ok(1)
}

fn log(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    let x = get_number_arg(stack, 1)?;
    let result = if stack.len() > 2 {
        x.log(get_number_arg(stack, 2)?)
    } else {
        x.ln()
    };
    stack[0] = result.into();
    Ok(1)
}

fn rad(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.to_radians().into();
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

fn sin(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.sin().into();
    Ok(1)
}

fn sqrt(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.sqrt().into();
    Ok(1)
}

fn tan(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let stack = vm.stack_mut(window);
    stack[0] = get_number_arg(stack, 1)?.tan().into();
    Ok(1)
}

fn ty(vm: &mut Vm, window: StackWindow) -> Result<usize, ErrorKind> {
    let heap = vm.heap();
    let stack = vm.stack_mut(window);
    let result = match stack[1] {
        Value::Integer(_) => heap.allocate_string(B("integer")).into(),
        Value::Number(_) => heap.allocate_string(B("float")).into(),
        _ => Value::Nil,
    };
    stack[0] = result;
    Ok(1)
}
