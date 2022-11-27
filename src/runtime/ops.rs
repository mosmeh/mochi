use super::{ErrorKind, Instruction};
use crate::{
    number_is_valid_integer,
    types::{Integer, Number, Value},
};

fn arithmetic<'gc, I, F>(a: Value<'gc>, b: Value<'gc>, int_op: I, float_op: F) -> Option<Value<'gc>>
where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    if let (Value::Integer(a), Value::Integer(b)) = (a, b) {
        return Some(Value::Integer(int_op(a, b)));
    }
    if let (Some(a), Some(b)) = (
        a.to_number_without_string_coercion(),
        b.to_number_without_string_coercion(),
    ) {
        return Some(Value::Number(float_op(a, b)));
    }
    None
}

pub(super) fn compare_with_immediate<I, F>(
    a: Value,
    imm: i16,
    int_op: I,
    float_op: F,
) -> Option<bool>
where
    I: Fn(&Integer, &Integer) -> bool,
    F: Fn(&Number, &Number) -> bool,
{
    match a {
        Value::Integer(x) => Some(int_op(&x, &(imm as Integer))),
        Value::Number(x) => Some(float_op(&x, &(imm as Number))),
        _ => None,
    }
}

pub(super) fn do_arithmetic<I, F>(
    stack: &mut [Value],
    pc: &mut usize,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = stack[insn.b()];
    let rc = stack[insn.c() as usize];
    if let Some(result) = arithmetic(rb, rc, int_op, float_op) {
        stack[insn.a()] = result;
        *pc += 1;
    }
}

pub(super) fn do_arithmetic_with_constant<'gc, I, F>(
    stack: &mut [Value<'gc>],
    pc: &mut usize,
    constants: &[Value<'gc>],
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = stack[insn.b()];
    let kc = constants[insn.c() as usize];
    debug_assert!(matches!(kc, Value::Integer(_) | Value::Number(_)));
    if let Some(result) = arithmetic(rb, kc, int_op, float_op) {
        stack[insn.a()] = result;
        *pc += 1;
    }
}

pub(super) fn do_arithmetic_with_immediate<I, F>(
    stack: &mut [Value],
    pc: &mut usize,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = stack[insn.b()];
    let imm = insn.sc();
    let result = match rb {
        Value::Integer(b) => {
            *pc += 1;
            Value::Integer(int_op(b, imm as Integer))
        }
        Value::Number(b) => {
            *pc += 1;
            Value::Number(float_op(b, imm as Number))
        }
        _ => return,
    };
    stack[insn.a()] = result;
}

pub(super) fn do_float_arithmetic<F>(
    stack: &mut [Value],
    pc: &mut usize,
    insn: Instruction,
    float_op: F,
) where
    F: Fn(Number, Number) -> Number,
{
    let rb = stack[insn.b()];
    let rc = stack[insn.c() as usize];
    if let (Some(a), Some(b)) = (
        rb.to_number_without_string_coercion(),
        rc.to_number_without_string_coercion(),
    ) {
        stack[insn.a()] = Value::Number(float_op(a, b));
        *pc += 1;
    }
}

pub(super) fn do_float_arithmetic_with_constant<'gc, F>(
    stack: &mut [Value<'gc>],
    pc: &mut usize,
    constants: &[Value<'gc>],
    insn: Instruction,
    float_op: F,
) where
    F: Fn(Number, Number) -> Number,
{
    let rb = stack[insn.b()];
    let kc = constants[insn.c() as usize];
    debug_assert!(matches!(kc, Value::Integer(_) | Value::Number(_)));
    if let (Some(b), Some(c)) = (
        rb.to_number_without_string_coercion(),
        kc.to_number_without_string_coercion(),
    ) {
        stack[insn.a()] = Value::Number(float_op(b, c));
        *pc += 1;
    }
}

pub(super) fn do_bitwise_op<I>(stack: &mut [Value], pc: &mut usize, insn: Instruction, int_op: I)
where
    I: Fn(Integer, Integer) -> Integer,
{
    let rb = stack[insn.b()];
    let rc = stack[insn.c() as usize];
    if let (Some(a), Some(b)) = (
        rb.to_integer_without_string_coercion(),
        rc.to_integer_without_string_coercion(),
    ) {
        stack[insn.a()] = Value::Integer(int_op(a, b));
        *pc += 1;
    }
}

pub(super) fn do_bitwise_op_with_constant<'gc, I>(
    stack: &mut [Value<'gc>],
    pc: &mut usize,
    constants: &[Value<'gc>],
    insn: Instruction,
    int_op: I,
) where
    I: Fn(Integer, Integer) -> Integer,
{
    let rb = stack[insn.b()];
    if let Some(a) = rb.to_integer_without_string_coercion() {
        stack[insn.a()] = match constants[insn.c() as usize] {
            Value::Integer(b) => Value::Integer(int_op(a, b)),
            _ => unreachable!(),
        };
        *pc += 1;
    }
}

pub(super) fn do_conditional_jump(
    pc: &mut usize,
    code: &[Instruction],
    insn: Instruction,
    cond: bool,
) {
    if cond == insn.k() {
        let next_insn = code[*pc];
        *pc = (*pc as isize + next_insn.sj() as isize + 1) as usize;
    } else {
        *pc += 1;
    }
}

pub(super) fn do_forprep(for_stack: &mut [Value]) -> Result<bool, ErrorKind> {
    let [init_value, limit_value, step_value, control_variable]: &mut [_; 4] =
        (&mut for_stack[..4]).try_into().unwrap();

    if let (Value::Integer(init), Value::Integer(step)) = (*init_value, *step_value) {
        if step == 0 {
            return Err(ErrorKind::other("'for' step is zero"));
        }

        *control_variable = *init_value;

        let limit = match limit_value.to_integer() {
            Some(l) => l,
            None => {
                let float_limit = match limit_value.to_number() {
                    Some(l) => l,
                    None => {
                        return Err(ErrorKind::ForError {
                            what: "limit",
                            got_type: limit_value.ty().name(),
                        })
                    }
                };
                let round_limit = if step < 0 {
                    float_limit.ceil()
                } else {
                    float_limit.floor()
                };
                if number_is_valid_integer(round_limit) {
                    round_limit as Integer
                } else if float_limit > 0.0 {
                    if step < 0 {
                        return Ok(false);
                    }
                    Integer::MAX
                } else {
                    if step > 0 {
                        return Ok(false);
                    }
                    Integer::MIN
                }
            }
        };
        if step > 0 {
            if init > limit {
                return Ok(false);
            }
        } else if init < limit {
            return Ok(false);
        }

        let (uint_init, uint_limit, uint_step) = (init as u64, limit as u64, step as u64);
        let count = if step > 0 {
            uint_limit.wrapping_sub(uint_init) / uint_step
        } else {
            uint_init.wrapping_sub(uint_limit) / uint_step.wrapping_neg()
        };
        *limit_value = (count as Integer).into();
        return Ok(true);
    }

    let limit = limit_value.to_number().ok_or(ErrorKind::ForError {
        what: "limit",
        got_type: limit_value.ty().name(),
    })?;
    let step = step_value.to_number().ok_or(ErrorKind::ForError {
        what: "step",
        got_type: step_value.ty().name(),
    })?;
    let init = init_value.to_number().ok_or(ErrorKind::ForError {
        what: "initial value",
        got_type: init_value.ty().name(),
    })?;
    if step == 0.0 {
        return Err(ErrorKind::other("'for' step is zero"));
    }
    if step > 0.0 {
        if init > limit {
            return Ok(false);
        }
    } else if init < limit {
        return Ok(false);
    }

    *limit_value = limit.into();
    *step_value = step.into();

    let init = init.into();
    *init_value = init;
    *control_variable = init;

    Ok(true)
}

pub(super) fn idivi(m: Integer, n: Integer) -> Integer {
    match n {
        0 => todo!("attempt to divide by zero"),
        -1 => m.wrapping_neg(),
        _ => {
            let q = m / n;
            if m ^ n < 0 && m % n != 0 {
                q - 1
            } else {
                q
            }
        }
    }
}

pub(super) fn idivf(m: Number, n: Number) -> Number {
    (m / n).floor()
}

pub(super) fn modi(m: Integer, n: Integer) -> Integer {
    match n {
        0 => todo!("attempt to perform 'n%0'"),
        -1 => 0,
        _ => {
            let r = m % n;
            if r != 0 && r ^ n < 0 {
                r + n
            } else {
                r
            }
        }
    }
}

pub(super) fn modf(m: Number, n: Number) -> Number {
    let r = m % n;
    let c = if r > 0.0 { n < 0.0 } else { r < 0.0 && n > 0.0 };
    if c {
        r + n
    } else {
        r
    }
}

pub(super) fn shl(x: Integer, y: Integer) -> Integer {
    const BITS: Integer = Integer::BITS as Integer;
    if y <= -BITS || BITS <= y {
        0
    } else if y >= 0 {
        ((x as u64) << y as u64) as Integer
    } else {
        (x as u64 >> -y as u64) as Integer
    }
}

pub(super) fn shr(x: Integer, y: Integer) -> Integer {
    shl(x, y.wrapping_neg())
}

pub(super) fn lt(a: Value, b: Value) -> Option<bool> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Some(a < b),
        (Value::Number(a), Value::Number(b)) => Some(a < b),
        (Value::Integer(a), Value::Number(b)) => {
            let ceil_b = b.ceil();
            Some(if number_is_valid_integer(ceil_b) {
                a < ceil_b as Integer
            } else {
                b > 0.0
            })
        }
        (Value::Number(a), Value::Integer(b)) => {
            let floor_a = a.floor();
            Some(if number_is_valid_integer(floor_a) {
                (floor_a as Integer) < b
            } else {
                a < 0.0
            })
        }
        (Value::String(a), Value::String(b)) => Some(a < b),
        _ => None,
    }
}

pub(super) fn le(a: Value, b: Value) -> Option<bool> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Some(a <= b),
        (Value::Number(a), Value::Number(b)) => Some(a <= b),
        (Value::Integer(a), Value::Number(b)) => {
            let floor_b = b.floor();
            Some(if number_is_valid_integer(floor_b) {
                a <= floor_b as Integer
            } else {
                b > 0.0
            })
        }
        (Value::Number(a), Value::Integer(b)) => {
            let ceil_a = a.ceil();
            Some(if number_is_valid_integer(ceil_a) {
                ceil_a as Integer <= b
            } else {
                a < 0.0
            })
        }
        (Value::String(a), Value::String(b)) => Some(a <= b),
        _ => None,
    }
}
