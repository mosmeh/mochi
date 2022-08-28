use super::{ExecutionState, Instruction};
use crate::types::{Integer, LuaClosureProto, Number, Value};

fn calc_arithmetic_result<'gc, I, F>(
    a: Value<'gc>,
    b: Value<'gc>,
    int_op: I,
    float_op: F,
) -> Option<Value<'gc>>
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

pub(super) fn do_arithmetic<I, F>(
    state: &mut ExecutionState,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = state.stack[insn.b()];
    let rc = state.stack[insn.c() as usize];
    if let Some(result) = calc_arithmetic_result(rb, rc, int_op, float_op) {
        state.stack[insn.a()] = result;
        state.pc += 1;
    }
}

pub(super) fn do_arithmetic_with_constant<'gc, I, F>(
    state: &mut ExecutionState<'gc, '_>,
    proto: &LuaClosureProto<'gc>,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = state.stack[insn.b()];
    let kc = proto.constants[insn.c() as usize];
    if let Some(result) = calc_arithmetic_result(rb, kc, int_op, float_op) {
        state.stack[insn.a()] = result;
        state.pc += 1;
    }
}

pub(super) fn do_arithmetic_with_immediate<I, F>(
    state: &mut ExecutionState,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    let rb = state.stack[insn.b()];
    let imm = insn.sc();
    let result = match rb {
        Value::Integer(b) => {
            state.pc += 1;
            Value::Integer(int_op(b, imm as Integer))
        }
        Value::Number(b) => {
            state.pc += 1;
            Value::Number(float_op(b, imm as Number))
        }
        _ => return,
    };
    state.stack[insn.a()] = result;
}

fn calc_float_arithmetic_result<'gc, F>(
    a: Value<'gc>,
    b: Value<'gc>,
    float_op: F,
) -> Option<Value<'gc>>
where
    F: Fn(Number, Number) -> Number,
{
    if let (Some(a), Some(b)) = (
        a.to_number_without_string_coercion(),
        b.to_number_without_string_coercion(),
    ) {
        Some(Value::Number(float_op(a, b)))
    } else {
        None
    }
}

pub(super) fn do_float_arithmetic<F>(state: &mut ExecutionState, insn: Instruction, float_op: F)
where
    F: Fn(Number, Number) -> Number,
{
    let rb = state.stack[insn.b()];
    let rc = state.stack[insn.c() as usize];
    if let Some(result) = calc_float_arithmetic_result(rb, rc, float_op) {
        state.stack[insn.a()] = result;
        state.pc += 1;
    }
}

pub(super) fn do_float_arithmetic_with_constant<F>(
    state: &mut ExecutionState,
    proto: &LuaClosureProto,
    insn: Instruction,
    float_op: F,
) where
    F: Fn(Number, Number) -> Number,
{
    let rb = state.stack[insn.b()];
    let kc = proto.constants[insn.c() as usize];
    if let (Some(b), Some(c)) = (
        rb.to_number_without_string_coercion(),
        kc.to_number_without_string_coercion(),
    ) {
        state.stack[insn.a()] = Value::Number(float_op(b, c));
        state.pc += 1;
    }
}

fn calc_bitwise_op_result<'gc, I>(a: Value<'gc>, b: Value<'gc>, int_op: I) -> Option<Value<'gc>>
where
    I: Fn(Integer, Integer) -> Integer,
{
    if let (Some(a), Some(b)) = (
        a.to_integer_without_string_coercion(),
        b.to_integer_without_string_coercion(),
    ) {
        Some(Value::Integer(int_op(a, b)))
    } else {
        None
    }
}

pub(super) fn do_bitwise_op<I>(state: &mut ExecutionState, insn: Instruction, int_op: I)
where
    I: Fn(Integer, Integer) -> Integer,
{
    let rb = state.stack[insn.b()];
    let rc = state.stack[insn.c() as usize];
    if let Some(result) = calc_bitwise_op_result(rb, rc, int_op) {
        state.stack[insn.a()] = result;
        state.pc += 1;
    }
}

pub(super) fn do_bitwise_op_with_constant<'gc, I>(
    state: &mut ExecutionState<'gc, '_>,
    proto: &LuaClosureProto<'gc>,
    insn: Instruction,
    int_op: I,
) where
    I: Fn(Integer, Integer) -> Integer,
{
    let rb = state.stack[insn.b()];
    let kc = proto.constants[insn.c() as usize];
    debug_assert!(matches!(kc, Value::Integer(_)));
    if let Some(result) = calc_bitwise_op_result(rb, kc, int_op) {
        state.stack[insn.a()] = result;
        state.pc += 1;
    }
}

pub(super) fn do_conditional_jump(
    state: &mut ExecutionState,
    proto: &LuaClosureProto,
    insn: Instruction,
    cond: bool,
) {
    if cond == insn.k() {
        let next_insn = proto.code[state.pc];
        state.pc = (state.pc as isize + next_insn.sj() as isize + 1) as usize;
    } else {
        state.pc += 1;
    }
}

pub(super) fn do_comparison<I, F, S>(
    state: &mut ExecutionState,
    proto: &LuaClosureProto,
    insn: Instruction,
    int_op: I,
    float_op: F,
    str_op: S,
) where
    I: Fn(&Integer, &Integer) -> bool,
    F: Fn(&Number, &Number) -> bool,
    S: Fn(&[u8], &[u8]) -> bool,
{
    let ra = state.stack[insn.a()];
    let rb = state.stack[insn.b()];
    let cond = match (ra, rb) {
        (Value::Integer(a), Value::Integer(b)) => int_op(&a, &b),
        (Value::String(ref a), Value::String(ref b)) => str_op(a, b),
        _ => {
            if let (Some(a), Some(b)) = (
                ra.to_number_without_string_coercion(),
                rb.to_number_without_string_coercion(),
            ) {
                float_op(&a, &b)
            } else {
                todo!("comparison metamethod")
            }
        }
    };
    do_conditional_jump(state, proto, insn, cond);
}

pub(super) fn do_comparison_with_immediate<I, F>(
    state: &mut ExecutionState,
    proto: &LuaClosureProto,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(&Integer, &Integer) -> bool,
    F: Fn(&Number, &Number) -> bool,
{
    let ra = state.stack[insn.a()];
    let imm = insn.sb();
    let cond = match ra {
        Value::Integer(x) => int_op(&x, &(imm as Integer)),
        Value::Number(x) => float_op(&x, &(imm as Number)),
        _ => todo!("comparison metamethod"),
    };
    do_conditional_jump(state, proto, insn, cond);
}

pub(super) fn idivi(m: Integer, n: Integer) -> Integer {
    let q = m / n;
    if m ^ n < 0 && m % n != 0 {
        q - 1
    } else {
        q
    }
}

pub(super) fn idivf(m: Number, n: Number) -> Number {
    (m / n).floor()
}

pub(super) fn modi(m: Integer, n: Integer) -> Integer {
    let r = m % n;
    if r != 0 && r ^ n < 0 {
        r + n
    } else {
        r
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
