use super::{Instruction, State};
use crate::types::{Integer, LuaClosureProto, Number, Value};

fn calc_arithmetic_result<'a, I, F>(
    a: Value<'a>,
    b: Value<'a>,
    int_op: I,
    float_op: F,
) -> Option<Value<'a>>
where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    if let (Value::Integer(a), Value::Integer(b)) = (a, b) {
        return Some(Value::Integer(int_op(a, b)));
    }
    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
        return Some(Value::Number(float_op(a, b)));
    }
    None
}

pub(crate) fn do_arithmetic<I, F>(state: &mut State, insn: Instruction, int_op: I, float_op: F)
where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let rc = state.stack[insn.c() as usize];
        if let Some(result) = calc_arithmetic_result(rb, rc, int_op, float_op) {
            state.pc += 1;
            result
        } else {
            return;
        }
    };
}

pub(crate) fn do_arithmetic_with_constant<'a, I, F>(
    state: &mut State<'a, '_>,
    proto: &LuaClosureProto<'a>,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let kc = proto.constants[insn.c() as usize];
        if let Some(result) = calc_arithmetic_result(rb, kc, int_op, float_op) {
            state.pc += 1;
            result
        } else {
            return;
        }
    };
}

pub(crate) fn do_arithmetic_with_immediate<I, F>(
    state: &mut State,
    insn: Instruction,
    int_op: I,
    float_op: F,
) where
    I: Fn(Integer, Integer) -> Integer,
    F: Fn(Number, Number) -> Number,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let imm = insn.sc();
        match rb {
            Value::Integer(b) => {
                state.pc += 1;
                Value::Integer(int_op(b, imm as Integer))
            }
            Value::Number(b) => {
                state.pc += 1;
                Value::Number(float_op(b, imm as Number))
            }
            _ => return,
        }
    };
}

fn calc_float_arithmetic_result<'a, F>(a: Value<'a>, b: Value<'a>, float_op: F) -> Option<Value<'a>>
where
    F: Fn(Number, Number) -> Number,
{
    if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
        Some(Value::Number(float_op(a, b)))
    } else {
        None
    }
}

pub(crate) fn do_float_arithmetic<F>(state: &mut State, insn: Instruction, float_op: F)
where
    F: Fn(Number, Number) -> Number,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let rc = state.stack[insn.c() as usize];
        if let Some(result) = calc_float_arithmetic_result(rb, rc, float_op) {
            state.pc += 1;
            result
        } else {
            return;
        }
    };
}

pub(crate) fn do_float_arithmetic_with_constant<F>(
    state: &mut State,
    proto: &LuaClosureProto,
    insn: Instruction,
    float_op: F,
) where
    F: Fn(Number, Number) -> Number,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let kc = proto.constants[insn.c() as usize];
        debug_assert!(matches!(kc, Value::Number(_)));
        if let (Some(b), Some(c)) = (rb.as_number(), kc.as_number()) {
            state.pc += 1;
            Value::Number(float_op(b, c))
        } else {
            return;
        }
    };
}

fn calc_bitwise_op_result<'a, I>(a: Value<'a>, b: Value<'a>, int_op: I) -> Option<Value<'a>>
where
    I: Fn(Integer, Integer) -> Integer,
{
    if let (Some(a), Some(b)) = (a.as_integer(), b.as_integer()) {
        Some(Value::Integer(int_op(a, b)))
    } else {
        None
    }
}

pub(crate) fn do_bitwise_op<I>(state: &mut State, insn: Instruction, int_op: I)
where
    I: Fn(Integer, Integer) -> Integer,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let rc = state.stack[insn.c() as usize];
        if let Some(result) = calc_bitwise_op_result(rb, rc, int_op) {
            state.pc += 1;
            result
        } else {
            return;
        }
    };
}

pub(crate) fn do_bitwise_op_with_constant<'a, I>(
    state: &mut State<'a, '_>,
    proto: &LuaClosureProto<'a>,
    insn: Instruction,
    int_op: I,
) where
    I: Fn(Integer, Integer) -> Integer,
{
    state.stack[insn.a()] = {
        let rb = state.stack[insn.b()];
        let kc = proto.constants[insn.c() as usize];
        debug_assert!(matches!(kc, Value::Integer(_)));
        if let Some(result) = calc_bitwise_op_result(rb, kc, int_op) {
            state.pc += 1;
            result
        } else {
            return;
        }
    }
}

pub(crate) fn do_conditional_jump(
    state: &mut State,
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

pub(crate) fn do_comparison<I, F, S>(
    state: &mut State,
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
            if let (Some(a), Some(b)) = (ra.as_number(), rb.as_number()) {
                float_op(&a, &b)
            } else {
                unimplemented!("order")
            }
        }
    };
    do_conditional_jump(state, proto, insn, cond);
}

pub(crate) fn do_comparison_with_immediate<I, F>(
    state: &mut State,
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
        _ => unimplemented!("orderI"),
    };
    do_conditional_jump(state, proto, insn, cond);
}
