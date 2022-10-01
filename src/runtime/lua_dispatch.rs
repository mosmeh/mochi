use super::{ops, ErrorKind, Frame, LuaFrame, OpCode, Operation, Vm};
use crate::{
    gc::GcContext,
    types::{Integer, Number, Table, Upvalue, UpvalueDescription, Value},
    LuaClosure,
};
use std::{
    cmp::PartialOrd,
    ops::{Add, BitAnd, BitOr, BitXor, ControlFlow, Div, Mul, Shl, Shr, Sub},
};

impl<'gc> Vm<'gc> {
    pub(super) fn execute_lua_frame(
        &self,
        gc: &'gc GcContext,
    ) -> Result<ControlFlow<()>, ErrorKind> {
        let thread = self.current_thread();
        let mut thread_ref = thread.borrow_mut(gc);
        let LuaFrame {
            bottom,
            base,
            mut pc,
            num_extra_args,
        } = thread_ref.current_lua_frame().clone();

        let bottom_value = thread_ref.stack[bottom];
        let closure = bottom_value.as_lua_closure().unwrap();
        let upvalues = closure.upvalues.as_slice();
        let proto = closure.proto.as_ref();
        let code = proto.code.as_ref();
        let constants = proto.constants.as_ref();

        let saved_stack_top = thread_ref.stack.len();
        let new_stack_len = base + proto.max_stack_size as usize;
        if thread_ref.stack.len() < new_stack_len {
            thread_ref.stack.resize(new_stack_len, Value::Nil);
        }

        let (lower_stack, stack) = thread_ref.stack.split_at_mut(base);

        loop {
            let insn = code[pc];
            pc += 1;

            match insn.raw_opcode() {
                opcode if opcode == OpCode::Move as u8 => stack[insn.a()] = stack[insn.b()],
                opcode if opcode == OpCode::LoadI as u8 => {
                    stack[insn.a()] = Value::Integer(insn.sbx() as Integer)
                }
                opcode if opcode == OpCode::LoadF as u8 => {
                    stack[insn.a()] = Value::Number(insn.sbx() as Number)
                }
                opcode if opcode == OpCode::LoadK as u8 => {
                    stack[insn.a()] = constants[insn.bx()];
                }
                opcode if opcode == OpCode::LoadKX as u8 => {
                    let next_insn = code[pc];
                    let rb = constants[next_insn.ax()];
                    stack[insn.a()] = rb;
                    pc += 1;
                }
                opcode if opcode == OpCode::LoadFalse as u8 => {
                    stack[insn.a()] = Value::Boolean(false)
                }
                opcode if opcode == OpCode::LFalseSkip as u8 => {
                    stack[insn.a()] = Value::Boolean(false);
                    pc += 1;
                }
                opcode if opcode == OpCode::LoadTrue as u8 => {
                    stack[insn.a()] = Value::Boolean(true)
                }
                opcode if opcode == OpCode::LoadNil as u8 => {
                    stack[insn.a()..][..=insn.b()].fill(Value::Nil)
                }
                opcode if opcode == OpCode::GetUpval as u8 => {
                    let upvalue = upvalues[insn.b()].borrow();
                    let value = super::get_upvalue(thread, base, lower_stack, stack, &upvalue);
                    stack[insn.a()] = value;
                }
                opcode if opcode == OpCode::SetUpval as u8 => {
                    let value = stack[insn.a()];
                    let mut upvalue = upvalues[insn.b()].borrow_mut(gc);
                    super::set_upvalue(gc, thread, base, lower_stack, stack, &mut upvalue, value);
                }
                opcode if opcode == OpCode::GetTabUp as u8 => {
                    let upvalue = upvalues[insn.b()].borrow();
                    let table_value =
                        super::get_upvalue(thread, base, lower_stack, stack, &upvalue);
                    let rc = match constants[insn.c() as usize] {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let raw_value = table_value
                        .borrow_as_table()
                        .map(|table| table.get_field(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            table_value,
                            rc,
                            base + insn.a(),
                        );
                    }
                    stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetTable as u8 => {
                    let rb = stack[insn.b()];
                    let rc = stack[insn.c() as usize];
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            rc,
                            base + insn.a(),
                        );
                    }
                    stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetI as u8 => {
                    let rb = stack[insn.b()];
                    let c = insn.c() as Integer;
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get(c))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            c,
                            base + insn.a(),
                        );
                    }
                    stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetField as u8 => {
                    let rb = stack[insn.b()];
                    let rc = match constants[insn.c() as usize] {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get_field(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            rc,
                            base + insn.a(),
                        );
                    }
                    stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::SetTabUp as u8 => {
                    let kb = match constants[insn.b()] {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let upvalue = upvalues[insn.a()].borrow();
                    let table_value =
                        super::get_upvalue(thread, base, lower_stack, stack, &upvalue);
                    let mut table = table_value.borrow_as_table_mut(gc).ok_or_else(|| {
                        ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: table_value.ty(),
                        }
                    })?;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() { constants[c] } else { stack[c] };
                    table.set_field(kb, rkc);
                }
                opcode if opcode == OpCode::SetTable as u8 => {
                    let ra = stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let rb = stack[insn.b()];
                    let c = insn.c() as usize;
                    let rkc = if insn.k() { constants[c] } else { stack[c] };
                    table.set(rb, rkc)?;
                }
                opcode if opcode == OpCode::SetI as u8 => {
                    let ra = stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let b = insn.b() as Integer;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() { constants[c] } else { stack[c] };
                    table.set(b, rkc)?;
                }
                opcode if opcode == OpCode::SetField as u8 => {
                    let ra = stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let kb = match constants[insn.b()] {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let c = insn.c() as usize;
                    let rkc = if insn.k() { constants[c] } else { stack[c] };
                    table.set_field(kb, rkc);
                }
                opcode if opcode == OpCode::NewTable as u8 => {
                    let mut b = insn.b();
                    if b > 0 {
                        b = 1 << (b - 1);
                    }
                    let mut c = insn.c() as usize;
                    if insn.k() {
                        let next_insn = code[pc];
                        c += next_insn.ax() * (u8::MAX as usize + 1);
                    }
                    let table = Table::with_size(c, b);
                    stack[insn.a()] = gc.allocate_cell(table).into();
                    pc += 1;
                }
                opcode if opcode == OpCode::Self_ as u8 => {
                    let a = insn.a();
                    let rb = stack[insn.b()];
                    stack[a + 1] = rb;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() { constants[c] } else { stack[c] };
                    let rkc = match rkc {
                        Value::String(s) => s,
                        _ => unreachable!(),
                    };
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get_field(rkc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(gc, thread, rb, rkc, base + a);
                    }
                    stack[a] = raw_value;
                }
                opcode if opcode == OpCode::AddI as u8 => ops::do_arithmetic_with_immediate(
                    stack,
                    &mut pc,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                opcode if opcode == OpCode::AddK as u8 => ops::do_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                opcode if opcode == OpCode::SubK as u8 => ops::do_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::wrapping_sub,
                    Number::sub,
                ),
                opcode if opcode == OpCode::MulK as u8 => ops::do_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::wrapping_mul,
                    Number::mul,
                ),
                opcode if opcode == OpCode::ModK as u8 => ops::do_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    ops::modi,
                    ops::modf,
                ),
                opcode if opcode == OpCode::PowK as u8 => ops::do_float_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Number::powf,
                ),
                opcode if opcode == OpCode::DivK as u8 => ops::do_float_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Number::div,
                ),
                opcode if opcode == OpCode::IDivK as u8 => ops::do_arithmetic_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    ops::idivi,
                    ops::idivf,
                ),
                opcode if opcode == OpCode::BAndK as u8 => ops::do_bitwise_op_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::bitand,
                ),
                opcode if opcode == OpCode::BOrK as u8 => ops::do_bitwise_op_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::bitor,
                ),
                opcode if opcode == OpCode::BXorK as u8 => ops::do_bitwise_op_with_constant(
                    stack,
                    &mut pc,
                    constants,
                    insn,
                    Integer::bitxor,
                ),
                opcode if opcode == OpCode::ShrI as u8 => todo!("SHRI"),
                opcode if opcode == OpCode::ShlI as u8 => todo!("SHLI"),
                opcode if opcode == OpCode::Add as u8 => {
                    ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_add, Number::add)
                }
                opcode if opcode == OpCode::Sub as u8 => {
                    ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_sub, Number::sub)
                }
                opcode if opcode == OpCode::Mul as u8 => {
                    ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_mul, Number::mul)
                }
                opcode if opcode == OpCode::Mod as u8 => {
                    ops::do_arithmetic(stack, &mut pc, insn, ops::modi, ops::modf)
                }
                opcode if opcode == OpCode::Pow as u8 => {
                    ops::do_float_arithmetic(stack, &mut pc, insn, Number::powf)
                }
                opcode if opcode == OpCode::Div as u8 => {
                    ops::do_float_arithmetic(stack, &mut pc, insn, Number::div)
                }
                opcode if opcode == OpCode::IDiv as u8 => {
                    ops::do_arithmetic(stack, &mut pc, insn, ops::idivi, ops::idivf)
                }
                opcode if opcode == OpCode::BAnd as u8 => {
                    ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitand)
                }
                opcode if opcode == OpCode::BOr as u8 => {
                    ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitor)
                }
                opcode if opcode == OpCode::BXor as u8 => {
                    ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitxor)
                }
                opcode if opcode == OpCode::Shr as u8 => {
                    ops::do_bitwise_op(stack, &mut pc, insn, Integer::shr)
                }
                opcode if opcode == OpCode::Shl as u8 => {
                    ops::do_bitwise_op(stack, &mut pc, insn, Integer::shl)
                }
                opcode if opcode == OpCode::MmBin as u8 => {
                    let ra = stack[insn.a()];
                    let rb = stack[insn.b()];
                    let prev_insn = code[pc - 2];

                    let metatable = self
                        .metatable_of_object(ra)
                        .or_else(|| self.metatable_of_object(rb))
                        .ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Arithmetic,
                            ty: rb.ty(),
                        })?;

                    let metamethod_name = self.metamethod_names[insn.c() as usize];
                    let metamethod = metatable.borrow().get_field(metamethod_name);

                    thread_ref.current_lua_frame().pc = pc;
                    return Ok(thread_ref.deferred_call_metamethod(
                        metamethod,
                        &[ra, rb],
                        base + prev_insn.a(),
                    ));
                }
                opcode if opcode == OpCode::MmBinI as u8 => todo!("MMBINI"),
                opcode if opcode == OpCode::MmBinK as u8 => todo!("MMBINK"),
                opcode if opcode == OpCode::Unm as u8 => {
                    let rb = stack[insn.b()];
                    stack[insn.a()] = if let Value::Integer(x) = rb {
                        Value::Integer(-x)
                    } else if let Some(x) = rb.to_number_without_string_coercion() {
                        Value::Number(-x)
                    } else {
                        todo!("__unm")
                    };
                }
                opcode if opcode == OpCode::BNot as u8 => {
                    let rb = stack[insn.b()];
                    stack[insn.a()] = if let Some(x) = rb.to_integer_without_string_coercion() {
                        Value::Integer(!x)
                    } else {
                        todo!("__bnot")
                    }
                }
                opcode if opcode == OpCode::Not as u8 => {
                    let rb = stack[insn.b()];
                    stack[insn.a()] = Value::Boolean(!rb.to_boolean())
                }
                opcode if opcode == OpCode::Len as u8 => {
                    let rb = stack[insn.b()];
                    let len = match rb {
                        Value::String(s) => s.len() as Integer,
                        Value::Table(t) => t.borrow().lua_len(),
                        _ => todo!("__len"),
                    };
                    stack[insn.a()] = len.into();
                }
                opcode if opcode == OpCode::Concat as u8 => {
                    let a = insn.a();
                    let b = insn.b();
                    let mut strings = Vec::with_capacity(b);
                    for value in stack[a..].iter().take(b) {
                        if let Some(string) = value.to_string() {
                            strings.push(string);
                        } else {
                            return Err(ErrorKind::TypeError {
                                operation: Operation::Concatenate,
                                ty: value.ty(),
                            });
                        }
                    }
                    stack[a] = gc.allocate_string(strings.concat()).into();
                }
                opcode if opcode == OpCode::Close as u8 => {
                    thread_ref.current_lua_frame().pc = pc;
                    thread_ref.close_upvalues(gc, base + insn.a());
                    return Ok(ControlFlow::Continue(()));
                }
                opcode if opcode == OpCode::Tbc as u8 => todo!("TBC"),
                opcode if opcode == OpCode::Jmp as u8 => {
                    pc = (pc as isize + insn.sj() as isize) as usize
                }
                opcode if opcode == OpCode::Eq as u8 => {
                    let ra = stack[insn.a()];
                    let rb = stack[insn.b()];
                    let cond = ra == rb;
                    ops::do_conditional_jump(&mut pc, code, insn, cond)
                }
                opcode if opcode == OpCode::Lt as u8 => ops::do_comparison(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::lt,
                    Number::lt,
                    PartialOrd::lt,
                ),
                opcode if opcode == OpCode::Le as u8 => ops::do_comparison(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::le,
                    Number::le,
                    PartialOrd::le,
                ),
                opcode if opcode == OpCode::EqK as u8 => {
                    let ra = stack[insn.a()];
                    let rb = constants[insn.b()];
                    let cond = ra == rb;
                    ops::do_conditional_jump(&mut pc, code, insn, cond)
                }
                opcode if opcode == OpCode::EqI as u8 => {
                    let ra = stack[insn.a()];
                    let imm = insn.sb();
                    let cond = match ra {
                        Value::Integer(x) => x == imm as Integer,
                        Value::Number(x) => x == imm as Number,
                        _ => false,
                    };
                    ops::do_conditional_jump(&mut pc, code, insn, cond)
                }
                opcode if opcode == OpCode::LtI as u8 => ops::do_comparison_with_immediate(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::lt,
                    Number::lt,
                ),
                opcode if opcode == OpCode::LeI as u8 => ops::do_comparison_with_immediate(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::le,
                    Number::le,
                ),
                opcode if opcode == OpCode::GtI as u8 => ops::do_comparison_with_immediate(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::gt,
                    Number::gt,
                ),
                opcode if opcode == OpCode::GeI as u8 => ops::do_comparison_with_immediate(
                    stack,
                    &mut pc,
                    code,
                    insn,
                    Integer::ge,
                    Number::ge,
                ),
                opcode if opcode == OpCode::Test as u8 => {
                    let cond = stack[insn.a()].to_boolean();
                    ops::do_conditional_jump(&mut pc, code, insn, cond);
                }
                opcode if opcode == OpCode::TestSet as u8 => {
                    let rb = stack[insn.b()];
                    let cond = rb.to_boolean();
                    if cond == insn.k() {
                        stack[insn.a()] = rb;
                        let next_insn = code[pc];
                        pc = (pc as isize + next_insn.sj() as isize + 1) as usize;
                    } else {
                        pc += 1;
                    }
                }
                opcode if opcode == OpCode::Call as u8 => {
                    let a = insn.a();
                    let b = insn.b();

                    thread_ref.current_lua_frame().pc = pc;
                    thread_ref
                        .stack
                        .truncate(if b > 0 { base + a + b } else { saved_stack_top });
                    return thread_ref.deferred_call(base + a);
                }
                opcode if opcode == OpCode::TailCall as u8 => {
                    let a = insn.a();
                    let b = insn.b();
                    if insn.k() {
                        thread_ref.close_upvalues(gc, bottom);
                    }
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - base
                    };
                    thread_ref
                        .stack
                        .copy_within(base + a..base + a + num_results + 1, bottom);
                    thread_ref.stack.truncate(bottom + num_results + 1);
                    if b > 0 {
                        thread_ref.stack.truncate(bottom + b);
                    }
                    thread_ref.frames.pop().unwrap();
                    return thread_ref.deferred_call(bottom);
                }
                opcode if opcode == OpCode::Return as u8 => {
                    if insn.k() {
                        thread_ref.close_upvalues(gc, bottom);
                    }
                    let a = insn.a();
                    let b = insn.b();
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - base
                    };
                    thread_ref
                        .stack
                        .copy_within(base + a..base + a + num_results, bottom);
                    thread_ref.stack.truncate(bottom + num_results);
                    thread_ref.frames.pop().unwrap();
                    return Ok(if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    });
                }
                opcode if opcode == OpCode::Return0 as u8 => {
                    thread_ref.stack.truncate(bottom);
                    thread_ref.frames.pop().unwrap();
                    return Ok(if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    });
                }
                opcode if opcode == OpCode::Return1 as u8 => {
                    thread_ref.stack[bottom] = stack[insn.a()];
                    thread_ref.stack.truncate(bottom + 1);
                    thread_ref.frames.pop().unwrap();
                    return Ok(if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(())
                    });
                }
                opcode if opcode == OpCode::ForLoop as u8 => {
                    let a = insn.a();
                    if let Some(step) = stack[a + 2].to_integer() {
                        let count = stack[a + 1].to_integer().unwrap();
                        if count > 0 {
                            let index = stack[a].to_integer().unwrap();
                            stack[a + 1] = (count - 1).into();
                            let index = Value::from(index + step);
                            stack[a] = index;
                            stack[a + 3] = index;
                            pc -= insn.bx();
                        }
                    } else {
                        todo!("float FORLOOP")
                    }
                }
                opcode if opcode == OpCode::ForPrep as u8 => {
                    let a = insn.a();
                    if let (Some(init), Some(limit), Some(step)) = (
                        stack[a].to_integer(),
                        stack[a + 1].to_integer(),
                        stack[a + 2].to_integer(),
                    ) {
                        assert!(step != 0);
                        let skip = if step > 0 { init > limit } else { init < limit };
                        if skip {
                            pc += insn.bx() + 1;
                        } else {
                            stack[a + 3] = stack[a];
                            let count = if step > 0 {
                                (limit - init) / step
                            } else {
                                (init - limit) / (-(step + 1) + 1)
                            };
                            stack[a + 1] = count.into();
                        }
                    } else {
                        todo!("float FORPREP")
                    }
                }
                opcode if opcode == OpCode::TForPrep as u8 => pc += insn.bx(),
                opcode if opcode == OpCode::TForCall as u8 => {
                    let a = insn.a();
                    thread_ref.current_lua_frame().pc = pc;

                    let arg_base = base + a;
                    let new_bottom = arg_base + 4;
                    thread_ref.stack.resize(new_bottom + 3, Value::Nil);
                    thread_ref
                        .stack
                        .copy_within(arg_base..arg_base + 3, new_bottom);
                    return thread_ref.deferred_call(new_bottom);
                }
                opcode if opcode == OpCode::TForLoop as u8 => {
                    let a = insn.a();
                    let control = stack[a + 4];
                    if !control.is_nil() {
                        stack[a + 2] = control;
                        pc -= insn.bx();
                    }
                }
                opcode if opcode == OpCode::SetList as u8 => {
                    let a = insn.a();
                    let n = if insn.b() > 0 {
                        Some(insn.b())
                    } else {
                        saved_stack_top.checked_sub(a + base + 1)
                    };
                    if let Some(n) = n {
                        let mut offset = insn.c() as usize;
                        if insn.k() {
                            let next_insn = code[pc];
                            offset += next_insn.ax() * (u8::MAX as usize + 1);
                            pc += 1;
                        }

                        let ra = stack[a];
                        let mut table =
                            ra.borrow_as_table_mut(gc)
                                .ok_or_else(|| ErrorKind::TypeError {
                                    operation: Operation::Index,
                                    ty: ra.ty(),
                                })?;
                        let new_array_len = offset + n;
                        if new_array_len > table.array().len() {
                            table.resize_array(new_array_len);
                        }
                        for (i, x) in stack[a + 1..=a + n].iter().cloned().enumerate() {
                            table.set((offset + i + 1) as Integer, x)?;
                        }
                    }
                }
                opcode if opcode == OpCode::Closure as u8 => {
                    thread_ref.current_lua_frame().pc = pc;
                    let proto = proto.protos[insn.bx()];
                    let upvalues = proto
                        .upvalues
                        .iter()
                        .map(|desc| match desc {
                            UpvalueDescription::Register(index) => {
                                let index = base + index.0 as usize;
                                *thread_ref.open_upvalues.entry(index).or_insert_with(|| {
                                    gc.allocate_cell(Upvalue::Open { thread, index })
                                })
                            }
                            UpvalueDescription::Upvalue(index) => upvalues[index.0 as usize],
                        })
                        .collect();
                    thread_ref.stack[base + insn.a()] =
                        gc.allocate(LuaClosure { proto, upvalues }).into();
                    return Ok(ControlFlow::Continue(()));
                }
                opcode if opcode == OpCode::VarArg as u8 => {
                    let a = insn.a();
                    let n = insn.c();
                    let num_wanted = if n > 0 {
                        n as usize - 1
                    } else {
                        num_extra_args
                    };

                    thread_ref.current_lua_frame().pc = pc;
                    thread_ref.stack.resize(base + a + num_wanted, Value::Nil);

                    if num_wanted > 0 {
                        let extra_args_bottom = base - 1 - num_extra_args;
                        let num_copied = num_wanted.min(num_extra_args);
                        thread_ref.stack.copy_within(
                            extra_args_bottom..extra_args_bottom + num_copied,
                            base + a,
                        );

                        if num_wanted > num_extra_args {
                            thread_ref.stack[base + a + num_extra_args..base + a + num_wanted]
                                .fill(Value::Nil);
                        }
                    }

                    return Ok(ControlFlow::Continue(()));
                }
                opcode if opcode == OpCode::VarArgPrep as u8 => {
                    let num_fixed_args = insn.a();
                    let new_num_extra_args = saved_stack_top - bottom - 1 - num_fixed_args;
                    if new_num_extra_args > 0 {
                        let new_base = saved_stack_top + 1;
                        {
                            let frame = thread_ref.current_lua_frame();
                            frame.pc = pc;
                            frame.base = new_base;
                            frame.num_extra_args = new_num_extra_args;
                        }

                        let new_stack_len = new_base + proto.max_stack_size as usize;
                        if thread_ref.stack.len() < new_stack_len {
                            thread_ref.stack.resize(new_stack_len, Value::Nil);
                        }

                        thread_ref
                            .stack
                            .copy_within(bottom..bottom + num_fixed_args + 1, saved_stack_top);
                        thread_ref.stack[bottom + 1..bottom + num_fixed_args + 1].fill(Value::Nil);

                        return Ok(ControlFlow::Continue(()));
                    }
                }
                opcode if opcode == OpCode::ExtraArg as u8 => unreachable!(),
                _ => panic!("unknown opcode"),
            }

            if gc.should_perform_gc() {
                thread_ref.current_lua_frame().pc = pc;
                return Ok(ControlFlow::Break(()));
            }
        }
    }
}
