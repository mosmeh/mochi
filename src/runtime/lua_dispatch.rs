use super::{ops, ErrorKind, ExecutionState, OpCode, Operation, Vm};
use crate::{
    gc::GcContext,
    types::{Integer, Number, Table, Upvalue, UpvalueDescription, Value},
    LuaClosure,
};
use std::{
    cmp::PartialOrd,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub},
};

impl<'gc> Vm<'gc> {
    pub(super) fn execute_lua_frame(&self, gc: &'gc GcContext) -> Result<(), ErrorKind> {
        let thread = self.current_thread();
        let mut thread_ref = thread.borrow_mut(gc);
        let saved_current_frame = thread_ref.current_lua_frame().clone();

        let bottom_value = thread_ref.stack[saved_current_frame.bottom];
        let closure = bottom_value.as_lua_closure().unwrap();

        let saved_stack_top = thread_ref.stack.len();
        let new_stack_len = saved_current_frame.base + closure.proto.max_stack_size as usize;
        if thread_ref.stack.len() < new_stack_len {
            thread_ref.stack.resize(new_stack_len, Value::Nil);
        }

        let (lower_stack, stack) = thread_ref.stack.split_at_mut(saved_current_frame.base);
        let mut state = ExecutionState {
            thread,
            base: saved_current_frame.base,
            pc: saved_current_frame.pc,
            stack,
            lower_stack,
        };

        loop {
            let insn = closure.proto.code[state.pc];
            state.pc += 1;

            match insn.raw_opcode() {
                opcode if opcode == OpCode::Move as u8 => {
                    state.stack[insn.a()] = state.stack[insn.b()]
                }
                opcode if opcode == OpCode::LoadI as u8 => {
                    state.stack[insn.a()] = Value::Integer(insn.sbx() as Integer)
                }
                opcode if opcode == OpCode::LoadF as u8 => {
                    state.stack[insn.a()] = Value::Number(insn.sbx() as Number)
                }
                opcode if opcode == OpCode::LoadK as u8 => {
                    state.stack[insn.a()] = closure.proto.constants[insn.bx()];
                }
                opcode if opcode == OpCode::LoadKX as u8 => {
                    let next_insn = closure.proto.code[state.pc];
                    let rb = closure.proto.constants[next_insn.ax()];
                    state.stack[insn.a()] = rb;
                    state.pc += 1;
                }
                opcode if opcode == OpCode::LoadFalse as u8 => {
                    state.stack[insn.a()] = Value::Boolean(false)
                }
                opcode if opcode == OpCode::LFalseSkip as u8 => {
                    state.stack[insn.a()] = Value::Boolean(false);
                    state.pc += 1;
                }
                opcode if opcode == OpCode::LoadTrue as u8 => {
                    state.stack[insn.a()] = Value::Boolean(true)
                }
                opcode if opcode == OpCode::LoadNil as u8 => {
                    state.stack[insn.a()..][..=insn.b()].fill(Value::Nil)
                }
                opcode if opcode == OpCode::GetUpval as u8 => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let value = state.upvalue(&upvalue);
                    state.stack[insn.a()] = value;
                }
                opcode if opcode == OpCode::SetUpval as u8 => {
                    let value = state.stack[insn.a()];
                    let mut upvalue = closure.upvalues[insn.b()].borrow_mut(gc);
                    state.set_upvalue(gc, &mut upvalue, value);
                }
                opcode if opcode == OpCode::GetTabUp as u8 => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let table_value = state.upvalue(&upvalue);
                    let rc = if let Value::String(s) = closure.proto.constants[insn.c() as usize] {
                        s
                    } else {
                        unreachable!();
                    };
                    let raw_value = table_value
                        .borrow_as_table()
                        .map(|table| table.get_field(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = state.pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            table_value,
                            rc,
                            saved_current_frame.base + insn.a(),
                        );
                    }
                    state.stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetTable as u8 => {
                    let rb = state.stack[insn.b()];
                    let rc = state.stack[insn.c() as usize];
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = state.pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            rc,
                            saved_current_frame.base + insn.a(),
                        );
                    }
                    state.stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetI as u8 => {
                    let rb = state.stack[insn.b()];
                    let c = insn.c() as Integer;
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get(c))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = state.pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            c,
                            saved_current_frame.base + insn.a(),
                        );
                    }
                    state.stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::GetField as u8 => {
                    let rb = state.stack[insn.b()];
                    let rc = if let Value::String(s) = closure.proto.constants[insn.c() as usize] {
                        s
                    } else {
                        unreachable!();
                    };
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get_field(rc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = state.pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            rc,
                            saved_current_frame.base + insn.a(),
                        );
                    }
                    state.stack[insn.a()] = raw_value;
                }
                opcode if opcode == OpCode::SetTabUp as u8 => {
                    let kb = if let Value::String(s) = closure.proto.constants[insn.b()] {
                        s
                    } else {
                        unreachable!();
                    };
                    let upvalue = closure.upvalues[insn.a()].borrow();
                    let table_value = state.upvalue(&upvalue);
                    let mut table = table_value.borrow_as_table_mut(gc).ok_or_else(|| {
                        ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: table_value.ty(),
                        }
                    })?;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() {
                        closure.proto.constants[c]
                    } else {
                        state.stack[c]
                    };
                    table.set_field(kb, rkc);
                }
                opcode if opcode == OpCode::SetTable as u8 => {
                    let ra = state.stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let rb = state.stack[insn.b()];
                    let c = insn.c() as usize;
                    let rkc = if insn.k() {
                        closure.proto.constants[c]
                    } else {
                        state.stack[c]
                    };
                    table.set(rb, rkc)?;
                }
                opcode if opcode == OpCode::SetI as u8 => {
                    let ra = state.stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let b = insn.b() as Integer;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() {
                        closure.proto.constants[c]
                    } else {
                        state.stack[c]
                    };
                    table.set(b, rkc)?;
                }
                opcode if opcode == OpCode::SetField as u8 => {
                    let ra = state.stack[insn.a()];
                    let mut table =
                        ra.borrow_as_table_mut(gc)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: ra.ty(),
                            })?;
                    let kb = if let Value::String(s) = closure.proto.constants[insn.b()] {
                        s
                    } else {
                        unreachable!();
                    };
                    let c = insn.c() as usize;
                    let rkc = if insn.k() {
                        closure.proto.constants[c]
                    } else {
                        state.stack[c]
                    };
                    table.set_field(kb, rkc);
                }
                opcode if opcode == OpCode::NewTable as u8 => {
                    let mut b = insn.b();
                    if b > 0 {
                        b = 1 << (b - 1);
                    }
                    let mut c = insn.c() as usize;
                    if insn.k() {
                        let next_insn = closure.proto.code[state.pc];
                        c += next_insn.ax() * (u8::MAX as usize + 1);
                    }
                    let table = Table::with_size(c, b);
                    state.stack[insn.a()] = gc.allocate_cell(table).into();
                    state.pc += 1;
                }
                opcode if opcode == OpCode::Self_ as u8 => {
                    let a = insn.a();
                    let rb = state.stack[insn.b()];
                    state.stack[a + 1] = rb;
                    let c = insn.c() as usize;
                    let rkc = if insn.k() {
                        closure.proto.constants[c]
                    } else {
                        state.stack[c]
                    };
                    let rkc = if let Value::String(s) = rkc {
                        s
                    } else {
                        unreachable!();
                    };
                    let raw_value = rb
                        .borrow_as_table()
                        .map(|table| table.get_field(rkc))
                        .unwrap_or_default();
                    if raw_value.is_nil() {
                        thread_ref.current_lua_frame().pc = state.pc;
                        drop(thread_ref);
                        return self.deferred_call_index_metamethod(
                            gc,
                            thread,
                            rb,
                            rkc,
                            saved_current_frame.base + a,
                        );
                    }
                    state.stack[a] = raw_value;
                }
                opcode if opcode == OpCode::AddI as u8 => ops::do_arithmetic_with_immediate(
                    &mut state,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                opcode if opcode == OpCode::AddK as u8 => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                opcode if opcode == OpCode::SubK as u8 => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_sub,
                    Number::sub,
                ),
                opcode if opcode == OpCode::MulK as u8 => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_mul,
                    Number::mul,
                ),
                opcode if opcode == OpCode::ModK as u8 => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    ops::modi,
                    ops::modf,
                ),
                opcode if opcode == OpCode::PowK as u8 => ops::do_float_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Number::powf,
                ),
                opcode if opcode == OpCode::DivK as u8 => ops::do_float_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Number::div,
                ),
                opcode if opcode == OpCode::IDivK as u8 => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    ops::idivi,
                    ops::idivf,
                ),
                opcode if opcode == OpCode::BAndK as u8 => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitand,
                ),
                opcode if opcode == OpCode::BOrK as u8 => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitor,
                ),
                opcode if opcode == OpCode::BXorK as u8 => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitxor,
                ),
                opcode if opcode == OpCode::ShrI as u8 => todo!("SHRI"),
                opcode if opcode == OpCode::ShlI as u8 => todo!("SHLI"),
                opcode if opcode == OpCode::Add as u8 => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_add, Number::add)
                }
                opcode if opcode == OpCode::Sub as u8 => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_sub, Number::sub)
                }
                opcode if opcode == OpCode::Mul as u8 => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_mul, Number::mul)
                }
                opcode if opcode == OpCode::Mod as u8 => {
                    ops::do_arithmetic(&mut state, insn, ops::modi, ops::modf)
                }
                opcode if opcode == OpCode::Pow as u8 => {
                    ops::do_float_arithmetic(&mut state, insn, Number::powf)
                }
                opcode if opcode == OpCode::Div as u8 => {
                    ops::do_float_arithmetic(&mut state, insn, Number::div)
                }
                opcode if opcode == OpCode::IDiv as u8 => {
                    ops::do_arithmetic(&mut state, insn, ops::idivi, ops::idivf)
                }
                opcode if opcode == OpCode::BAnd as u8 => {
                    ops::do_bitwise_op(&mut state, insn, Integer::bitand)
                }
                opcode if opcode == OpCode::BOr as u8 => {
                    ops::do_bitwise_op(&mut state, insn, Integer::bitor)
                }
                opcode if opcode == OpCode::BXor as u8 => {
                    ops::do_bitwise_op(&mut state, insn, Integer::bitxor)
                }
                opcode if opcode == OpCode::Shr as u8 => {
                    ops::do_bitwise_op(&mut state, insn, Integer::shr)
                }
                opcode if opcode == OpCode::Shl as u8 => {
                    ops::do_bitwise_op(&mut state, insn, Integer::shl)
                }
                opcode if opcode == OpCode::MmBin as u8 => {
                    let ra = state.stack[insn.a()];
                    let rb = state.stack[insn.b()];
                    let prev_insn = closure.proto.code[state.pc - 2];

                    let metatable = self
                        .metatable_of_object(ra)
                        .or_else(|| self.metatable_of_object(rb))
                        .ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Arithmetic,
                            ty: rb.ty(),
                        })?;

                    let metamethod_name = self.metamethod_names[insn.c() as usize];
                    let metamethod = metatable.borrow().get_field(metamethod_name);

                    thread_ref.current_lua_frame().pc = state.pc;
                    thread_ref.deferred_call_metamethod(
                        metamethod,
                        &[ra, rb],
                        saved_current_frame.base + prev_insn.a(),
                    );

                    return Ok(());
                }
                opcode if opcode == OpCode::MmBinI as u8 => todo!("MMBINI"),
                opcode if opcode == OpCode::MmBinK as u8 => todo!("MMBINK"),
                opcode if opcode == OpCode::Unm as u8 => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Value::Integer(x) = rb {
                        Value::Integer(-x)
                    } else if let Some(x) = rb.to_number_without_string_coercion() {
                        Value::Number(-x)
                    } else {
                        todo!("__unm")
                    };
                }
                opcode if opcode == OpCode::BNot as u8 => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Some(x) = rb.to_integer_without_string_coercion()
                    {
                        Value::Integer(!x)
                    } else {
                        todo!("__bnot")
                    }
                }
                opcode if opcode == OpCode::Not as u8 => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = Value::Boolean(!rb.to_boolean())
                }
                opcode if opcode == OpCode::Len as u8 => {
                    let rb = state.stack[insn.b()];
                    let len = match rb {
                        Value::String(s) => s.len() as Integer,
                        Value::Table(t) => t.borrow().lua_len(),
                        _ => todo!("__len"),
                    };
                    state.stack[insn.a()] = len.into();
                }
                opcode if opcode == OpCode::Concat as u8 => {
                    let a = insn.a();
                    let b = insn.b();
                    let mut strings = Vec::with_capacity(b);
                    for value in state.stack[a..].iter().take(b) {
                        if let Some(string) = value.to_string() {
                            strings.push(string);
                        } else {
                            return Err(ErrorKind::TypeError {
                                operation: Operation::Concatenate,
                                ty: value.ty(),
                            });
                        }
                    }
                    state.stack[a] = gc.allocate_string(strings.concat()).into();
                }
                opcode if opcode == OpCode::Close as u8 => {
                    thread_ref.current_lua_frame().pc = state.pc;
                    thread_ref.close_upvalues(gc, saved_current_frame.base + insn.a());
                    return Ok(());
                }
                opcode if opcode == OpCode::Tbc as u8 => todo!("TBC"),
                opcode if opcode == OpCode::Jmp as u8 => {
                    state.pc = (state.pc as isize + insn.sj() as isize) as usize
                }
                opcode if opcode == OpCode::Eq as u8 => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::eq,
                    Number::eq,
                    PartialEq::eq,
                ),
                opcode if opcode == OpCode::Lt as u8 => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::lt,
                    Number::lt,
                    PartialOrd::lt,
                ),
                opcode if opcode == OpCode::Le as u8 => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::le,
                    Number::le,
                    PartialOrd::le,
                ),
                opcode if opcode == OpCode::EqK as u8 => {
                    let ra = state.stack[insn.a()];
                    let rb = closure.proto.constants[insn.b()];
                    let cond = ra == rb;
                    ops::do_conditional_jump(&mut state, &closure.proto, insn, cond)
                }
                opcode if opcode == OpCode::EqI as u8 => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::eq,
                    Number::eq,
                ),
                opcode if opcode == OpCode::LtI as u8 => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::lt,
                    Number::lt,
                ),
                opcode if opcode == OpCode::LeI as u8 => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::le,
                    Number::le,
                ),
                opcode if opcode == OpCode::GtI as u8 => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::gt,
                    Number::gt,
                ),
                opcode if opcode == OpCode::GeI as u8 => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::ge,
                    Number::ge,
                ),
                opcode if opcode == OpCode::Test as u8 => {
                    let cond = state.stack[insn.a()].to_boolean();
                    ops::do_conditional_jump(&mut state, &closure.proto, insn, cond);
                }
                opcode if opcode == OpCode::TestSet as u8 => {
                    let rb = state.stack[insn.b()];
                    let cond = rb.to_boolean();
                    if cond == insn.k() {
                        state.stack[insn.a()] = rb;
                        let next_insn = closure.proto.code[state.pc];
                        state.pc = (state.pc as isize + next_insn.sj() as isize + 1) as usize;
                    } else {
                        state.pc += 1;
                    }
                }
                opcode if opcode == OpCode::Call as u8 => {
                    let a = insn.a();
                    let b = insn.b();

                    thread_ref.current_lua_frame().pc = state.pc;
                    thread_ref.stack.truncate(if b > 0 {
                        saved_current_frame.base + a + b
                    } else {
                        saved_stack_top
                    });
                    thread_ref.deferred_call(saved_current_frame.base + a)?;

                    return Ok(());
                }
                opcode if opcode == OpCode::TailCall as u8 => {
                    let a = insn.a();
                    let b = insn.b();
                    if insn.k() {
                        thread_ref.close_upvalues(gc, saved_current_frame.bottom);
                    }
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - saved_current_frame.base
                    };
                    thread_ref.stack.copy_within(
                        saved_current_frame.base + a
                            ..saved_current_frame.base + a + num_results + 1,
                        saved_current_frame.bottom,
                    );
                    thread_ref
                        .stack
                        .truncate(saved_current_frame.bottom + num_results + 1);
                    if b > 0 {
                        thread_ref.stack.truncate(saved_current_frame.bottom + b);
                    }
                    thread_ref.frames.pop().unwrap();
                    thread_ref.deferred_call(saved_current_frame.bottom)?;

                    return Ok(());
                }
                opcode if opcode == OpCode::Return as u8 => {
                    if insn.k() {
                        thread_ref.close_upvalues(gc, saved_current_frame.bottom);
                    }
                    let a = insn.a();
                    let b = insn.b();
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - saved_current_frame.base
                    };
                    thread_ref.stack.copy_within(
                        saved_current_frame.base + a..saved_current_frame.base + a + num_results,
                        saved_current_frame.bottom,
                    );
                    thread_ref
                        .stack
                        .truncate(saved_current_frame.bottom + num_results);
                    thread_ref.frames.pop().unwrap();
                    return Ok(());
                }
                opcode if opcode == OpCode::Return0 as u8 => {
                    thread_ref.stack.truncate(saved_current_frame.bottom);
                    thread_ref.frames.pop().unwrap();
                    return Ok(());
                }
                opcode if opcode == OpCode::Return1 as u8 => {
                    thread_ref.stack[saved_current_frame.bottom] = state.stack[insn.a()];
                    thread_ref.stack.truncate(saved_current_frame.bottom + 1);
                    thread_ref.frames.pop().unwrap();
                    return Ok(());
                }
                opcode if opcode == OpCode::ForLoop as u8 => {
                    let a = insn.a();
                    if let Some(step) = state.stack[a + 2].to_integer() {
                        let count = state.stack[a + 1].to_integer().unwrap();
                        if count > 0 {
                            let index = state.stack[a].to_integer().unwrap();
                            state.stack[a + 1] = (count - 1).into();
                            let index = Value::from(index + step);
                            state.stack[a] = index;
                            state.stack[a + 3] = index;
                            state.pc -= insn.bx();
                        }
                    } else {
                        todo!("float FORLOOP")
                    }
                }
                opcode if opcode == OpCode::ForPrep as u8 => {
                    let a = insn.a();
                    if let (Some(init), Some(limit), Some(step)) = (
                        state.stack[a].to_integer(),
                        state.stack[a + 1].to_integer(),
                        state.stack[a + 2].to_integer(),
                    ) {
                        assert!(step != 0);
                        let skip = if step > 0 { init > limit } else { init < limit };
                        if skip {
                            state.pc += insn.bx() + 1;
                        } else {
                            state.stack[a + 3] = state.stack[a];
                            let count = if step > 0 {
                                (limit - init) / step
                            } else {
                                (init - limit) / (-(step + 1) + 1)
                            };
                            state.stack[a + 1] = count.into();
                        }
                    } else {
                        todo!("float FORPREP")
                    }
                }
                opcode if opcode == OpCode::TForPrep as u8 => state.pc += insn.bx(),
                opcode if opcode == OpCode::TForCall as u8 => {
                    let a = insn.a();
                    thread_ref.current_lua_frame().pc = state.pc;

                    let arg_base = saved_current_frame.base + a;
                    let new_bottom = arg_base + 4;
                    thread_ref.stack.resize(new_bottom + 3, Value::Nil);
                    thread_ref
                        .stack
                        .copy_within(arg_base..arg_base + 3, new_bottom);
                    thread_ref.deferred_call(new_bottom)?;

                    return Ok(());
                }
                opcode if opcode == OpCode::TForLoop as u8 => {
                    let a = insn.a();
                    let control = state.stack[a + 4];
                    if !control.is_nil() {
                        state.stack[a + 2] = control;
                        state.pc -= insn.bx();
                    }
                }
                opcode if opcode == OpCode::SetList as u8 => {
                    let a = insn.a();
                    let n = if insn.b() > 0 {
                        Some(insn.b())
                    } else {
                        saved_stack_top.checked_sub(a + saved_current_frame.base + 1)
                    };
                    if let Some(n) = n {
                        let mut offset = insn.c() as usize;
                        if insn.k() {
                            let next_insn = closure.proto.code[state.pc];
                            offset += next_insn.ax() * (u8::MAX as usize + 1);
                            state.pc += 1;
                        }

                        let ra = state.stack[a];
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
                        for (i, x) in state.stack[a + 1..=a + n].iter().cloned().enumerate() {
                            table.set((offset + i + 1) as Integer, x)?;
                        }
                    }
                }
                opcode if opcode == OpCode::Closure as u8 => {
                    thread_ref.current_lua_frame().pc = state.pc;
                    let proto = closure.proto.protos[insn.bx()];
                    let upvalues = proto
                        .upvalues
                        .iter()
                        .map(|desc| match desc {
                            UpvalueDescription::Register(index) => {
                                let index = saved_current_frame.base + index.0 as usize;
                                *thread_ref.open_upvalues.entry(index).or_insert_with(|| {
                                    gc.allocate_cell(Upvalue::Open { thread, index })
                                })
                            }
                            UpvalueDescription::Upvalue(index) => {
                                closure.upvalues[index.0 as usize]
                            }
                        })
                        .collect();
                    thread_ref.stack[saved_current_frame.base + insn.a()] =
                        gc.allocate(LuaClosure { proto, upvalues }).into();
                    return Ok(());
                }
                opcode if opcode == OpCode::VarArg as u8 => {
                    let a = insn.a();
                    let n = insn.c();
                    let num_wanted = if n > 0 {
                        n as usize - 1
                    } else {
                        saved_current_frame.num_extra_args
                    };

                    thread_ref.current_lua_frame().pc = state.pc;
                    thread_ref
                        .stack
                        .resize(saved_current_frame.base + a + num_wanted, Value::Nil);

                    if num_wanted > 0 {
                        let extra_args_bottom =
                            saved_current_frame.base - 1 - saved_current_frame.num_extra_args;
                        let num_copied = num_wanted.min(saved_current_frame.num_extra_args);
                        thread_ref.stack.copy_within(
                            extra_args_bottom..extra_args_bottom + num_copied,
                            saved_current_frame.base + a,
                        );

                        if num_wanted > saved_current_frame.num_extra_args {
                            thread_ref.stack[saved_current_frame.base
                                + a
                                + saved_current_frame.num_extra_args
                                ..saved_current_frame.base + a + num_wanted]
                                .fill(Value::Nil);
                        }
                    }

                    return Ok(());
                }
                opcode if opcode == OpCode::VarArgPrep as u8 => {
                    let num_fixed_args = insn.a();
                    let num_extra_args =
                        saved_stack_top - saved_current_frame.bottom - 1 - num_fixed_args;
                    if num_extra_args > 0 {
                        let new_base = saved_stack_top + 1;
                        {
                            let pc = state.pc;
                            let frame = thread_ref.current_lua_frame();
                            frame.pc = pc;
                            frame.base = new_base;
                            frame.num_extra_args = num_extra_args;
                        }

                        let new_stack_len = new_base + closure.proto.max_stack_size as usize;
                        if thread_ref.stack.len() < new_stack_len {
                            thread_ref.stack.resize(new_stack_len, Value::Nil);
                        }

                        thread_ref.stack.copy_within(
                            saved_current_frame.bottom
                                ..saved_current_frame.bottom + num_fixed_args + 1,
                            saved_stack_top,
                        );
                        thread_ref.stack[saved_current_frame.bottom + 1
                            ..saved_current_frame.bottom + num_fixed_args + 1]
                            .fill(Value::Nil);

                        return Ok(());
                    }
                }
                opcode if opcode == OpCode::ExtraArg as u8 => unreachable!(),
                _ => panic!("unknown opcode"),
            }

            if gc.should_perform_gc() {
                thread_ref.current_lua_frame().pc = state.pc;
                return Ok(());
            }
        }
    }
}
