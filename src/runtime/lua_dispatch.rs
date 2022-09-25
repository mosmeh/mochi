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
            base: saved_current_frame.base,
            pc: saved_current_frame.pc,
            stack,
            lower_stack,
        };

        loop {
            let insn = closure.proto.code[state.pc];
            state.pc += 1;

            match insn.opcode() {
                OpCode::Move => state.stack[insn.a()] = state.stack[insn.b()],
                OpCode::LoadI => state.stack[insn.a()] = Value::Integer(insn.sbx() as Integer),
                OpCode::LoadF => state.stack[insn.a()] = Value::Number(insn.sbx() as Number),
                OpCode::LoadK => {
                    state.stack[insn.a()] = closure.proto.constants[insn.bx()];
                }
                OpCode::LoadKX => {
                    let next_insn = closure.proto.code[state.pc];
                    let rb = closure.proto.constants[next_insn.ax()];
                    state.stack[insn.a()] = rb;
                    state.pc += 1;
                }
                OpCode::LoadFalse => state.stack[insn.a()] = Value::Boolean(false),
                OpCode::LFalseSkip => {
                    state.stack[insn.a()] = Value::Boolean(false);
                    state.pc += 1;
                }
                OpCode::LoadTrue => state.stack[insn.a()] = Value::Boolean(true),
                OpCode::LoadNil => state.stack[insn.a()..][..=insn.b()].fill(Value::Nil),
                OpCode::GetUpval => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let value = state.resolve_upvalue(&upvalue);
                    state.stack[insn.a()] = value;
                }
                OpCode::SetUpval => {
                    let value = state.stack[insn.a()];
                    let mut upvalue = closure.upvalues[insn.b()].borrow_mut(gc);
                    *state.resolve_upvalue_mut(&mut upvalue) = value;
                }
                OpCode::GetTabUp => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let table_value = state.resolve_upvalue(&upvalue);
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
                OpCode::GetTable => {
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
                OpCode::GetI => {
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
                OpCode::GetField => {
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
                OpCode::SetTabUp => {
                    let kb = if let Value::String(s) = closure.proto.constants[insn.b()] {
                        s
                    } else {
                        unreachable!();
                    };
                    let upvalue = closure.upvalues[insn.a()].borrow();
                    let table_value = state.resolve_upvalue(&upvalue);
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
                OpCode::SetTable => {
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
                OpCode::SetI => {
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
                OpCode::SetField => {
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
                OpCode::NewTable => {
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
                OpCode::Self_ => {
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
                            saved_current_frame.base + insn.a(),
                        );
                    }
                    state.stack[insn.a()] = raw_value;
                }
                OpCode::AddI => ops::do_arithmetic_with_immediate(
                    &mut state,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                OpCode::AddK => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_add,
                    Number::add,
                ),
                OpCode::SubK => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_sub,
                    Number::sub,
                ),
                OpCode::MulK => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::wrapping_mul,
                    Number::mul,
                ),
                OpCode::ModK => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    ops::modi,
                    ops::modf,
                ),
                OpCode::PowK => ops::do_float_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Number::powf,
                ),
                OpCode::DivK => ops::do_float_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Number::div,
                ),
                OpCode::IDivK => ops::do_arithmetic_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    ops::idivi,
                    ops::idivf,
                ),
                OpCode::BAndK => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitand,
                ),
                OpCode::BOrK => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitor,
                ),
                OpCode::BXorK => ops::do_bitwise_op_with_constant(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::bitxor,
                ),
                OpCode::ShrI => todo!("SHRI"),
                OpCode::ShlI => todo!("SHLI"),
                OpCode::Add => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_add, Number::add)
                }
                OpCode::Sub => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_sub, Number::sub)
                }
                OpCode::Mul => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_mul, Number::mul)
                }
                OpCode::Mod => ops::do_arithmetic(&mut state, insn, ops::modi, ops::modf),
                OpCode::Pow => ops::do_float_arithmetic(&mut state, insn, Number::powf),
                OpCode::Div => ops::do_float_arithmetic(&mut state, insn, Number::div),
                OpCode::IDiv => ops::do_arithmetic(&mut state, insn, ops::idivi, ops::idivf),
                OpCode::BAnd => ops::do_bitwise_op(&mut state, insn, Integer::bitand),
                OpCode::BOr => ops::do_bitwise_op(&mut state, insn, Integer::bitor),
                OpCode::BXor => ops::do_bitwise_op(&mut state, insn, Integer::bitxor),
                OpCode::Shr => ops::do_bitwise_op(&mut state, insn, Integer::shr),
                OpCode::Shl => ops::do_bitwise_op(&mut state, insn, Integer::shl),
                OpCode::MmBin => {
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
                OpCode::MmBinI => todo!("MMBINI"),
                OpCode::MmBinK => todo!("MMBINK"),
                OpCode::Unm => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Value::Integer(x) = rb {
                        Value::Integer(-x)
                    } else if let Some(x) = rb.to_number_without_string_coercion() {
                        Value::Number(-x)
                    } else {
                        todo!("__unm")
                    };
                }
                OpCode::BNot => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Some(x) = rb.to_integer_without_string_coercion()
                    {
                        Value::Integer(!x)
                    } else {
                        todo!("__bnot")
                    }
                }
                OpCode::Not => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = Value::Boolean(!rb.to_boolean())
                }
                OpCode::Len => {
                    let rb = state.stack[insn.b()];
                    let len = match rb {
                        Value::String(s) => s.len() as Integer,
                        Value::Table(t) => t.borrow().lua_len(),
                        _ => todo!("__len"),
                    };
                    state.stack[insn.a()] = len.into();
                }
                OpCode::Concat => {
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
                OpCode::Close => {
                    thread_ref.current_lua_frame().pc = state.pc;
                    thread_ref.close_upvalues(gc, saved_current_frame.base + insn.a());
                    return Ok(());
                }
                OpCode::Tbc => todo!("TBC"),
                OpCode::Jmp => state.pc = (state.pc as isize + insn.sj() as isize) as usize,
                OpCode::Eq => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::eq,
                    Number::eq,
                    PartialEq::eq,
                ),
                OpCode::Lt => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::lt,
                    Number::lt,
                    PartialOrd::lt,
                ),
                OpCode::Le => ops::do_comparison(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::le,
                    Number::le,
                    PartialOrd::le,
                ),
                OpCode::EqK => {
                    let ra = state.stack[insn.a()];
                    let rb = closure.proto.constants[insn.b()];
                    let cond = ra == rb;
                    ops::do_conditional_jump(&mut state, &closure.proto, insn, cond)
                }
                OpCode::EqI => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::eq,
                    Number::eq,
                ),
                OpCode::LtI => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::lt,
                    Number::lt,
                ),
                OpCode::LeI => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::le,
                    Number::le,
                ),
                OpCode::GtI => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::gt,
                    Number::gt,
                ),
                OpCode::GeI => ops::do_comparison_with_immediate(
                    &mut state,
                    &closure.proto,
                    insn,
                    Integer::ge,
                    Number::ge,
                ),
                OpCode::Test => {
                    let cond = state.stack[insn.a()].to_boolean();
                    ops::do_conditional_jump(&mut state, &closure.proto, insn, cond);
                }
                OpCode::TestSet => {
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
                OpCode::Call => {
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
                OpCode::TailCall => {
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
                OpCode::Return => {
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
                OpCode::Return0 => {
                    thread_ref.stack.truncate(saved_current_frame.bottom);
                    thread_ref.frames.pop().unwrap();
                    return Ok(());
                }
                OpCode::Return1 => {
                    thread_ref.stack[saved_current_frame.bottom] = state.stack[insn.a()];
                    thread_ref.stack.truncate(saved_current_frame.bottom + 1);
                    thread_ref.frames.pop().unwrap();
                    return Ok(());
                }
                OpCode::ForLoop => {
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
                OpCode::ForPrep => {
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
                OpCode::TForPrep => state.pc += insn.bx(),
                OpCode::TForCall => {
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
                OpCode::TForLoop => {
                    let a = insn.a();
                    let control = state.stack[a + 4];
                    if !control.is_nil() {
                        state.stack[a + 2] = control;
                        state.pc -= insn.bx();
                    }
                }
                OpCode::SetList => {
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
                OpCode::Closure => {
                    thread_ref.current_lua_frame().pc = state.pc;
                    let proto = closure.proto.protos[insn.bx()];
                    let upvalues = proto
                        .upvalues
                        .iter()
                        .map(|desc| match desc {
                            UpvalueDescription::Register(index) => {
                                let index = saved_current_frame.base + index.0 as usize;
                                *thread_ref
                                    .open_upvalues
                                    .entry(index)
                                    .or_insert_with(|| gc.allocate_cell(Upvalue::Open(index)))
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
                OpCode::VarArg => {
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
                OpCode::VarArgPrep => {
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
                OpCode::ExtraArg => unreachable!(),
            }

            if gc.should_perform_gc() {
                thread_ref.current_lua_frame().pc = state.pc;
                return Ok(());
            }
        }
    }
}
