use super::{opcode, ops, ErrorKind, Frame, LuaFrame, Metamethod, Operation, Vm};
use crate::{
    gc::GcContext,
    types::{Integer, Number, Table, Upvalue, UpvalueDescription, Value},
    LuaClosure,
};
use std::{
    cmp::PartialOrd,
    ops::{Add, BitAnd, BitOr, BitXor, ControlFlow, Div, Mul, Sub},
};

impl<'gc> Vm<'gc> {
    pub(super) fn execute_lua_frame(&self, gc: &'gc GcContext) -> Result<(), ErrorKind> {
        let thread = self.current_thread();
        let mut thread_ref = thread.borrow_mut(gc);

        'start: loop {
            let frame = match thread_ref.frames.as_slice() {
                [.., Frame::Lua(frame)] => frame.clone(),
                _ => unreachable!(),
            };
            let LuaFrame {
                bottom,
                base,
                mut pc,
                num_extra_args,
            } = frame;

            let bottom_value = thread_ref.stack[bottom];
            let closure = bottom_value.as_lua_closure().unwrap();
            let upvalues = closure.upvalues.as_slice();
            let proto = closure.proto.as_ref();
            let code = proto.code.as_ref();
            let constants = proto.constants.as_ref();

            let saved_stack_top = thread_ref.stack.len();
            let new_stack_len = base + proto.max_stack_size as usize;
            if saved_stack_top < new_stack_len {
                thread_ref.stack.resize(new_stack_len, Value::Nil);
            }

            let (lower_stack, stack) = thread_ref.stack.split_at_mut(base);

            while let Some(&insn) = code.get(pc) {
                pc += 1;

                match insn.raw_opcode() {
                    opcode::MOVE => stack[insn.a()] = stack[insn.b()],
                    opcode::LOADI => stack[insn.a()] = Value::Integer(insn.sbx() as Integer),
                    opcode::LOADF => stack[insn.a()] = Value::Number(insn.sbx() as Number),
                    opcode::LOADK => stack[insn.a()] = constants[insn.bx()],
                    opcode::LOADKX => {
                        let next_insn = code[pc];
                        let rb = constants[next_insn.ax()];
                        stack[insn.a()] = rb;
                        pc += 1;
                    }
                    opcode::LOADFALSE => stack[insn.a()] = Value::Boolean(false),
                    opcode::LFALSESKIP => {
                        stack[insn.a()] = Value::Boolean(false);
                        pc += 1;
                    }
                    opcode::LOADTRUE => stack[insn.a()] = Value::Boolean(true),
                    opcode::LOADNIL => stack[insn.a()..][..=insn.b()].fill(Value::Nil),
                    opcode::GETUPVAL => {
                        let value =
                            upvalues[insn.b()]
                                .borrow()
                                .get(thread, base, lower_stack, stack);
                        stack[insn.a()] = value;
                    }
                    opcode::SETUPVAL => {
                        let value = stack[insn.a()];
                        upvalues[insn.b()].borrow_mut(gc).set(
                            gc,
                            thread,
                            base,
                            lower_stack,
                            stack,
                            value,
                        );
                    }
                    opcode::GETTABUP => {
                        let table =
                            upvalues[insn.b()]
                                .borrow()
                                .get(thread, base, lower_stack, stack);
                        let rc = match constants[insn.c() as usize] {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        };
                        let value = table.borrow_as_table().map(|table| table.get_field(rc));
                        match value {
                            Some(Value::Nil) | None => {
                                thread_ref.save_pc(pc);
                                match self.index_slow_path(
                                    &mut thread_ref,
                                    table,
                                    rc,
                                    base + insn.a(),
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            Some(v) => stack[insn.a()] = v,
                        }
                    }
                    opcode::GETTABLE => {
                        let rb = stack[insn.b()];
                        let rc = stack[insn.c() as usize];
                        let value = rb.borrow_as_table().map(|table| table.get(rc));
                        match value {
                            Some(Value::Nil) | None => {
                                thread_ref.save_pc(pc);
                                match self.index_slow_path(
                                    &mut thread_ref,
                                    rb,
                                    rc,
                                    base + insn.a(),
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            Some(v) => stack[insn.a()] = v,
                        }
                    }
                    opcode::GETI => {
                        let rb = stack[insn.b()];
                        let c = insn.c() as Integer;
                        let value = rb.borrow_as_table().map(|table| table.get(c));
                        match value {
                            Some(Value::Nil) | None => {
                                thread_ref.save_pc(pc);
                                match self.index_slow_path(
                                    &mut thread_ref,
                                    rb,
                                    c,
                                    base + insn.a(),
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            Some(v) => stack[insn.a()] = v,
                        }
                    }
                    opcode::GETFIELD => {
                        let rb = stack[insn.b()];
                        let rc = match constants[insn.c() as usize] {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        };
                        let value = rb.borrow_as_table().map(|table| table.get_field(rc));
                        match value {
                            Some(Value::Nil) | None => {
                                thread_ref.save_pc(pc);
                                match self.index_slow_path(
                                    &mut thread_ref,
                                    rb,
                                    rc,
                                    base + insn.a(),
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            Some(v) => stack[insn.a()] = v,
                        }
                    }
                    opcode::SETTABUP => {
                        let kb = match constants[insn.b()] {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        };
                        let table =
                            upvalues[insn.a()]
                                .borrow()
                                .get(thread, base, lower_stack, stack);
                        let c = insn.c() as usize;
                        let rkc = if insn.k() { constants[c] } else { stack[c] };
                        let replaced = table
                            .borrow_as_table_mut(gc)
                            .map(|mut table| table.replace_field(kb, rkc))
                            .unwrap_or_default();
                        if !replaced {
                            thread_ref.save_pc(pc);
                            match self.new_index_slow_path(gc, &mut thread_ref, table, kb, rkc)? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        }
                    }
                    opcode::SETTABLE => {
                        let ra = stack[insn.a()];
                        let rb = stack[insn.b()];
                        let c = insn.c() as usize;
                        let rkc = if insn.k() { constants[c] } else { stack[c] };
                        let replaced = ra
                            .borrow_as_table_mut(gc)
                            .map(|mut table| table.replace(rb, rkc))
                            .transpose()?
                            .unwrap_or_default();
                        if !replaced {
                            thread_ref.save_pc(pc);
                            match self.new_index_slow_path(gc, &mut thread_ref, ra, rb, rkc)? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        }
                    }
                    opcode::SETI => {
                        let ra = stack[insn.a()];
                        let b = insn.b() as Integer;
                        let c = insn.c() as usize;
                        let rkc = if insn.k() { constants[c] } else { stack[c] };
                        let replaced = ra
                            .borrow_as_table_mut(gc)
                            .map(|mut table| table.replace(b, rkc))
                            .transpose()?
                            .unwrap_or_default();
                        if !replaced {
                            thread_ref.save_pc(pc);
                            match self.new_index_slow_path(gc, &mut thread_ref, ra, b, rkc)? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        }
                    }
                    opcode::SETFIELD => {
                        let ra = stack[insn.a()];
                        let kb = match constants[insn.b()] {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        };
                        let c = insn.c() as usize;
                        let rkc = if insn.k() { constants[c] } else { stack[c] };
                        let replaced = ra
                            .borrow_as_table_mut(gc)
                            .map(|mut table| table.replace_field(kb, rkc))
                            .unwrap_or_default();
                        if !replaced {
                            thread_ref.save_pc(pc);
                            match self.new_index_slow_path(gc, &mut thread_ref, ra, kb, rkc)? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        }
                    }
                    opcode::NEWTABLE => {
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
                        if gc.should_perform_gc() {
                            thread_ref.save_pc(pc);
                            return Ok(());
                        }
                    }
                    opcode::SELF => {
                        let a = insn.a();
                        let rb = stack[insn.b()];
                        stack[a + 1] = rb;
                        let c = insn.c() as usize;
                        let rkc = if insn.k() { constants[c] } else { stack[c] };
                        let rkc = match rkc {
                            Value::String(s) => s,
                            _ => unreachable!(),
                        };
                        let value = rb.borrow_as_table().map(|table| table.get_field(rkc));
                        match value {
                            Some(Value::Nil) | None => {
                                thread_ref.save_pc(pc);
                                match self.index_slow_path(&mut thread_ref, rb, rkc, base + a)? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            Some(v) => stack[a] = v,
                        }
                    }
                    opcode::ADDI => ops::do_arithmetic_with_immediate(
                        stack,
                        &mut pc,
                        insn,
                        Integer::wrapping_add,
                        Number::add,
                    ),
                    opcode::ADDK => ops::do_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::wrapping_add,
                        Number::add,
                    ),
                    opcode::SUBK => ops::do_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::wrapping_sub,
                        Number::sub,
                    ),
                    opcode::MULK => ops::do_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::wrapping_mul,
                        Number::mul,
                    ),
                    opcode::MODK => ops::do_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        ops::modi,
                        ops::modf,
                    ),
                    opcode::POWK => ops::do_float_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Number::powf,
                    ),
                    opcode::DIVK => ops::do_float_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Number::div,
                    ),
                    opcode::IDIVK => ops::do_arithmetic_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        ops::idivi,
                        ops::idivf,
                    ),
                    opcode::BANDK => ops::do_bitwise_op_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::bitand,
                    ),
                    opcode::BORK => ops::do_bitwise_op_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::bitor,
                    ),
                    opcode::BXORK => ops::do_bitwise_op_with_constant(
                        stack,
                        &mut pc,
                        constants,
                        insn,
                        Integer::bitxor,
                    ),
                    opcode::SHRI => {
                        let rb = stack[insn.b()];
                        if let Some(lhs) = rb.to_integer_without_string_coercion() {
                            let ic = insn.sc() as Integer;
                            stack[insn.a()] = ops::shl(lhs, -ic).into();
                            pc += 1;
                        }
                    }
                    opcode::SHLI => {
                        let rb = stack[insn.b()];
                        if let Some(rhs) = rb.to_integer_without_string_coercion() {
                            let ic = insn.sc() as Integer;
                            stack[insn.a()] = ops::shl(ic, rhs).into();
                            pc += 1;
                        }
                    }
                    opcode::ADD => {
                        ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_add, Number::add)
                    }
                    opcode::SUB => {
                        ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_sub, Number::sub)
                    }
                    opcode::MUL => {
                        ops::do_arithmetic(stack, &mut pc, insn, Integer::wrapping_mul, Number::mul)
                    }
                    opcode::MOD => ops::do_arithmetic(stack, &mut pc, insn, ops::modi, ops::modf),
                    opcode::POW => ops::do_float_arithmetic(stack, &mut pc, insn, Number::powf),
                    opcode::DIV => ops::do_float_arithmetic(stack, &mut pc, insn, Number::div),
                    opcode::IDIV => {
                        ops::do_arithmetic(stack, &mut pc, insn, ops::idivi, ops::idivf)
                    }
                    opcode::BAND => ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitand),
                    opcode::BOR => ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitor),
                    opcode::BXOR => ops::do_bitwise_op(stack, &mut pc, insn, Integer::bitxor),
                    opcode::SHR => ops::do_bitwise_op(stack, &mut pc, insn, ops::shr),
                    opcode::SHL => ops::do_bitwise_op(stack, &mut pc, insn, ops::shl),
                    opcode::MMBIN => {
                        let ra = stack[insn.a()];
                        let rb = stack[insn.b()];
                        let prev_insn = code[pc - 2];

                        let dest = base + prev_insn.a();
                        let metamethod = Metamethod::from(insn.c());

                        thread_ref.save_pc(pc);
                        match self.arithmetic_slow_path(
                            &mut thread_ref,
                            metamethod,
                            ra,
                            rb,
                            dest,
                        )? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::MMBINI => {
                        let ra = stack[insn.a()];
                        let imm = (insn.sb() as Integer).into();
                        let metamethod = Metamethod::from(insn.c());
                        let prev_insn = code[pc - 2];
                        let dest = base + prev_insn.a();
                        let (a, b) = if insn.k() { (imm, ra) } else { (ra, imm) };

                        thread_ref.save_pc(pc);
                        match self.arithmetic_slow_path(&mut thread_ref, metamethod, a, b, dest)? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::MMBINK => {
                        let ra = stack[insn.a()];
                        let imm = constants[insn.b()];
                        let metamethod = Metamethod::from(insn.c());
                        let prev_insn = code[pc - 2];
                        let dest = base + prev_insn.a();
                        let (a, b) = if insn.k() { (imm, ra) } else { (ra, imm) };

                        thread_ref.save_pc(pc);
                        match self.arithmetic_slow_path(&mut thread_ref, metamethod, a, b, dest)? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::UNM => {
                        let a = insn.a();
                        let rb = stack[insn.b()];
                        let result = if let Value::Integer(x) = rb {
                            Value::Integer(x.wrapping_neg())
                        } else if let Some(x) = rb.to_number_without_string_coercion() {
                            Value::Number(-x)
                        } else {
                            thread_ref.save_pc(pc);
                            match self.arithmetic_slow_path(
                                &mut thread_ref,
                                Metamethod::Unm,
                                rb,
                                rb,
                                base + a,
                            )? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        };
                        stack[a] = result;
                    }
                    opcode::BNOT => {
                        let a = insn.a();
                        let rb = stack[insn.b()];
                        let result = if let Some(x) = rb.to_integer_without_string_coercion() {
                            Value::Integer(!x)
                        } else {
                            thread_ref.save_pc(pc);
                            match self.arithmetic_slow_path(
                                &mut thread_ref,
                                Metamethod::BNot,
                                rb,
                                rb,
                                base + a,
                            )? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        };
                        stack[a] = result;
                    }
                    opcode::NOT => {
                        let rb = stack[insn.b()];
                        stack[insn.a()] = Value::Boolean(!rb.to_boolean())
                    }
                    opcode::LEN => {
                        let a = insn.a();
                        let rb = stack[insn.b()];
                        let len = match rb {
                            Value::String(s) => s.len() as Integer,
                            Value::Table(table) => {
                                if self.metamethod_of_object(Metamethod::Len, rb).is_some() {
                                    thread_ref.save_pc(pc);
                                    match self.len_slow_path(&mut thread_ref, rb, base + a)? {
                                        ControlFlow::Continue(()) => continue 'start,
                                        ControlFlow::Break(()) => return Ok(()),
                                    }
                                } else {
                                    table.borrow().lua_len()
                                }
                            }
                            _ => {
                                thread_ref.save_pc(pc);
                                match self.len_slow_path(&mut thread_ref, rb, base + a)? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        };
                        stack[a] = len.into();
                    }
                    opcode::CONCAT => {
                        let b = insn.b();
                        if b >= 1 {
                            let a = insn.a();
                            let mut strings = Vec::with_capacity(b);
                            for (i, value) in stack[a..].iter().take(b).enumerate().rev() {
                                if let Some(string) = value.to_string() {
                                    strings.push(string);
                                    continue;
                                }
                                let (lhs_index, rhs) = match strings.len() {
                                    0 => (i - 1, *value),
                                    1 => (i, stack[a + i + 1]),
                                    _ => {
                                        strings.reverse();
                                        (i, gc.allocate_string(strings.concat()).into())
                                    }
                                };
                                thread_ref.save_pc(pc);
                                match self.concat_slow_path(
                                    &mut thread_ref,
                                    lhs_index,
                                    rhs,
                                    base + a,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                            strings.reverse();
                            stack[a] = gc.allocate_string(strings.concat()).into();
                            if gc.should_perform_gc() {
                                thread_ref.save_pc(pc);
                                return Ok(());
                            }
                        }
                    }
                    opcode::CLOSE => {
                        thread_ref.save_pc(pc);
                        thread_ref.close_upvalues(gc, base + insn.a());
                        continue 'start;
                    }
                    opcode::TBC => todo!("TBC"),
                    opcode::JMP => pc = (pc as isize + insn.sj() as isize) as usize,
                    opcode::EQ => {
                        let ra = stack[insn.a()];
                        let rb = stack[insn.b()];
                        if ra == rb {
                            ops::do_conditional_jump(&mut pc, code, insn, true);
                        } else if self.metamethod_of_object(Metamethod::Eq, ra).is_some()
                            || self.metamethod_of_object(Metamethod::Eq, rb).is_some()
                        {
                            thread_ref.save_pc(pc);
                            match self.compare_slow_path(
                                &mut thread_ref,
                                Metamethod::Eq,
                                ra,
                                rb,
                                pc,
                                code,
                            )? {
                                ControlFlow::Continue(()) => continue 'start,
                                ControlFlow::Break(()) => return Ok(()),
                            }
                        } else {
                            ops::do_conditional_jump(&mut pc, code, insn, false);
                        }
                    }
                    opcode::LT => {
                        let ra = stack[insn.a()];
                        let rb = stack[insn.b()];
                        match ops::lt(ra, rb) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Lt,
                                    ra,
                                    rb,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::LE => {
                        let ra = stack[insn.a()];
                        let rb = stack[insn.b()];
                        match ops::le(ra, rb) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Le,
                                    ra,
                                    rb,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::EQK => {
                        let ra = stack[insn.a()];
                        let rb = constants[insn.b()];
                        let cond = ra == rb;
                        ops::do_conditional_jump(&mut pc, code, insn, cond)
                    }
                    opcode::EQI => {
                        let ra = stack[insn.a()];
                        let imm = insn.sb();
                        let cond = match ra {
                            Value::Integer(x) => x == imm as Integer,
                            Value::Number(x) => x == imm as Number,
                            _ => false,
                        };
                        ops::do_conditional_jump(&mut pc, code, insn, cond)
                    }
                    opcode::LTI => {
                        let ra = stack[insn.a()];
                        let imm = insn.sb();
                        match ops::compare_with_immediate(ra, imm, PartialOrd::lt, PartialOrd::lt) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                let imm = if insn.c() == 0 {
                                    (imm as Integer).into()
                                } else {
                                    (imm as Number).into()
                                };
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Lt,
                                    ra,
                                    imm,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::LEI => {
                        let ra = stack[insn.a()];
                        let imm = insn.sb();
                        match ops::compare_with_immediate(ra, imm, PartialOrd::le, PartialOrd::le) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                let imm = if insn.c() == 0 {
                                    (imm as Integer).into()
                                } else {
                                    (imm as Number).into()
                                };
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Le,
                                    ra,
                                    imm,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::GTI => {
                        let ra = stack[insn.a()];
                        let imm = insn.sb();
                        match ops::compare_with_immediate(ra, imm, PartialOrd::gt, PartialOrd::gt) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                let imm = if insn.c() == 0 {
                                    (imm as Integer).into()
                                } else {
                                    (imm as Number).into()
                                };
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Lt,
                                    imm,
                                    ra,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::GEI => {
                        let ra = stack[insn.a()];
                        let imm = insn.sb();
                        match ops::compare_with_immediate(ra, imm, PartialOrd::ge, PartialOrd::ge) {
                            Some(cond) => ops::do_conditional_jump(&mut pc, code, insn, cond),
                            None => {
                                let imm = if insn.c() == 0 {
                                    (imm as Integer).into()
                                } else {
                                    (imm as Number).into()
                                };
                                thread_ref.save_pc(pc);
                                match self.compare_slow_path(
                                    &mut thread_ref,
                                    Metamethod::Le,
                                    imm,
                                    ra,
                                    pc,
                                    code,
                                )? {
                                    ControlFlow::Continue(()) => continue 'start,
                                    ControlFlow::Break(()) => return Ok(()),
                                }
                            }
                        }
                    }
                    opcode::TEST => {
                        let cond = stack[insn.a()].to_boolean();
                        ops::do_conditional_jump(&mut pc, code, insn, cond);
                    }
                    opcode::TESTSET => {
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
                    opcode::CALL => {
                        let a = insn.a();
                        let b = insn.b();

                        thread_ref.save_pc(pc);
                        thread_ref.stack.truncate(if b > 0 {
                            base + a + b
                        } else {
                            saved_stack_top
                        });
                        match self.push_frame(&mut thread_ref, base + a)? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::TAILCALL => {
                        let a = insn.a();
                        let b = insn.b();
                        if insn.k() {
                            thread_ref.close_upvalues(gc, bottom);
                        }
                        let num_results = if b > 0 {
                            b - 1
                        } else {
                            saved_stack_top - a - base - 1
                        };
                        thread_ref
                            .stack
                            .copy_within(base + a..base + a + num_results + 1, bottom);
                        thread_ref.stack.truncate(bottom + num_results + 1);
                        if b > 0 {
                            thread_ref.stack.truncate(bottom + b);
                        }
                        thread_ref.frames.pop().unwrap();
                        match self.push_frame(&mut thread_ref, bottom)? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::RETURN => {
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
                        match thread_ref.frames.as_slice() {
                            [.., Frame::Lua(_)] => continue 'start,
                            _ => return Ok(()),
                        }
                    }
                    opcode::RETURN0 => {
                        thread_ref.stack.truncate(bottom);
                        thread_ref.frames.pop().unwrap();
                        match thread_ref.frames.as_slice() {
                            [.., Frame::Lua(_)] => continue 'start,
                            _ => return Ok(()),
                        }
                    }
                    opcode::RETURN1 => {
                        thread_ref.stack[bottom] = stack[insn.a()];
                        thread_ref.stack.truncate(bottom + 1);
                        thread_ref.frames.pop().unwrap();
                        match thread_ref.frames.as_slice() {
                            [.., Frame::Lua(_)] => continue 'start,
                            _ => return Ok(()),
                        }
                    }
                    opcode::FORLOOP => {
                        let a = insn.a();
                        let next_index = match stack[a + 2] {
                            Value::Integer(step) => {
                                let count = match stack[a + 1] {
                                    Value::Integer(i) => i,
                                    _ => unreachable!(),
                                };
                                if count > 0 {
                                    let index = match stack[a] {
                                        Value::Integer(i) => i,
                                        _ => unreachable!(),
                                    };
                                    stack[a + 1] = count.wrapping_sub(1).into();
                                    Some(index.wrapping_add(step).into())
                                } else {
                                    None
                                }
                            }
                            Value::Number(step) => {
                                let (index, limit) = match (stack[a], stack[a + 1]) {
                                    (Value::Number(index), Value::Number(limit)) => (index, limit),
                                    _ => unreachable!(),
                                };
                                let next_index = index + step;
                                if step >= 0.0 {
                                    limit >= next_index
                                } else {
                                    next_index >= limit
                                }
                                .then_some(next_index.into())
                            }
                            _ => unreachable!(),
                        };
                        if let Some(next_index) = next_index {
                            stack[a] = next_index;
                            stack[a + 3] = next_index;
                            pc -= insn.bx();
                        }
                    }
                    opcode::FORPREP => {
                        if !ops::do_forprep(&mut stack[insn.a()..])? {
                            pc += insn.bx() + 1;
                        }
                    }
                    opcode::TFORPREP => pc += insn.bx(),
                    opcode::TFORCALL => {
                        let a = insn.a();
                        thread_ref.save_pc(pc);

                        let arg_base = base + a;
                        let new_bottom = arg_base + 4;
                        thread_ref.stack.resize(new_bottom + 3, Value::Nil);
                        thread_ref
                            .stack
                            .copy_within(arg_base..arg_base + 3, new_bottom);
                        match self.push_frame(&mut thread_ref, new_bottom)? {
                            ControlFlow::Continue(()) => continue 'start,
                            ControlFlow::Break(()) => return Ok(()),
                        }
                    }
                    opcode::TFORLOOP => {
                        let a = insn.a();
                        let control = stack[a + 4];
                        if !control.is_nil() {
                            stack[a + 2] = control;
                            pc -= insn.bx();
                        }
                    }
                    opcode::SETLIST => {
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
                            for (i, x) in stack[a + 1..=a + n].iter().copied().enumerate() {
                                table.set((offset + i + 1) as Integer, x)?;
                            }
                        }
                    }
                    opcode::CLOSURE => {
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
                        thread_ref.save_pc(pc);
                        if gc.should_perform_gc() {
                            return Ok(());
                        } else {
                            continue 'start;
                        }
                    }
                    opcode::VARARG => {
                        let a = insn.a();
                        let n = insn.c();
                        let num_wanted = if n > 0 {
                            n as usize - 1
                        } else {
                            num_extra_args
                        };

                        thread_ref.save_pc(pc);
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

                        continue 'start;
                    }
                    opcode::VARARGPREP => {
                        let num_fixed_args = insn.a();
                        let new_num_extra_args =
                            saved_stack_top.saturating_sub(bottom + 1 + num_fixed_args);
                        if new_num_extra_args > 0 {
                            let new_base = saved_stack_top + 1;
                            match thread_ref.frames.as_mut_slice() {
                                [.., Frame::Lua(frame)] => {
                                    frame.pc = pc;
                                    frame.base = new_base;
                                    frame.num_extra_args = new_num_extra_args;
                                }
                                _ => unreachable!(),
                            };

                            let new_stack_len = new_base + proto.max_stack_size as usize;
                            if thread_ref.stack.len() < new_stack_len {
                                thread_ref.stack.resize(new_stack_len, Value::Nil);
                            }

                            thread_ref
                                .stack
                                .copy_within(bottom..bottom + num_fixed_args + 1, saved_stack_top);
                            thread_ref.stack[bottom + 1..bottom + num_fixed_args + 1]
                                .fill(Value::Nil);

                            continue 'start;
                        }
                    }
                    _ => unreachable!(),
                }
            }

            unreachable!()
        }
    }
}
