use super::{ops, ErrorKind, OpCode, Operation, Root, State, Vm};
use crate::{
    gc::GcHeap,
    types::{Integer, Number, Table, Upvalue, Value},
    LuaClosure,
};
use std::{
    cmp::PartialOrd,
    num::NonZeroUsize,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub},
};

impl<'gc> Vm<'gc> {
    pub(super) fn execute_frame(&mut self, heap: &'gc GcHeap) -> Result<(), ErrorKind> {
        let frame = self.frames.last().unwrap().clone();

        let bottom_value = self.stack[frame.bottom];
        let closure = bottom_value.as_lua_closure().unwrap();

        let saved_stack_top = self.stack.len();
        let new_stack_len = frame.base + closure.proto.max_stack_size as usize;
        if self.stack.len() < new_stack_len {
            self.stack.resize(new_stack_len, Value::Nil);
        }

        let (lower_stack, stack) = self.stack.split_at_mut(frame.base);
        let mut state = State {
            base: frame.base,
            pc: frame.pc,
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
                OpCode::LoadNil => state.stack[insn.a()..][..insn.b()].fill(Value::Nil),
                OpCode::GetUpval => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let value = state.resolve_upvalue(&upvalue);
                    state.stack[insn.a()] = *value;
                }
                OpCode::SetUpval => {
                    let value = state.stack[insn.a()];
                    let mut upvalue = closure.upvalues[insn.b()].borrow_mut(heap);
                    *state.resolve_upvalue_mut(&mut upvalue) = value;
                }
                OpCode::GetTabUp => {
                    let upvalue = closure.upvalues[insn.b()].borrow();
                    let value = state.resolve_upvalue(&upvalue);
                    let rc = if let Value::String(s) = closure.proto.constants[insn.c() as usize] {
                        s
                    } else {
                        unreachable!();
                    };
                    state.stack[insn.a()] = {
                        let table = value.as_table().ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: value.ty(),
                        })?;
                        table.get_field(rc)
                    };
                }
                OpCode::GetTable => {
                    let rb = state.stack[insn.b()];
                    let rc = state.stack[insn.c() as usize];
                    state.stack[insn.a()] = {
                        let table = rb.as_table().ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: rb.ty(),
                        })?;
                        table.get(rc)
                    };
                }
                OpCode::GetI => {
                    let rb = state.stack[insn.b()];
                    let c = insn.c() as Integer;
                    state.stack[insn.a()] = {
                        let table = rb.as_table().ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: rb.ty(),
                        })?;
                        table.get(c)
                    };
                }
                OpCode::GetField => {
                    let rb = state.stack[insn.b()];
                    let rc = if let Value::String(s) = closure.proto.constants[insn.c() as usize] {
                        s
                    } else {
                        unreachable!();
                    };
                    state.stack[insn.a()] = {
                        let table = rb.as_table().ok_or_else(|| ErrorKind::TypeError {
                            operation: Operation::Index,
                            ty: rb.ty(),
                        })?;
                        table.get_field(rc)
                    };
                }
                OpCode::SetTabUp => {
                    let kb = if let Value::String(s) = closure.proto.constants[insn.b()] {
                        s
                    } else {
                        unreachable!();
                    };
                    let upvalue = closure.upvalues[insn.a()].borrow();
                    let table_value = state.resolve_upvalue(&upvalue);
                    let mut table =
                        table_value
                            .as_table_mut(heap)
                            .ok_or_else(|| ErrorKind::TypeError {
                                operation: Operation::Index,
                                ty: table_value.ty(),
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
                    let mut table = ra.as_table_mut(heap).ok_or_else(|| ErrorKind::TypeError {
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
                    table.set(rb, rkc);
                }
                OpCode::SetI => {
                    let ra = state.stack[insn.a()];
                    let mut table = ra.as_table_mut(heap).ok_or_else(|| ErrorKind::TypeError {
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
                    table.set(b, rkc);
                }
                OpCode::SetField => {
                    let ra = state.stack[insn.a()];
                    let mut table = ra.as_table_mut(heap).ok_or_else(|| ErrorKind::TypeError {
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
                    let table = Table::with_capacity_and_len(b, c);
                    state.stack[insn.a()] = heap.allocate_cell(table).into();
                    state.pc += 1;
                }
                OpCode::Self_ => {
                    let a = insn.a();
                    let rb = state.stack[insn.b()];
                    state.stack[a + 1] = rb;
                    let table = rb.as_table().ok_or_else(|| ErrorKind::TypeError {
                        operation: Operation::Index,
                        ty: rb.ty(),
                    })?;
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
                    state.stack[a] = table.get_field(rkc);
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
                OpCode::ModK => unimplemented!("MODK"),
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
                OpCode::IDivK => unimplemented!("IDIVK"),
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
                OpCode::ShrI => unimplemented!("SHRI"),
                OpCode::ShlI => unimplemented!("SHLI"),
                OpCode::Add => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_add, Number::add)
                }
                OpCode::Sub => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_sub, Number::sub)
                }
                OpCode::Mul => {
                    ops::do_arithmetic(&mut state, insn, Integer::wrapping_mul, Number::mul)
                }
                OpCode::Mod => unimplemented!("MOD"),
                OpCode::Pow => ops::do_float_arithmetic(&mut state, insn, Number::powf),
                OpCode::Div => ops::do_float_arithmetic(&mut state, insn, Number::div),
                OpCode::IDiv => unimplemented!("IDIV"),
                OpCode::BAnd => ops::do_bitwise_op(&mut state, insn, Integer::bitand),
                OpCode::BOr => ops::do_bitwise_op(&mut state, insn, Integer::bitor),
                OpCode::BXor => ops::do_bitwise_op(&mut state, insn, Integer::bitxor),
                OpCode::Shr => ops::do_bitwise_op(&mut state, insn, Integer::shr),
                OpCode::Shl => ops::do_bitwise_op(&mut state, insn, Integer::shl),
                OpCode::MmBin => {
                    self.frames.last_mut().unwrap().pc = state.pc;

                    let ra = state.stack[insn.a()];
                    let rb = state.stack[insn.b()];
                    let prev_insn = closure.proto.code[state.pc - 2];

                    let metatable = ra.metatable().or_else(|| rb.metatable()).ok_or_else(|| {
                        ErrorKind::TypeError {
                            operation: Operation::Arithmetic,
                            ty: rb.ty(),
                        }
                    })?;

                    let tag_method_name = self.tag_method_names[insn.c() as usize];
                    let tag_method_value = metatable.borrow().get_field(tag_method_name);

                    let result = self.execute_inner(heap, tag_method_value, &[ra, rb])?;
                    self.stack[frame.base + prev_insn.a()] = result;
                    return Ok(());
                }
                OpCode::MmBinI => unimplemented!("MMBINI"),
                OpCode::MmBinK => unimplemented!("MMBINK"),
                OpCode::Unm => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Value::Integer(x) = rb {
                        Value::Integer(-x)
                    } else if let Some(x) = rb.as_number_without_string_coercion() {
                        Value::Number(-x)
                    } else {
                        unimplemented!("UNM")
                    };
                }
                OpCode::BNot => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = if let Some(x) = rb.as_integer_without_string_coercion()
                    {
                        Value::Integer(!x)
                    } else {
                        unimplemented!("BNOT")
                    }
                }
                OpCode::Not => {
                    let rb = state.stack[insn.b()];
                    state.stack[insn.a()] = Value::Boolean(!rb.as_boolean())
                }
                OpCode::Len => {
                    let rb = state.stack[insn.b()];
                    let len = match rb {
                        Value::String(s) => s.len() as Integer,
                        Value::Table(t) => t.borrow().lua_len(),
                        _ => unimplemented!("LEN"),
                    };
                    state.stack[insn.a()] = len.into();
                }
                OpCode::Concat => {
                    let a = insn.a();
                    let b = insn.b();
                    let mut strings = Vec::with_capacity(b);
                    for value in state.stack[a..].iter().take(b) {
                        if let Some(string) = value.as_lua_string(heap) {
                            strings.push(string);
                        } else {
                            return Err(ErrorKind::TypeError {
                                operation: Operation::Concatenate,
                                ty: value.ty(),
                            });
                        }
                    }
                    let strings: Vec<_> = strings.iter().map(|x| x.as_bytes()).collect();
                    state.stack[a] = heap.allocate_string(strings.concat()).into();
                }
                OpCode::Close => unimplemented!("CLOSE"),
                OpCode::Tbc => unimplemented!("TBC"),
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
                    let cond = state.stack[insn.a()].as_boolean();
                    ops::do_conditional_jump(&mut state, &closure.proto, insn, cond);
                }
                OpCode::TestSet => {
                    let rb = state.stack[insn.b()];
                    let cond = !rb.as_boolean();
                    if cond == insn.k() {
                        state.pc += 1;
                    } else {
                        state.stack[insn.a()] = rb;
                        let next_insn = closure.proto.code[state.pc];
                        state.pc = (state.pc as isize + next_insn.sj() as isize + 1) as usize;
                    }
                }
                OpCode::Call => {
                    self.frames.last_mut().unwrap().pc = state.pc;
                    let a = insn.a();
                    let callee = state.stack[a];
                    return self.call_closure(
                        heap,
                        callee,
                        frame.base + a..saved_stack_top,
                        NonZeroUsize::new(insn.b()),
                    );
                }
                OpCode::TailCall => {
                    let a = insn.a();
                    let b = insn.b();
                    let callee = state.stack[a];
                    if insn.k() {
                        self.close_upvalues(heap, frame.bottom);
                    }
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - frame.base
                    };
                    self.stack.copy_within(
                        frame.base + a..frame.base + a + num_results + 1,
                        frame.bottom,
                    );
                    let new_stack_len = frame.bottom + num_results + 1;
                    self.stack.truncate(new_stack_len);
                    self.frames.pop().unwrap();
                    return self.call_closure(
                        heap,
                        callee,
                        frame.bottom..new_stack_len,
                        NonZeroUsize::new(insn.b()),
                    );
                }
                OpCode::Return => {
                    if insn.k() {
                        self.close_upvalues(heap, frame.bottom);
                    }
                    let a = insn.a();
                    let b = insn.b();
                    let num_results = if b > 0 {
                        b - 1
                    } else {
                        saved_stack_top - a - frame.base
                    };
                    self.stack
                        .copy_within(frame.base + a..frame.base + a + num_results, frame.bottom);
                    self.stack.truncate(frame.bottom + num_results);
                    self.frames.pop().unwrap();
                    return Ok(());
                }
                OpCode::Return0 => {
                    self.stack.truncate(frame.bottom);
                    self.frames.pop().unwrap();
                    return Ok(());
                }
                OpCode::Return1 => {
                    self.stack[frame.bottom] = state.stack[insn.a()];
                    self.stack.truncate(frame.bottom + 1);
                    self.frames.pop().unwrap();
                    return Ok(());
                }
                OpCode::ForLoop => {
                    let a = insn.a();
                    if let Some(step) = state.stack[a + 2].as_integer() {
                        let count = state.stack[a + 1].as_integer().unwrap();
                        if count > 0 {
                            let index = state.stack[a].as_integer().unwrap();
                            state.stack[a + 1] = (count - 1).into();
                            let index = Value::from(index + step);
                            state.stack[a] = index;
                            state.stack[a + 3] = index;
                            state.pc -= insn.bx();
                        }
                    } else {
                        unimplemented!("FORLOOP")
                    }
                }
                OpCode::ForPrep => {
                    let a = insn.a();
                    if let (Some(init), Some(limit), Some(step)) = (
                        state.stack[a].as_integer(),
                        state.stack[a + 1].as_integer(),
                        state.stack[a + 2].as_integer(),
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
                        unimplemented!("FORPREP")
                    }
                }
                OpCode::TForPrep => unimplemented!("TFORPREP"),
                OpCode::TForCall => unimplemented!("TFORCALL"),
                OpCode::TForLoop => unimplemented!("TFORLOOP"),
                OpCode::SetList => {
                    let a = insn.a();
                    let b = insn.b();
                    let c = insn.c() as usize;
                    let ra = state.stack[a];
                    let mut table = ra.as_table_mut(heap).ok_or_else(|| ErrorKind::TypeError {
                        operation: Operation::Index,
                        ty: ra.ty(),
                    })?;
                    for (i, x) in state.stack[a + 1..=a + b].iter().cloned().enumerate() {
                        table.set((c + i + 1) as Integer, x);
                    }
                }
                OpCode::Closure => {
                    let proto = closure.proto.protos[insn.bx()];
                    let upvalues = proto
                        .upvalues
                        .iter()
                        .map(|desc| {
                            if desc.in_stack {
                                let index = frame.base + desc.index as usize;
                                *self
                                    .open_upvalues
                                    .entry(index)
                                    .or_insert_with(|| heap.allocate_cell(Upvalue::Open(index)))
                            } else {
                                closure.upvalues[desc.index as usize]
                            }
                        })
                        .collect();
                    state.stack[insn.a()] = heap.allocate(LuaClosure { proto, upvalues }).into();
                }
                OpCode::VarArg => {
                    let n = insn.c();
                    let num_wanted = if n > 0 {
                        n as usize - 1
                    } else {
                        frame.num_extra_args
                    };
                    if num_wanted > 0 {
                        self.frames.last_mut().unwrap().pc = state.pc;

                        let a = insn.a();

                        let new_stack_len = frame.base + a + num_wanted;
                        if self.stack.len() < new_stack_len {
                            self.stack.resize(new_stack_len, Value::Nil);
                        }
                        let extra_args_bottom = frame.base - 1 - frame.num_extra_args;
                        let num_copied = num_wanted.min(frame.num_extra_args);
                        self.stack.copy_within(
                            extra_args_bottom..extra_args_bottom + num_copied,
                            frame.base + a,
                        );

                        if num_wanted > frame.num_extra_args {
                            self.stack[frame.base + a + frame.num_extra_args
                                ..frame.base + a + num_wanted]
                                .fill(Value::Nil);
                        }

                        return Ok(());
                    }
                }
                OpCode::VarArgPrep => {
                    let num_fixed_args = insn.a();
                    let num_extra_args = saved_stack_top - frame.bottom - 1 - num_fixed_args;
                    if num_extra_args > 0 {
                        let new_base = saved_stack_top + 1;
                        let frame = self.frames.last_mut().unwrap();
                        frame.pc = state.pc;
                        frame.base = new_base;
                        frame.num_extra_args = num_extra_args;

                        let new_stack_len = new_base + closure.proto.max_stack_size as usize;
                        if self.stack.len() < new_stack_len {
                            self.stack.resize(new_stack_len, Value::Nil);
                        }

                        self.stack.copy_within(
                            frame.bottom..frame.bottom + num_fixed_args + 1,
                            saved_stack_top,
                        );
                        self.stack[frame.bottom + 1..frame.bottom + num_fixed_args + 1]
                            .fill(Value::Nil);

                        return Ok(());
                    }
                }
                OpCode::ExtraArg => unreachable!(),
            }

            let root = Root {
                state: &state,
                global_table: self.global_table,
                open_upvalues: &self.open_upvalues,
                tag_method_names: &self.tag_method_names,
            };
            unsafe { heap.step(&root) };
        }
    }
}
