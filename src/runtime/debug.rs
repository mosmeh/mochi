use super::{opcode::OpCode, Instruction, LuaFrame, Metamethod, Vm};
use crate::types::{AbsLineInfo, LuaClosureProto, LuaThread, Value};

pub(crate) struct Name<'a> {
    pub kind: &'static str,
    pub name: &'a str,
}

impl<'a> From<(&'static str, &'a str)> for Name<'a> {
    fn from((kind, name): (&'static str, &'a str)) -> Self {
        Self { kind, name }
    }
}

impl<'gc> Vm<'gc> {
    pub(crate) fn func_name_from_call<'a>(
        &self,
        thread: &'a LuaThread<'gc>,
        bottom: usize,
    ) -> Option<Name<'a>> {
        let frame = thread
            .frames
            .iter()
            .rev()
            .find_map(|f| f.as_lua().filter(|l| l.bottom <= bottom))?;
        let closure = thread
            .stack
            .get(frame.bottom)
            .and_then(|v| v.as_lua_closure())?;
        closure.proto.func_name_from_pc(frame.last_pc())
    }
}

impl<'gc> LuaClosureProto<'gc> {
    pub(crate) fn func_name_from_pc(&self, pc: usize) -> Option<Name<'_>> {
        let insn = self.code.get(pc)?;
        let metamethod = match insn.opcode() {
            OpCode::Call | OpCode::TailCall => {
                return self.obj_name(pc, insn.a()); // Get function name
            }
            OpCode::TForCall => {
                // For iterator
                return Some(("for iterator", "for iterator").into());
            }
            // Other instructions can do calls through metamethods
            OpCode::Self_
            | OpCode::GetTabUp
            | OpCode::GetTable
            | OpCode::GetI
            | OpCode::GetField => Metamethod::Index,
            OpCode::SetTabUp | OpCode::SetTable | OpCode::SetI | OpCode::SetField => {
                Metamethod::NewIndex
            }
            OpCode::MmBin | OpCode::MmBinI | OpCode::MmBinK => Metamethod::from(insn.c()),
            OpCode::Unm => Metamethod::Unm,
            OpCode::BNot => Metamethod::BNot,
            OpCode::Len => Metamethod::Len,
            OpCode::Concat => Metamethod::Concat,
            OpCode::Eq => Metamethod::Eq,
            // No cases for OP_EQI and OP_EQK, as they don't call metamethods
            OpCode::Lt | OpCode::LtI | OpCode::GtI => Metamethod::Lt,
            OpCode::Le | OpCode::LeI | OpCode::GeI => Metamethod::Le,
            OpCode::Close | OpCode::Return => Metamethod::Close,
            _ => return None,
        };
        Some(("metamethod", metamethod.name()).into())
    }

    // refer to "getobjname" in ldebug.c
    fn obj_name(&self, last_pc: usize, register: usize) -> Option<Name<'_>> {
        if let Some(name) = self.local_name(register as u32 + 1, last_pc as u32) {
            return Some(("local", name).into());
        }

        let pc = self.find_register_setter(last_pc, register)?;
        let insn = *self.code.get(pc)?;
        let opcode = insn.opcode();
        match opcode {
            OpCode::Move => {
                let b = insn.b();
                if b < insn.a() {
                    return self.obj_name(pc, b);
                }
            }
            OpCode::GetTabUp => {
                let k = insn.c();
                return Some((self.kind_from_insn(pc, insn, true), self.constant_name(k)).into());
            }
            OpCode::GetTable => {
                let k = insn.c();
                return Some(
                    (
                        self.kind_from_insn(pc, insn, false),
                        self.register_name(pc, k as usize),
                    )
                        .into(),
                );
            }
            OpCode::GetI => {
                return Some(("field", "integer index").into());
            }
            OpCode::GetField => {
                let k = insn.c();
                return Some((self.kind_from_insn(pc, insn, false), self.constant_name(k)).into());
            }
            OpCode::GetUpval => {
                return Some(("upvalue", self.upvalue_name(insn.b())?).into());
            }
            OpCode::LoadK | OpCode::LoadKX => {
                let b = if opcode == OpCode::LoadK {
                    insn.bx()
                } else {
                    insn.ax()
                };
                if let Some(s) = self.constants[b].as_lua_string() {
                    return Some(("constant", s.as_str().ok()?).into());
                }
            }
            OpCode::Self_ => {
                return Some(("method", self.rk_name(pc, insn)).into());
            }
            _ => {}
        }
        None
    }

    // refer to "findsetreg" in ldebug.c
    // Try to find last instruction before `last_pc` that modified `register`.
    fn find_register_setter(&self, mut last_pc: usize, register: usize) -> Option<usize> {
        if self
            .code
            .get(last_pc)?
            .opcode()
            .properties()
            .calls_metamethod
        {
            // Previous instruction was not actually executed.
            last_pc = last_pc.checked_sub(1)?;
        }

        let mut register_setter = None; // Keep last instruction that changed `register`.
        let mut jump_target = 0; // Any code before this address is conditional.
        let mut pc = 0;
        while pc < last_pc {
            let insn = self.code.get(pc)?;
            let op: OpCode = insn.opcode();
            let a = insn.a();

            // true if current instruction changed `register`.
            let is_changed = match op {
                OpCode::LoadNil => {
                    // Set registers from 'a' to 'a+b'.
                    let b = insn.b();
                    a <= register && register <= a + b
                }
                OpCode::TForCall => {
                    // Affect all regs above its base.
                    register >= a + 2
                }
                OpCode::Call | OpCode::TailCall => {
                    // Affect all registers above base.
                    register >= a
                }
                OpCode::Jmp => {
                    // Doesn't change registers, but changes `jump_target`.
                    let b = insn.sj();
                    let dest = (pc as i32 + 1 + b) as usize;

                    // Jump does not skip 'lastpc' and is larger than the current one?
                    if dest <= last_pc && dest > jump_target {
                        jump_target = dest;
                    }
                    false
                }
                _ => {
                    // Any instruction that sets A.
                    op.properties().sets_a && register == a
                }
            };

            if is_changed {
                // is code conditional (inside a jump)?
                register_setter = if pc < jump_target {
                    None // cannot know who sets that register
                } else {
                    // current position sets that register
                    Some(pc)
                };
            }

            pc += 1;
        }

        register_setter
    }

    fn rk_name(&self, pc: usize, insn: Instruction) -> &'_ str {
        let c = insn.c();
        if insn.k() {
            self.constant_name(c)
        } else {
            self.register_name(pc, c as usize)
        }
    }

    fn constant_name(&self, k: u8) -> &'_ str {
        self.constants
            .get(k as usize)
            .and_then(Value::as_lua_string)
            .and_then(|s| s.as_str().ok())
            .unwrap_or("?")
    }

    fn register_name(&self, pc: usize, register: usize) -> &'_ str {
        self.obj_name(pc, register)
            .map(|x| if x.kind == "c" { x.name } else { "?" })
            .unwrap_or("??")
    }

    fn upvalue_name(&self, _upvalue: usize) -> Option<&str> {
        // TODO: fetch name from upvalue
        None
    }

    fn kind_from_insn(&self, pc: usize, insn: Instruction, is_upvalue: bool) -> &'static str {
        // Check whether table being indexed by instruction `insn` is the
        // environment '_ENV'
        let table_register = insn.b(); // table index
        let name = if is_upvalue {
            self.upvalue_name(table_register)
        } else {
            self.obj_name(pc, table_register).map(|x| x.name)
        };
        match name {
            Some("_ENV") => "global",
            _ => "field",
        }
    }

    pub(crate) fn current_line(&self, frame: &LuaFrame) -> Option<u32> {
        self.func_line(frame.last_pc() as u32)
    }

    /// Get the line corresponding to instruction `pc` in the function.
    pub(crate) fn func_line(&self, pc: u32) -> Option<u32> {
        // first gets a base line and from there does the increments until
        // the desired instruction.
        let mut abs = self.base_line(pc);
        let line_info = self.line_info.as_ref()?;
        let mut base_line = abs.line;
        while abs.pc < pc {
            base_line += line_info[abs.pc as usize] as u32;
            abs.pc += 1;
        }
        Some(base_line)
    }

    fn base_line(&self, pc: u32) -> AbsLineInfo {
        self.abs_line_info
            .as_ref()
            .and_then(|abs| {
                let i = match abs.binary_search_by_key(&pc, |i| i.pc) {
                    Ok(i) => i,
                    Err(i) => i.saturating_sub(1),
                };
                abs.get(i).filter(|abs| abs.pc <= pc).cloned()
            })
            .unwrap_or_else(|| AbsLineInfo {
                pc: 0,
                line: self.lines_defined.base_line(),
            })
    }

    fn local_name(&self, register: u32, pc: u32) -> Option<&'_ str> {
        if register == 0 {
            return None;
        }
        self.local_vars
            .as_ref()?
            .iter()
            .take_while(|l| l.pc.start <= pc)
            .filter(|l| pc < l.pc.end)
            .nth(register as usize - 1)
            .and_then(|var| var.name.as_str().ok())
    }
}

impl LuaFrame {
    pub fn last_pc(&self) -> usize {
        self.pc.saturating_sub(1)
    }
}
