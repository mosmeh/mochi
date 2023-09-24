use crate::types::{AbsLineInfo, LuaClosureProto, LuaThread, Value};

use super::{
    opcode::{self, OpCode},
    Instruction, LuaFrame, Metamethod, Vm,
};

pub(crate) struct DebugNameInfo<'a> {
    pub kind: &'static str,
    pub name: &'a str,
}

impl<'a> From<(&'static str, &'a str)> for DebugNameInfo<'a> {
    fn from((kind, name): (&'static str, &'a str)) -> Self {
        Self { kind, name }
    }
}

impl<'gc> Vm<'gc> {
    pub(crate) fn funcname_from_call<'a>(
        &self,
        thread: &'a mut LuaThread<'gc>,
        bottom: usize,
    ) -> Option<DebugNameInfo<'a>> {
        let frame = thread.lua_frame_before(bottom)?;
        let closure = thread.stack_closure(frame.bottom)?;
        closure.proto.funcname_from_code(frame.last_pc())
    }
}

impl<'gc> LuaClosureProto<'gc> {
    pub(crate) fn funcname_from_code(&self, pc: usize) -> Option<DebugNameInfo<'_>> {
        let insn = self.code.get(pc)?;
        let mut tm = Metamethod::Index;
        match insn.raw_opcode() {
            opcode::CALL | opcode::TAILCALL => {
                return self.get_objname(pc, insn.a()); // Get function name
            }
            opcode::TFORCALL => {
                // For iterator
                return Some(("for iterator", "for iterator").into());
            }
            // Other instructions can do calls through metamethods
            opcode::SELF
            | opcode::GETTABUP
            | opcode::GETTABLE
            | opcode::GETI
            | opcode::GETFIELD => {
                tm = Metamethod::Index;
            }
            opcode::SETTABUP | opcode::SETTABLE | opcode::SETI | opcode::SETFIELD => {
                tm = Metamethod::NewIndex;
            }
            opcode::MMBIN | opcode::MMBINI | opcode::MMBINK => {
                tm = Metamethod::from(insn.c());
            }
            opcode::UNM => tm = Metamethod::Unm,
            opcode::BNOT => tm = Metamethod::BNot,
            opcode::LEN => tm = Metamethod::Len,
            opcode::CONCAT => tm = Metamethod::Concat,
            opcode::EQ => tm = Metamethod::Eq,
            // No cases for OP_EQI and OP_EQK, as they don't call metamethods
            opcode::LT | opcode::LTI | opcode::GTI => tm = Metamethod::Lt,
            opcode::LE | opcode::LEI | opcode::GEI => tm = Metamethod::Le,
            opcode::CLOSE | opcode::RETURN => tm = Metamethod::Close,
            _ => return None,
        }
        Some(("metamethod", tm.static_name()).into())
    }

    // refer to "getobjname" in ldebug.c
    pub(crate) fn get_objname(&self, lastpc: usize, reg: usize) -> Option<DebugNameInfo<'_>> {
        if let Some(name) = self.get_localname(reg as u32 + 1, lastpc as _) {
            return Some(("local", name).into());
        }

        let pc = self.find_setreg(lastpc, reg)?;
        let insn = *self.code.get(pc)?;
        let opcode = insn.opcode();
        match opcode {
            OpCode::Move => {
                let b = insn.b();
                if b < insn.a() {
                    return self.get_objname(pc, b);
                }
            }
            OpCode::GetTabUp => {
                let k = insn.c();
                return Some((self.gxf(pc, insn, true), self.kname(k)).into());
            }
            OpCode::GetTable => {
                let k = insn.c();
                return Some((self.gxf(pc, insn, false), self.rname(pc, k as _)).into());
            }
            OpCode::GetI => {
                return Some(("field", "integer index").into());
            }
            OpCode::GetField => {
                let k = insn.c();
                return Some((self.gxf(pc, insn, false), self.kname(k)).into());
            }
            OpCode::GetUpval => {
                return Some(("upvalue", self.upvalname(insn.b())?).into());
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
                return Some(("method", self.rkname(pc, &insn)).into());
            }
            _ => {}
        }
        None
    }

    // refer to "findsetreg" in ldebug.c
    /*
     ** Try to find last instruction before 'lastpc' that modified register 'reg'.
     */
    pub(crate) fn find_setreg(&self, mut lastpc: usize, reg: usize) -> Option<usize> {
        if self.code.get(lastpc)?.opcode().modes().mm {
            // Previous instruction was not actually executed.
            lastpc = lastpc.checked_sub(1)?;
        }

        let mut setreg = None; // Keep last instruction that changed 'reg'.
        let mut jmptarget = 0; // Any code before this address is conditional.
        let mut pc = 0;
        while pc < lastpc {
            let i = self.code.get(pc)?;
            let op: OpCode = i.opcode();
            let a = i.a();

            let mut change: bool = false; // True if current instruction changed 'reg'.
            match op {
                OpCode::LoadNil => {
                    // Set registers from 'a' to 'a+b'.
                    let b = i.b();
                    change = a <= reg && reg <= a + b;
                }
                OpCode::TForCall => {
                    // Affect all regs above its base.
                    change = reg >= a + 2;
                }
                OpCode::Call | OpCode::TailCall => {
                    // Affect all registers above base.
                    change = reg >= a;
                }
                OpCode::Jmp => {
                    // Doesn't change registers, but changes 'jmptarget'.
                    let b = i.sj();
                    let dest = (pc as i32 + 1 + b) as usize;

                    // Jump does not skip 'lastpc' and is larger than the current one?
                    if dest <= lastpc && dest > jmptarget {
                        jmptarget = dest; // Update 'jmptarget'.
                    }
                    change = false;
                }
                _ => {
                    // Any instruction that sets A.
                    change = op.modes().set_a && reg == a;
                }
            }

            if change {
                setreg = filterpc(pc, jmptarget);
            }

            pc += 1;
        }

        fn filterpc(pc: usize, jmptarget: usize) -> Option<usize> {
            /* is code conditional (inside a jump)? */
            if pc < jmptarget {
                None /* cannot know who sets that register */
            } else {
                /* current position sets that register */
                Some(pc)
            }
        }

        setreg
    }

    fn rkname(&self, pc: usize, i: &Instruction) -> &'_ str {
        let c = i.c();
        if i.k() {
            self.kname(c)
        } else {
            self.rname(pc, c as _)
        }
    }

    fn kname(&self, k: u8) -> &'_ str {
        self.constants
            .get(k as usize)
            .and_then(Value::as_lua_string)
            .and_then(|s| s.as_str().ok())
            .unwrap_or("?")
    }

    fn rname(&self, pc: usize, c: usize) -> &'_ str {
        self.get_objname(pc, c)
            .map(|x| if x.kind == "c" { x.name } else { "?" })
            .unwrap_or("??")
    }

    fn upvalname(&self, _uv: usize) -> Option<&str> {
        // TODO: fetch name from upvalue
        None
    }

    /*
     ** Check whether table being indexed by instruction 'i' is the
     ** environment '_ENV'
     */
    fn gxf(&self, pc: usize, insn: Instruction, isup: bool) -> &'static str {
        let t = insn.b(); /* table index */
        // name of indexed variable
        let name = if isup {
            /* is an upvalue? */
            self.upvalname(t)
        } else {
            self.get_objname(pc, t).map(|x| x.name)
        };
        match name {
            Some("_ENV") => "global",
            _ => "field",
        }
    }

    pub(crate) fn get_currentline(&self, frame: &LuaFrame) -> Option<u32> {
        self.get_funcline(frame.last_pc() as _)
    }

    /*
     ** Get the line corresponding to instruction 'pc' in function 'f';
     ** first gets a base line and from there does the increments until
     ** the desired instruction.
     */
    pub(crate) fn get_funcline(&self, pc: u32) -> Option<u32> {
        let mut abs = self.get_baseline(pc);
        let lineinfo = self.line_info.as_ref()?;
        let mut baseline = abs.line;
        while abs.pc < pc {
            baseline += lineinfo[abs.pc as usize] as u32;
            abs.pc += 1;
        }
        Some(baseline)
    }

    pub(crate) fn get_baseline(&self, pc: u32) -> AbsLineInfo {
        self.abs_line_info
            .as_ref()
            .and_then(|abs| {
                let i = match abs.binary_search_by_key(&pc, |i| i.pc) {
                    Ok(i) => i,
                    Err(i) => i.checked_sub(1).unwrap_or(0),
                };
                abs.get(i).filter(|abs| abs.pc <= pc).copied()
            })
            .unwrap_or_else(|| AbsLineInfo {
                pc: 0,
                line: self.lines_defined.baseline(),
            })
    }

    pub(crate) fn get_localname(&self, mut ln: u32, pc: u32) -> Option<&'_ str> {
        let item = self
            .local_vars
            .as_ref()?
            .iter()
            .take_while(|l| l.pc.start <= pc)
            .filter(|l| pc < l.pc.end)
            .find(|_| {
                ln = ln.checked_sub(1).unwrap_or(0);
                ln == 0
            })?;
        item.name.as_str().ok()
    }
}

impl LuaFrame {
    pub(crate) fn last_pc(&self) -> usize {
        self.pc.checked_sub(1).unwrap_or(0)
    }
}
