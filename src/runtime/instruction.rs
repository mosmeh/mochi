use super::opcode::OpCode;

pub const UINT17_MAX: u32 = (1 << 17) - 1;
pub const UINT25_MAX: u32 = (1 << 25) - 1;

pub const OFFSET_SB: i16 = u8::MAX as i16 >> 1;
pub const OFFSET_SC: i16 = u8::MAX as i16 >> 1;
pub const OFFSET_SBX: i32 = UINT17_MAX as i32 >> 1;
pub const OFFSET_SJ: i32 = UINT25_MAX as i32 >> 1;

#[derive(Clone, Copy)]
pub struct Instruction(pub u32);

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("Instruction").field(&self.opcode()).finish()
    }
}

impl Instruction {
    pub fn opcode(&self) -> OpCode {
        OpCode::from(self.raw_opcode())
    }

    pub fn raw_opcode(&self) -> u8 {
        (self.0 as u8) & 0x7f
    }

    pub fn a(&self) -> usize {
        ((self.0 >> 7) & 0xff) as usize
    }

    pub fn b(&self) -> usize {
        (self.0 >> 16 & 0xff) as usize
    }

    pub fn sb(&self) -> i16 {
        self.b() as i16 - OFFSET_SB
    }

    pub fn c(&self) -> u8 {
        (self.0 >> 24) as u8
    }

    pub fn sc(&self) -> i16 {
        self.c() as i16 - OFFSET_SC
    }

    pub fn k(&self) -> bool {
        ((self.0 >> 15) & 1) != 0
    }

    pub fn bx(&self) -> usize {
        (self.0 >> 15) as usize
    }

    pub fn sbx(&self) -> i32 {
        (self.0 >> 15) as i32 - OFFSET_SBX
    }

    pub fn ax(&self) -> usize {
        (self.0 >> 7) as usize
    }

    pub fn sj(&self) -> i32 {
        (self.0 >> 7) as i32 - OFFSET_SJ
    }
}
