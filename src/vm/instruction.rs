use super::opcode::OpCode;

#[derive(Clone, Copy)]
pub struct Instruction(pub u32);

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("Instruction").field(&self.opcode()).finish()
    }
}

impl Instruction {
    pub fn opcode(&self) -> OpCode {
        let value = (self.0 & 0x7f) as u8;
        OpCode::new(value)
    }

    pub fn a(&self) -> usize {
        ((self.0 >> 7) & 0xff) as usize
    }

    pub fn b(&self) -> usize {
        (self.0 >> 16 & 0xff) as usize
    }

    pub fn sb(&self) -> i16 {
        const OFFSET: i16 = ((1 << 8) - 1) >> 1;
        self.b() as i16 - OFFSET
    }

    pub fn c(&self) -> u8 {
        (self.0 >> 24) as u8
    }

    pub fn sc(&self) -> i16 {
        const OFFSET: i16 = ((1 << 8) - 1) >> 1;
        self.c() as i16 - OFFSET
    }

    pub fn k(&self) -> bool {
        ((self.0 >> 15) & 1) != 0
    }

    pub fn bx(&self) -> usize {
        (self.0 >> 15) as usize
    }

    pub fn sbx(&self) -> i32 {
        const OFFSET: i32 = ((1 << 17) - 1) >> 1;
        (self.0 >> 15) as i32 - OFFSET
    }

    pub fn ax(&self) -> usize {
        (self.0 >> 7) as usize
    }

    pub fn sj(&self) -> i32 {
        const OFFSET: i32 = ((1 << 25) - 1) >> 1;
        (self.0 >> 7) as i32 - OFFSET
    }
}
