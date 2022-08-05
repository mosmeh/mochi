use crate::vm::{
    instruction::{OFFSET_SB, OFFSET_SBX, OFFSET_SC, OFFSET_SJ, UINT17_MAX, UINT25_MAX},
    Instruction, OpCode,
};

#[derive(Debug, Clone, Copy)]
pub struct Address(pub usize);

impl Instruction {
    /// iABC
    pub fn from_a_b_c_k(opcode: OpCode, a: u8, b: u8, c: u8, k: bool) -> Self {
        Self(
            opcode as u32
                | (a as u32) << 7
                | (k as u32) << 15
                | (b as u32) << 16
                | (c as u32) << 24,
        )
    }

    /// iABC (B is signed)
    pub fn from_a_sb_c_k(opcode: OpCode, a: u8, sb: i16, c: u8, k: bool) -> Self {
        let sb = (sb + OFFSET_SB) as u32;
        Self::from_a_b_c_k(opcode, a, sb.try_into().unwrap(), c, k)
    }

    /// iABC (C is signed)
    pub fn from_a_b_sc_k(opcode: OpCode, a: u8, b: u8, sc: i16, k: bool) -> Self {
        let sc = (sc + OFFSET_SC) as u32;
        Self::from_a_b_c_k(opcode, a, b, sc.try_into().unwrap(), k)
    }

    /// iABx
    pub fn from_a_bx(opcode: OpCode, a: u8, bx: u32) -> Self {
        assert!(bx <= UINT17_MAX);
        Self(opcode as u32 | (a as u32) << 7 | bx << 15)
    }

    /// iAsBx
    pub fn from_a_sbx(opcode: OpCode, a: u8, sbx: i32) -> Self {
        Self::from_a_bx(opcode, a, (sbx as i32 + OFFSET_SBX) as u32)
    }

    /// iAx
    pub fn from_ax(opcode: OpCode, ax: u32) -> Self {
        assert!(ax <= UINT25_MAX);
        Self(opcode as u32 | ax << 7)
    }

    /// isJ
    pub fn from_sj(opcode: OpCode, sj: i32) -> Self {
        let sj = (sj + OFFSET_SJ) as u32;
        assert!(sj <= UINT25_MAX);
        Self(opcode as u32 | sj << 7)
    }
}
