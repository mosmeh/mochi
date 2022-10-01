use super::{instruction::Address, CodegenError, Frame};
use crate::{
    gc::GcContext,
    parser::ast::{BinaryOp, UnaryOp},
    runtime::{
        instruction::{OFFSET_SBX, OFFSET_SC, OFFSET_SJ, UINT17_MAX, UINT25_MAX},
        Instruction, Metamethod, OpCode,
    },
    types::{Integer, LuaClosureProto, LuaString, RegisterIndex, UpvalueIndex},
};

#[derive(Debug, Clone, Copy)]
pub struct ConstantIndex25(u32);

impl TryFrom<usize> for ConstantIndex25 {
    type Error = ();

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        if i <= UINT25_MAX as usize {
            Ok(Self(i as u32))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstantIndex8(u8);

impl TryFrom<usize> for ConstantIndex8 {
    type Error = ();

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        if let Ok(i) = i.try_into() {
            Ok(Self(i))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ProtoIndex(u32);

impl TryFrom<usize> for ProtoIndex {
    type Error = ();

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        if i <= UINT17_MAX as usize {
            Ok(Self(i as u32))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ImmediateI8(i16);

impl TryFrom<Integer> for ImmediateI8 {
    type Error = ();

    fn try_from(i: Integer) -> Result<Self, Self::Error> {
        if (i as u64).wrapping_add(OFFSET_SC as u64) <= u8::MAX as u64 {
            Ok(Self(i as i16))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ImmediateI17(i32);

impl TryFrom<Integer> for ImmediateI17 {
    type Error = ();

    fn try_from(i: Integer) -> Result<Self, Self::Error> {
        if -OFFSET_SBX as Integer <= i && i <= UINT17_MAX as Integer - OFFSET_SBX as Integer {
            Ok(Self(i as i32))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum RkIndex {
    Register(RegisterIndex),
    Constant(ConstantIndex8),
}

impl From<RegisterIndex> for RkIndex {
    fn from(r: RegisterIndex) -> Self {
        Self::Register(r)
    }
}

impl From<ConstantIndex8> for RkIndex {
    fn from(c: ConstantIndex8) -> Self {
        Self::Constant(c)
    }
}

impl RkIndex {
    fn to_c_and_k(&self) -> (u8, bool) {
        match self {
            Self::Register(r) => (r.0, false),
            Self::Constant(c) => (c.0, true),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrAddress(pub usize);

#[derive(Debug, Clone)]
pub enum IrInstruction {
    Move {
        dest: RegisterIndex,
        source: RegisterIndex,
    },
    LoadConstant {
        dest: RegisterIndex,
        constant: ConstantIndex25,
    },
    LoadInteger {
        dest: RegisterIndex,
        immediate: ImmediateI17,
    },
    LoadFloat {
        dest: RegisterIndex,
        immediate: ImmediateI17,
    },
    LoadBoolean {
        dest: RegisterIndex,
        immediate: bool,
    },
    LoadFalseAndSkip {
        dest: RegisterIndex,
    },
    LoadNil {
        dest: RegisterIndex,
    },
    GetUpvalue {
        dest: RegisterIndex,
        upvalue: UpvalueIndex,
    },
    SetUpvalue {
        upvalue: UpvalueIndex,
        value: RegisterIndex,
    },
    GetUpvalueField {
        dest: RegisterIndex,
        table: UpvalueIndex,
        field: ConstantIndex8,
    },
    GetTableGenericKey {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: RegisterIndex,
    },
    GetTableIntegerKey {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: u8,
    },
    GetTableField {
        dest: RegisterIndex,
        table: RegisterIndex,
        field: ConstantIndex8,
    },
    SetUpvalueField {
        table: UpvalueIndex,
        field: ConstantIndex8,
        value: RkIndex,
    },
    SetTableGenericKey {
        table: RegisterIndex,
        key: RegisterIndex,
        value: RkIndex,
    },
    SetTableIntegerKey {
        table: RegisterIndex,
        key: u8,
        value: RkIndex,
    },
    SetTableField {
        table: RegisterIndex,
        field: ConstantIndex8,
        value: RkIndex,
    },
    CreateTable {
        dest: RegisterIndex,
    },
    GetSelf {
        dest: RegisterIndex,
        table: RegisterIndex,
        key: RkIndex,
    },
    BinaryOpImmediate {
        op: BinaryOp,
        dest: RegisterIndex,
        lhs: RegisterIndex,
        rhs: ImmediateI8,
        flipped: bool,
    },
    BinaryOpConstant {
        op: BinaryOp,
        dest: RegisterIndex,
        lhs: RegisterIndex,
        rhs: ConstantIndex8,
        flipped: bool,
    },
    BinaryOp {
        op: BinaryOp,
        dest: RegisterIndex,
        lhs: RegisterIndex,
        rhs: RegisterIndex,
    },
    UnaryOp {
        op: UnaryOp,
        dest: RegisterIndex,
        operand: RegisterIndex,
    },
    Compare {
        op: BinaryOp,
        lhs: RegisterIndex,
        rhs: RegisterIndex,
        jump_on: bool,
    },
    CompareConstant {
        op: BinaryOp,
        lhs: RegisterIndex,
        rhs: ConstantIndex8,
        jump_on: bool,
    },
    CompareImmediate {
        op: BinaryOp,
        lhs: RegisterIndex,
        rhs: ImmediateI8,
        jump_on: bool,
    },
    Test {
        condition: RegisterIndex,
        jump_on: bool,
    },
    ConditionalMove {
        dest: RegisterIndex,
        source: RegisterIndex,
        move_and_jump_on: bool,
    },
    Jump {
        target: Label,
    },
    Call {
        callee: RegisterIndex,
        num_fixed_args: Option<u8>,
    },
    Return {
        base: RegisterIndex,
        count: Option<u8>,
        close_upvalues: bool,
    },
    ForLoop {
        base: RegisterIndex,
        next_target: Label,
        is_generic: bool,
    },
    PrepareForLoop {
        base: RegisterIndex,
        skip_target: Label,
        is_generic: bool,
    },
    GenericForCall {
        base: RegisterIndex,
    },
    SetList {
        table: RegisterIndex,
        count: u8,
        index_offset: usize,
    },
    GetClosure {
        dest: RegisterIndex,
        proto: ProtoIndex,
    },
    PrepareVarArg {
        num_fixed_args: u8,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct Label(pub usize);

pub(super) fn lower_ir<'gc>(
    gc: &'gc GcContext,
    source: LuaString<'gc>,
    frame: Frame<'gc>,
) -> Result<LuaClosureProto<'gc>, CodegenError> {
    let mut label_addresses = vec![None; frame.label_ir_addresses.len()];
    let mut pending_instructions = Vec::new();

    let mut code = Vec::with_capacity(frame.ir_code.len());
    for (ir_addr, insn) in frame.ir_code.into_iter().enumerate() {
        for (label, _) in frame
            .label_ir_addresses
            .iter()
            .enumerate()
            .filter(|(_, a)| **a == Some(IrAddress(ir_addr)))
        {
            label_addresses[label] = Some(Address(code.len()));
        }

        match insn {
            IrInstruction::Move { dest, source } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::Move,
                    dest.0,
                    source.0,
                    0,
                    false,
                ));
            }
            IrInstruction::LoadInteger { dest, immediate } => {
                code.push(Instruction::from_a_sbx(OpCode::LoadI, dest.0, immediate.0));
            }
            IrInstruction::LoadFloat { dest, immediate } => {
                code.push(Instruction::from_a_sbx(OpCode::LoadF, dest.0, immediate.0));
            }
            IrInstruction::LoadBoolean { dest, immediate } => {
                let opcode = if immediate {
                    OpCode::LoadTrue
                } else {
                    OpCode::LoadFalse
                };
                code.push(Instruction::from_a_b_c_k(opcode, dest.0, 0, 0, false));
            }
            IrInstruction::LoadFalseAndSkip { dest } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::LFalseSkip,
                    dest.0,
                    0,
                    0,
                    false,
                ));
            }
            IrInstruction::LoadNil { dest } => match code.last_mut() {
                Some(prev_insn)
                    if prev_insn.opcode() == OpCode::LoadNil
                        && prev_insn.a() + prev_insn.b() + 1 == dest.0 as usize
                        && prev_insn.b() < u8::MAX as usize =>
                {
                    *prev_insn = Instruction::from_a_b_c_k(
                        OpCode::LoadNil,
                        prev_insn.a() as u8,
                        prev_insn.b() as u8 + 1,
                        0,
                        false,
                    )
                }
                _ => code.push(Instruction::from_a_b_c_k(
                    OpCode::LoadNil,
                    dest.0,
                    0,
                    0,
                    false,
                )),
            },
            IrInstruction::LoadConstant { dest, constant } => {
                if constant.0 <= UINT17_MAX {
                    code.push(Instruction::from_a_bx(OpCode::LoadK, dest.0, constant.0));
                } else {
                    code.push(Instruction::from_a_bx(OpCode::LoadKX, dest.0, 0));
                    code.push(Instruction::from_ax(OpCode::ExtraArg, constant.0));
                }
            }
            IrInstruction::GetUpvalue { dest, upvalue } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::GetUpval,
                    dest.0,
                    upvalue.0,
                    0,
                    false,
                ));
            }
            IrInstruction::SetUpvalue { upvalue, value } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::SetUpval,
                    value.0,
                    upvalue.0,
                    0,
                    false,
                ));
            }
            IrInstruction::GetUpvalueField { dest, table, field } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::GetTabUp,
                    dest.0,
                    table.0,
                    field.0,
                    false,
                ));
            }
            IrInstruction::GetTableGenericKey { dest, table, key } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::GetTable,
                    dest.0,
                    table.0,
                    key.0,
                    false,
                ));
            }
            IrInstruction::GetTableIntegerKey { dest, table, key } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::GetI,
                    dest.0,
                    table.0,
                    key,
                    false,
                ));
            }
            IrInstruction::GetTableField { dest, table, field } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::GetField,
                    dest.0,
                    table.0,
                    field.0,
                    false,
                ));
            }
            IrInstruction::SetUpvalueField {
                table,
                field,
                value,
            } => {
                let (c, k) = value.to_c_and_k();
                code.push(Instruction::from_a_b_c_k(
                    OpCode::SetTabUp,
                    table.0,
                    field.0,
                    c,
                    k,
                ));
            }
            IrInstruction::SetTableGenericKey { table, key, value } => {
                let (c, k) = value.to_c_and_k();
                code.push(Instruction::from_a_b_c_k(
                    OpCode::SetTable,
                    table.0,
                    key.0,
                    c,
                    k,
                ));
            }
            IrInstruction::SetTableIntegerKey { table, key, value } => {
                let (c, k) = value.to_c_and_k();
                code.push(Instruction::from_a_b_c_k(OpCode::SetI, table.0, key, c, k));
            }
            IrInstruction::SetTableField {
                table,
                field,
                value,
            } => {
                let (c, k) = value.to_c_and_k();
                code.push(Instruction::from_a_b_c_k(
                    OpCode::SetField,
                    table.0,
                    field.0,
                    c,
                    k,
                ));
            }
            IrInstruction::CreateTable { dest } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::NewTable,
                    dest.0,
                    0,
                    0,
                    false,
                ));
                code.push(Instruction::from_ax(OpCode::ExtraArg, 0));
            }
            IrInstruction::GetSelf { dest, table, key } => {
                let (c, k) = key.to_c_and_k();
                code.push(Instruction::from_a_b_c_k(
                    OpCode::Self_,
                    dest.0,
                    table.0,
                    c,
                    k,
                ));
            }
            IrInstruction::BinaryOpImmediate {
                op,
                dest,
                lhs,
                rhs,
                flipped,
            } => {
                let opcode = op.immdiate_opcode();
                code.push(Instruction::from_a_b_sc_k(
                    opcode, dest.0, lhs.0, rhs.0, false,
                ));

                let c = op.metamethod() as u8;
                code.push(Instruction::from_a_sb_c_k(
                    OpCode::MmBinI,
                    lhs.0,
                    rhs.0,
                    c,
                    flipped,
                ));
            }
            IrInstruction::BinaryOpConstant {
                op,
                dest,
                lhs,
                rhs,
                flipped,
            } => {
                let opcode = op.constant_opcode();
                code.push(Instruction::from_a_b_c_k(
                    opcode, dest.0, lhs.0, rhs.0, false,
                ));

                let c = op.metamethod() as u8;
                code.push(Instruction::from_a_b_c_k(
                    OpCode::MmBinK,
                    lhs.0,
                    rhs.0,
                    c,
                    flipped,
                ));
            }
            IrInstruction::BinaryOp { op, dest, lhs, rhs } => {
                let opcode = op.opcode();
                code.push(Instruction::from_a_b_c_k(
                    opcode, dest.0, lhs.0, rhs.0, false,
                ));

                let c = op.metamethod() as u8;
                code.push(Instruction::from_a_b_c_k(
                    OpCode::MmBin,
                    lhs.0,
                    rhs.0,
                    c,
                    false,
                ));
            }
            IrInstruction::UnaryOp { op, dest, operand } => {
                let opcode = op.opcode();
                code.push(Instruction::from_a_b_c_k(
                    opcode, dest.0, operand.0, 0, false,
                ));
            }
            IrInstruction::Compare {
                op,
                lhs,
                rhs,
                jump_on,
            } => {
                let (opcode, k) = match op {
                    BinaryOp::Lt => (OpCode::Lt, jump_on),
                    BinaryOp::Le => (OpCode::Le, jump_on),
                    BinaryOp::Gt => (OpCode::Le, !jump_on),
                    BinaryOp::Ge => (OpCode::Lt, !jump_on),
                    BinaryOp::Eq => (OpCode::Eq, jump_on),
                    BinaryOp::Ne => (OpCode::Eq, !jump_on),
                    _ => unreachable!(),
                };
                code.push(Instruction::from_a_b_c_k(opcode, lhs.0, rhs.0, 0, k));
            }
            IrInstruction::CompareConstant {
                op,
                lhs,
                rhs,
                jump_on,
            } => {
                let (opcode, k) = match op {
                    BinaryOp::Eq => (OpCode::EqK, jump_on),
                    BinaryOp::Ne => (OpCode::EqK, !jump_on),
                    _ => unreachable!(),
                };
                code.push(Instruction::from_a_b_c_k(opcode, lhs.0, rhs.0, 0, k));
            }
            IrInstruction::CompareImmediate {
                op,
                lhs,
                rhs,
                jump_on,
            } => {
                let (opcode, k) = match op {
                    BinaryOp::Lt => (OpCode::LtI, jump_on),
                    BinaryOp::Le => (OpCode::LeI, jump_on),
                    BinaryOp::Gt => (OpCode::GtI, jump_on),
                    BinaryOp::Ge => (OpCode::GeI, jump_on),
                    BinaryOp::Eq => (OpCode::EqI, jump_on),
                    BinaryOp::Ne => (OpCode::EqI, !jump_on),
                    _ => unreachable!(),
                };
                code.push(Instruction::from_a_sb_c_k(opcode, lhs.0, rhs.0, 0, k));
            }
            IrInstruction::Test { condition, jump_on } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::Test,
                    condition.0,
                    0,
                    0,
                    jump_on,
                ));
            }
            IrInstruction::ConditionalMove {
                dest,
                source,
                move_and_jump_on,
            } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::TestSet,
                    dest.0,
                    source.0,
                    0,
                    move_and_jump_on,
                ));
            }
            IrInstruction::Call {
                callee,
                num_fixed_args,
            } => {
                let b = if let Some(num_fixed_args) = num_fixed_args {
                    num_fixed_args + 1
                } else {
                    0
                };
                code.push(Instruction::from_a_b_c_k(
                    OpCode::Call,
                    callee.0,
                    b,
                    0,
                    false,
                ));
            }
            IrInstruction::Return {
                base,
                count,
                close_upvalues,
            } => {
                let opcode = match count {
                    _ if close_upvalues => OpCode::Return,
                    Some(0) => OpCode::Return0,
                    Some(1) => OpCode::Return1,
                    _ => OpCode::Return,
                };
                code.push(Instruction::from_a_b_c_k(
                    opcode,
                    base.0,
                    count.map(|c| c + 1).unwrap_or_default(),
                    0,
                    close_upvalues,
                ));
            }
            IrInstruction::GenericForCall { base } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::TForCall,
                    base.0,
                    0,
                    0,
                    false,
                ));
            }
            IrInstruction::SetList {
                table,
                count,
                index_offset,
            } => {
                if let Ok(index_offset) = index_offset.try_into() {
                    code.push(Instruction::from_a_b_c_k(
                        OpCode::SetList,
                        table.0,
                        count,
                        index_offset,
                        false,
                    ));
                } else {
                    const FACTOR: usize = u8::MAX as usize + 1;
                    code.push(Instruction::from_a_b_c_k(
                        OpCode::SetList,
                        table.0,
                        count,
                        (index_offset % FACTOR) as u8,
                        true,
                    ));
                    code.push(Instruction::from_ax(
                        OpCode::ExtraArg,
                        (index_offset / FACTOR).try_into().unwrap(),
                    ));
                }
            }
            IrInstruction::GetClosure { dest, proto } => {
                code.push(Instruction::from_a_bx(OpCode::Closure, dest.0, proto.0));
            }
            IrInstruction::PrepareVarArg { num_fixed_args } => {
                code.push(Instruction::from_a_b_c_k(
                    OpCode::VarArgPrep,
                    num_fixed_args,
                    0,
                    0,
                    false,
                ));
            }
            IrInstruction::Jump { .. }
            | IrInstruction::ForLoop { .. }
            | IrInstruction::PrepareForLoop { .. } => {
                let addr = Address(code.len());
                code.push(Instruction(0)); // put dummy
                pending_instructions.push((addr, insn));
            }
        }
    }

    for (addr, insn) in pending_instructions {
        let patched_insn = match insn {
            IrInstruction::Jump { target } => {
                let target_addr = label_addresses[target.0].unwrap();
                let sj = target_addr.0 as i32 - addr.0 as i32 - 1;
                (-OFFSET_SJ <= sj && sj <= UINT25_MAX as i32 - OFFSET_SJ)
                    .then_some(Instruction::from_sj(OpCode::Jmp, sj))
            }
            IrInstruction::ForLoop {
                base,
                next_target,
                is_generic,
            } => {
                let opcode = if is_generic {
                    OpCode::TForLoop
                } else {
                    OpCode::ForLoop
                };
                let target_addr = label_addresses[next_target.0].unwrap();
                let bx = (addr.0 - target_addr.0) as u32 + 1;
                (bx <= UINT17_MAX).then_some(Instruction::from_a_bx(opcode, base.0, bx))
            }
            IrInstruction::PrepareForLoop {
                base,
                skip_target,
                is_generic,
            } => {
                let opcode = if is_generic {
                    OpCode::TForPrep
                } else {
                    OpCode::ForPrep
                };
                let target_addr = label_addresses[skip_target.0].unwrap();
                let bx = (target_addr.0 - addr.0) as u32 - 1;
                (bx <= UINT17_MAX).then_some(Instruction::from_a_bx(opcode, base.0, bx))
            }
            _ => unreachable!(),
        };
        if let Some(patched_insn) = patched_insn {
            code[addr.0] = patched_insn;
        } else {
            return Err(CodegenError::ControlStructureTooLong);
        }
    }

    Ok(LuaClosureProto {
        max_stack_size: frame.max_stack_size,
        code: code.into(),
        constants: frame.constants.into(),
        protos: frame
            .protos
            .into_iter()
            .map(|proto| gc.allocate(proto))
            .collect::<Vec<_>>()
            .into(),
        upvalues: frame.upvalues.into(),
        lines_defined: crate::types::LineRange::File,
        source,
    })
}

impl UnaryOp {
    fn opcode(&self) -> OpCode {
        match self {
            Self::Unm => OpCode::Unm,
            Self::Not => OpCode::Not,
            Self::Len => OpCode::Len,
            Self::BNot => OpCode::BNot,
        }
    }
}

impl BinaryOp {
    fn opcode(&self) -> OpCode {
        match self {
            Self::Add => OpCode::Add,
            Self::Sub => OpCode::Sub,
            Self::Mul => OpCode::Mul,
            Self::Div => OpCode::Div,
            Self::IDiv => OpCode::IDiv,
            Self::Pow => OpCode::Pow,
            Self::Mod => OpCode::Mod,
            Self::BAnd => OpCode::BAnd,
            Self::BXor => OpCode::BXor,
            Self::BOr => OpCode::BOr,
            Self::Shr => OpCode::Shr,
            Self::Shl => OpCode::Shl,
            _ => unreachable!(),
        }
    }

    fn immdiate_opcode(&self) -> OpCode {
        match self {
            Self::Add => OpCode::AddI,
            Self::Shr => OpCode::ShrI,
            Self::Shl => OpCode::ShlI,
            Self::Lt => OpCode::LtI,
            Self::Le => OpCode::LeI,
            Self::Gt => OpCode::GtI,
            Self::Ge => OpCode::GeI,
            Self::Eq => OpCode::EqI,
            _ => unreachable!(),
        }
    }

    fn constant_opcode(&self) -> OpCode {
        match self {
            Self::Add => OpCode::AddK,
            Self::Sub => OpCode::SubK,
            Self::Mul => OpCode::MulK,
            Self::Div => OpCode::DivK,
            Self::IDiv => OpCode::IDivK,
            Self::Pow => OpCode::PowK,
            Self::Mod => OpCode::ModK,
            Self::BAnd => OpCode::BAndK,
            Self::BXor => OpCode::BXorK,
            Self::BOr => OpCode::BOrK,
            _ => unreachable!(),
        }
    }

    fn metamethod(&self) -> Metamethod {
        match self {
            Self::Add => Metamethod::Add,
            Self::Sub => Metamethod::Sub,
            Self::Mul => Metamethod::Mul,
            Self::Div => Metamethod::Div,
            Self::IDiv => Metamethod::IDiv,
            Self::Pow => Metamethod::Pow,
            Self::Mod => Metamethod::Mod,
            Self::BAnd => Metamethod::BAnd,
            Self::BXor => Metamethod::BXor,
            Self::BOr => Metamethod::BOr,
            Self::Shr => Metamethod::Shr,
            Self::Shl => Metamethod::Shl,
            _ => unreachable!(),
        }
    }
}
