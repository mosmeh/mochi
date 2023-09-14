macro_rules! opcodes {
    ($($name:ident => $variant:ident,)*) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum OpCode {
            $($variant,)*
        }

        impl From<u8> for OpCode {
            fn from(i: u8) -> Self {
                const OPCODES: [OpCode; crate::count!($($variant)*)] = [$(OpCode::$variant,)*];
                OPCODES[i as usize]
            }
        }

        impl std::fmt::Display for OpCode {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let s = match self {
                    $(Self::$variant => stringify!($name),)*
                };
                write!(f, "{:width$}", s, width = f.width().unwrap_or(0))
            }
        }

        $(
            #[allow(dead_code)]
            pub const $name: u32 = OpCode::$variant as u32;
        )*
    }
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

opcodes! {
    MOVE => Move,
    LOADI => LoadI,
    LOADF => LoadF,
    LOADK => LoadK,
    LOADKX => LoadKX,
    LOADFALSE => LoadFalse,
    LFALSESKIP => LFalseSkip,
    LOADTRUE => LoadTrue,
    LOADNIL => LoadNil,
    GETUPVAL => GetUpval,
    SETUPVAL => SetUpval,
    GETTABUP => GetTabUp,
    GETTABLE => GetTable,
    GETI => GetI,
    GETFIELD => GetField,
    SETTABUP => SetTabUp,
    SETTABLE => SetTable,
    SETI => SetI,
    SETFIELD => SetField,
    NEWTABLE => NewTable,
    SELF => Self_,
    ADDI => AddI,
    ADDK => AddK,
    SUBK => SubK,
    MULK => MulK,
    MODK => ModK,
    POWK => PowK,
    DIVK => DivK,
    IDIVK => IDivK,
    BANDK => BAndK,
    BORK => BOrK,
    BXORK => BXorK,
    SHRI => ShrI,
    SHLI => ShlI,
    ADD => Add,
    SUB => Sub,
    MUL => Mul,
    MOD => Mod,
    POW => Pow,
    DIV => Div,
    IDIV => IDiv,
    BAND => BAnd,
    BOR => BOr,
    BXOR => BXor,
    SHL => Shl,
    SHR => Shr,
    MMBIN => MmBin,
    MMBINI => MmBinI,
    MMBINK => MmBinK,
    UNM => Unm,
    BNOT => BNot,
    NOT => Not,
    LEN => Len,
    CONCAT => Concat,
    CLOSE => Close,
    TBC => Tbc,
    JMP => Jmp,
    EQ => Eq,
    LT => Lt,
    LE => Le,
    EQK => EqK,
    EQI => EqI,
    LTI => LtI,
    LEI => LeI,
    GTI => GtI,
    GEI => GeI,
    TEST => Test,
    TESTSET => TestSet,
    CALL => Call,
    TAILCALL => TailCall,
    RETURN => Return,
    RETURN0 => Return0,
    RETURN1 => Return1,
    FORLOOP => ForLoop,
    FORPREP => ForPrep,
    TFORPREP => TForPrep,
    TFORCALL => TForCall,
    TFORLOOP => TForLoop,
    SETLIST => SetList,
    CLOSURE => Closure,
    VARARG => VarArg,
    VARARGPREP => VarArgPrep,
    EXTRAARG => ExtraArg,
}

impl OpCode {
    pub fn modes(self) -> &'static Modes {
        &OPMODES[self as u8 as usize]
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpMode {
    ABC,
    ABx,
    AsBx,
    Ax,
    IsJ,
}

#[derive(Debug, Clone, Copy)]
pub struct Modes {
    pub mode: OpMode,
    pub mm: bool,
    pub ot: bool,
    pub it: bool,
    pub test: bool,
    pub set_a: bool,
}

impl Modes {
    const fn new(mm: u8, ot: u8, it: u8, t: u8, a: u8, mode: OpMode) -> Self {
        Modes {
            mode,
            mm: mm != 0,
            ot: ot != 0,
            it: it != 0,
            test: t != 0,
            set_a: a != 0,
        }
    }
}

use OpMode::*;

pub const OPMODES: &[Modes] = &[
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, AsBx),
    Modes::new(0, 0, 0, 0, 1, AsBx),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(1, 0, 0, 0, 0, ABC),
    Modes::new(1, 0, 0, 0, 0, ABC),
    Modes::new(1, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, IsJ),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 0, ABC),
    Modes::new(0, 0, 0, 1, 1, ABC),
    Modes::new(0, 1, 1, 0, 1, ABC),
    Modes::new(0, 1, 1, 0, 1, ABC),
    Modes::new(0, 0, 1, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 0, 0, 0, 0, ABx),
    Modes::new(0, 0, 0, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 0, 1, 0, 0, ABC),
    Modes::new(0, 0, 0, 0, 1, ABx),
    Modes::new(0, 1, 0, 0, 1, ABC),
    Modes::new(0, 0, 1, 0, 1, ABC),
    Modes::new(0, 0, 0, 0, 0, Ax),
];
