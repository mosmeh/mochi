macro_rules! opcodes {
    ($($name:ident => $variant:ident ($($properties:expr),*),)*) => {
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

        impl OpCode {
            pub const fn properties(self) -> &'static OpProperties {
                use OpMode::*;
                const PROPERTIES: [OpProperties; crate::count!($($variant)*)] = [$(OpProperties::new($($properties,)*),)*];
                &PROPERTIES[self as usize]
            }
        }
    }
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

opcodes! {
    MOVE       => Move       (0, 0, 0, 0, 1, ABC),
    LOADI      => LoadI      (0, 0, 0, 0, 1, AsBx),
    LOADF      => LoadF      (0, 0, 0, 0, 1, AsBx),
    LOADK      => LoadK      (0, 0, 0, 0, 1, ABx),
    LOADKX     => LoadKX     (0, 0, 0, 0, 1, ABx),
    LOADFALSE  => LoadFalse  (0, 0, 0, 0, 1, ABC),
    LFALSESKIP => LFalseSkip (0, 0, 0, 0, 1, ABC),
    LOADTRUE   => LoadTrue   (0, 0, 0, 0, 1, ABC),
    LOADNIL    => LoadNil    (0, 0, 0, 0, 1, ABC),
    GETUPVAL   => GetUpval   (0, 0, 0, 0, 1, ABC),
    SETUPVAL   => SetUpval   (0, 0, 0, 0, 0, ABC),
    GETTABUP   => GetTabUp   (0, 0, 0, 0, 1, ABC),
    GETTABLE   => GetTable   (0, 0, 0, 0, 1, ABC),
    GETI       => GetI       (0, 0, 0, 0, 1, ABC),
    GETFIELD   => GetField   (0, 0, 0, 0, 1, ABC),
    SETTABUP   => SetTabUp   (0, 0, 0, 0, 0, ABC),
    SETTABLE   => SetTable   (0, 0, 0, 0, 0, ABC),
    SETI       => SetI       (0, 0, 0, 0, 0, ABC),
    SETFIELD   => SetField   (0, 0, 0, 0, 0, ABC),
    NEWTABLE   => NewTable   (0, 0, 0, 0, 1, ABC),
    SELF       => Self_      (0, 0, 0, 0, 1, ABC),
    ADDI       => AddI       (0, 0, 0, 0, 1, ABC),
    ADDK       => AddK       (0, 0, 0, 0, 1, ABC),
    SUBK       => SubK       (0, 0, 0, 0, 1, ABC),
    MULK       => MulK       (0, 0, 0, 0, 1, ABC),
    MODK       => ModK       (0, 0, 0, 0, 1, ABC),
    POWK       => PowK       (0, 0, 0, 0, 1, ABC),
    DIVK       => DivK       (0, 0, 0, 0, 1, ABC),
    IDIVK      => IDivK      (0, 0, 0, 0, 1, ABC),
    BANDK      => BAndK      (0, 0, 0, 0, 1, ABC),
    BORK       => BOrK       (0, 0, 0, 0, 1, ABC),
    BXORK      => BXorK      (0, 0, 0, 0, 1, ABC),
    SHRI       => ShrI       (0, 0, 0, 0, 1, ABC),
    SHLI       => ShlI       (0, 0, 0, 0, 1, ABC),
    ADD        => Add        (0, 0, 0, 0, 1, ABC),
    SUB        => Sub        (0, 0, 0, 0, 1, ABC),
    MUL        => Mul        (0, 0, 0, 0, 1, ABC),
    MOD        => Mod        (0, 0, 0, 0, 1, ABC),
    POW        => Pow        (0, 0, 0, 0, 1, ABC),
    DIV        => Div        (0, 0, 0, 0, 1, ABC),
    IDIV       => IDiv       (0, 0, 0, 0, 1, ABC),
    BAND       => BAnd       (0, 0, 0, 0, 1, ABC),
    BOR        => BOr        (0, 0, 0, 0, 1, ABC),
    BXOR       => BXor       (0, 0, 0, 0, 1, ABC),
    SHL        => Shl        (0, 0, 0, 0, 1, ABC),
    SHR        => Shr        (0, 0, 0, 0, 1, ABC),
    MMBIN      => MmBin      (1, 0, 0, 0, 0, ABC),
    MMBINI     => MmBinI     (1, 0, 0, 0, 0, ABC),
    MMBINK     => MmBinK     (1, 0, 0, 0, 0, ABC),
    UNM        => Unm        (0, 0, 0, 0, 1, ABC),
    BNOT       => BNot       (0, 0, 0, 0, 1, ABC),
    NOT        => Not        (0, 0, 0, 0, 1, ABC),
    LEN        => Len        (0, 0, 0, 0, 1, ABC),
    CONCAT     => Concat     (0, 0, 0, 0, 1, ABC),
    CLOSE      => Close      (0, 0, 0, 0, 0, ABC),
    TBC        => Tbc        (0, 0, 0, 0, 0, ABC),
    JMP        => Jmp        (0, 0, 0, 0, 0, SJ),
    EQ         => Eq         (0, 0, 0, 1, 0, ABC),
    LT         => Lt         (0, 0, 0, 1, 0, ABC),
    LE         => Le         (0, 0, 0, 1, 0, ABC),
    EQK        => EqK        (0, 0, 0, 1, 0, ABC),
    EQI        => EqI        (0, 0, 0, 1, 0, ABC),
    LTI        => LtI        (0, 0, 0, 1, 0, ABC),
    LEI        => LeI        (0, 0, 0, 1, 0, ABC),
    GTI        => GtI        (0, 0, 0, 1, 0, ABC),
    GEI        => GeI        (0, 0, 0, 1, 0, ABC),
    TEST       => Test       (0, 0, 0, 1, 0, ABC),
    TESTSET    => TestSet    (0, 0, 0, 1, 1, ABC),
    CALL       => Call       (0, 1, 1, 0, 1, ABC),
    TAILCALL   => TailCall   (0, 1, 1, 0, 1, ABC),
    RETURN     => Return     (0, 0, 1, 0, 0, ABC),
    RETURN0    => Return0    (0, 0, 0, 0, 0, ABC),
    RETURN1    => Return1    (0, 0, 0, 0, 0, ABC),
    FORLOOP    => ForLoop    (0, 0, 0, 0, 1, ABx),
    FORPREP    => ForPrep    (0, 0, 0, 0, 1, ABx),
    TFORPREP   => TForPrep   (0, 0, 0, 0, 0, ABx),
    TFORCALL   => TForCall   (0, 0, 0, 0, 0, ABC),
    TFORLOOP   => TForLoop   (0, 0, 0, 0, 1, ABx),
    SETLIST    => SetList    (0, 0, 1, 0, 0, ABC),
    CLOSURE    => Closure    (0, 0, 0, 0, 1, ABx),
    VARARG     => VarArg     (0, 1, 0, 0, 1, ABC),
    VARARGPREP => VarArgPrep (0, 0, 1, 0, 1, ABC),
    EXTRAARG   => ExtraArg   (0, 0, 0, 0, 0, Ax),
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[allow(clippy::upper_case_acronyms)]
pub enum OpMode {
    ABC,
    ABx,
    AsBx,
    Ax,
    SJ,
}

pub struct OpProperties {
    pub mode: OpMode,
    /// instruction is an MM instruction (call a metamethod)
    pub calls_metamethod: bool,
    /// instruction sets 'L->top' for next instruction (when C == 0)
    pub is_out_top: bool,
    /// instruction uses 'L->top' set by previous instruction (when B == 0)
    pub is_in_top: bool,
    /// operator is a test (next instruction must be a jump)
    pub is_test: bool,
    /// instruction set register A
    pub sets_a: bool,
}

impl OpProperties {
    const fn new(mm: u8, ot: u8, it: u8, t: u8, a: u8, mode: OpMode) -> Self {
        OpProperties {
            mode,
            calls_metamethod: mm != 0,
            is_out_top: ot != 0,
            is_in_top: it != 0,
            is_test: t != 0,
            sets_a: a != 0,
        }
    }
}
