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

        $(pub const $name: u8 = OpCode::$variant as u8;)*
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
