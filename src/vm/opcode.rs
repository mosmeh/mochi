macro_rules! impl_from_u8 {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($variant:ident,)*
    }) => {
        $(#[$meta])* $vis enum $name {
            $($variant,)*
        }

        impl From<u8> for $name {
            fn from(v: u8) -> Self {
                match v {
                    $(x if x == $name::$variant as u8 => $name::$variant,)*
                    _ => unimplemented!()
                }
            }
        }
    }
}

impl_from_u8! {
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Move,
    LoadI,
    LoadF,
    LoadK,
    LoadKX,
    LoadFalse,
    LFalseSkip,
    LoadTrue,
    LoadNil,
    GetUpval,
    SetUpval,
    GetTabUp,
    GetTable,
    GetI,
    GetField,
    SetTabUp,
    SetTable,
    SetI,
    SetField,
    NewTable,
    Self_,
    AddI,
    AddK,
    SubK,
    MulK,
    ModK,
    PowK,
    DivK,
    IDivK,
    BAndK,
    BOrK,
    BXorK,
    ShrI,
    ShlI,
    Add,
    Sub,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    BAnd,
    BOr,
    BXor,
    Shl,
    Shr,
    MmBin,
    MmBinI,
    MmBinK,
    Unm,
    BNot,
    Not,
    Len,
    Concat,
    Close,
    Tbc,
    Jmp,
    Eq,
    Lt,
    Le,
    EqK,
    EqI,
    LtI,
    LeI,
    GtI,
    GeI,
    Test,
    TestSet,
    Call,
    TailCall,
    Return,
    Return0,
    Return1,
    ForLoop,
    ForPrep,
    TForPrep,
    TForCall,
    TForLoop,
    SetList,
    Closure,
    VarArg,
    VarArgPrep,
    ExtraArg,
}
}
