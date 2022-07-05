macro_rules! impl_try_from_insn {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        impl $name {
            pub fn new(v: u8) -> Self {
                match v {
                    $(x if x == $name::$vname as u8 => $name::$vname,)*
                    _ => unimplemented!()
                }
            }
        }
    }
}

impl_try_from_insn! {
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
