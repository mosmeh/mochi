use crate::{gc::GcContext, types::LuaString};
use bstr::B;

macro_rules! tag_methods {
    ($($variant:ident => $name:tt,)*) => {
        #[allow(dead_code)]
        #[derive(Clone, Copy)]
        pub enum TagMethod {
            $($variant,)*
        }

        impl TagMethod {
            pub const COUNT: usize = crate::count!($($variant)*);

            pub fn allocate_names(gc: &GcContext) -> [LuaString; Self::COUNT] {
                [
                    $(gc.allocate_string(B($name)),)*
                ]
            }
        }
    }
}

tag_methods!(
    Index => "__index",
    NewIndex => "__newindex",
    Gc => "__gc",
    Mode => "__mode",
    Len => "__len",
    Eq => "__eq",
    Add => "__add",
    Sub => "__sub",
    Mul => "__mul",
    Mod => "__mod",
    Pow => "__pow",
    Div => "__div",
    IDiv => "__idiv",
    BAnd => "__band",
    BOr => "__bor",
    BXor => "__bxor",
    Shl => "__shl",
    Shr => "__shr",
    Unm => "__unm",
    BNot => "__bnot",
    Lt => "__lt",
    Le => "__le",
    Concat => "__concat",
    Call => "__call",
    Close => "__close",
);
