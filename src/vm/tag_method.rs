use crate::{gc::GcHeap, types::LuaString};
use bstr::B;

macro_rules! count  {
    () => (0);
    ($x:tt $($xs:tt)*) => (1 + count!($($xs)*));
}

macro_rules! tag_methods {
    ($($name:tt,)*) => {
        pub const COUNT: usize = count!($($name)*);

        pub fn allocate_tag_method_names(heap: &GcHeap) -> [LuaString; COUNT] {
            [
                $(heap.allocate_string(B($name)),)*
            ]
        }
    }
}

tag_methods!(
    "__index",
    "__newindex",
    "__gc",
    "__mode",
    "__len",
    "__eq",
    "__add",
    "__sub",
    "__mul",
    "__mod",
    "__pow",
    "__div",
    "__idiv",
    "__band",
    "__bor",
    "__bxor",
    "__shl",
    "__shr",
    "__unm",
    "__bnot",
    "__lt",
    "__le",
    "__concat",
    "__call",
    "__close",
);
