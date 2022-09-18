mod function;
mod string;
mod table;
mod thread;
mod user_data;

pub use function::{
    Action, LineRange, LuaClosure, LuaClosureProto, NativeClosure, NativeClosureFn, NativeFunction,
    NativeFunctionPtr, RegisterIndex, Upvalue, UpvalueDescription, UpvalueIndex,
};
pub use string::LuaString;
pub use table::{Table, TableError};
pub(crate) use thread::ThreadStatus;
pub use thread::{LuaThread, StackWindow, TracebackFrame};
pub use user_data::UserData;

use crate::gc::{GarbageCollect, Gc, GcCell, GcContext, Tracer};
use std::{
    any::Any,
    borrow::Cow,
    cell::{Ref, RefMut},
    fmt::Display,
    io::Write,
};

macro_rules! types {
    ($($variant:ident => $name:tt,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Type {
            $($variant,)*
        }

        impl Type {
            pub const COUNT: usize = crate::count!($($variant)*);

            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name,)*
                }
            }
        }
    }
}

types! {
    Nil => "nil",
    Boolean => "boolean",
    Number => "number",
    String => "string",
    Table => "table",
    Function => "function",
    UserData => "userdata",
    Thread => "thread",
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

pub type Integer = i64;
pub type Number = f64;

#[derive(Debug, Clone, Copy)]
pub enum Value<'gc> {
    Nil,
    Boolean(bool),
    Integer(Integer),
    Number(Number),
    NativeFunction(NativeFunction),
    String(LuaString<'gc>),
    Table(GcCell<'gc, Table<'gc>>),
    LuaClosure(Gc<'gc, LuaClosure<'gc>>),
    NativeClosure(Gc<'gc, NativeClosure<'gc>>),
    UserData(GcCell<'gc, UserData<'gc>>),
    Thread(GcCell<'gc, LuaThread<'gc>>),
}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::Nil
    }
}

impl From<bool> for Value<'_> {
    fn from(x: bool) -> Self {
        Self::Boolean(x)
    }
}

impl From<Integer> for Value<'_> {
    fn from(x: Integer) -> Self {
        Self::Integer(x)
    }
}

impl From<Number> for Value<'_> {
    fn from(x: Number) -> Self {
        Self::Number(x)
    }
}

impl From<NativeFunction> for Value<'_> {
    fn from(x: NativeFunction) -> Self {
        Self::NativeFunction(x)
    }
}

impl<'gc> From<LuaString<'gc>> for Value<'gc> {
    fn from(x: LuaString<'gc>) -> Self {
        Self::String(x)
    }
}

impl<'gc> From<GcCell<'gc, Table<'gc>>> for Value<'gc> {
    fn from(x: GcCell<'gc, Table<'gc>>) -> Self {
        Self::Table(x)
    }
}

impl<'gc> From<Gc<'gc, LuaClosure<'gc>>> for Value<'gc> {
    fn from(x: Gc<'gc, LuaClosure<'gc>>) -> Self {
        Self::LuaClosure(x)
    }
}

impl<'gc> From<Gc<'gc, NativeClosure<'gc>>> for Value<'gc> {
    fn from(x: Gc<'gc, NativeClosure<'gc>>) -> Self {
        Self::NativeClosure(x)
    }
}

impl<'gc> From<GcCell<'gc, UserData<'gc>>> for Value<'gc> {
    fn from(x: GcCell<'gc, UserData<'gc>>) -> Self {
        Self::UserData(x)
    }
}

impl<'gc> From<GcCell<'gc, LuaThread<'gc>>> for Value<'gc> {
    fn from(x: GcCell<'gc, LuaThread<'gc>>) -> Self {
        Self::Thread(x)
    }
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Integer(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Number(rhs)) => *lhs as Number == *rhs,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Self::Number(lhs), Self::Integer(rhs)) => *lhs == *rhs as Number,
            (Self::NativeFunction(lhs), Self::NativeFunction(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Table(lhs), Self::Table(rhs)) => GcCell::ptr_eq(lhs, rhs),
            (Self::LuaClosure(lhs), Self::LuaClosure(rhs)) => Gc::ptr_eq(lhs, rhs),
            (Self::NativeClosure(lhs), Self::NativeClosure(rhs)) => Gc::ptr_eq(lhs, rhs),
            (Self::UserData(lhs), Self::UserData(rhs)) => GcCell::ptr_eq(lhs, rhs),
            (Self::Thread(lhs), Self::Thread(rhs)) => GcCell::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

impl Eq for Value<'_> {}

impl std::hash::Hash for Value<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Nil => {}
            Self::Boolean(x) => x.hash(state),
            Self::Integer(x) => x.hash(state),
            Self::Number(x) => x.to_bits().hash(state),
            Self::NativeFunction(x) => x.hash(state),
            Self::String(x) => x.hash(state),
            Self::Table(x) => x.as_ptr().hash(state),
            Self::LuaClosure(x) => x.as_ptr().hash(state),
            Self::NativeClosure(x) => x.as_ptr().hash(state),
            Self::UserData(x) => x.as_ptr().hash(state),
            Self::Thread(x) => x.as_ptr().hash(state),
        }
    }
}

unsafe impl GarbageCollect for Value<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Self::String(x) => x.trace(tracer),
            Self::Table(x) => x.trace(tracer),
            Self::LuaClosure(x) => x.trace(tracer),
            Self::NativeClosure(x) => x.trace(tracer),
            Self::UserData(x) => x.trace(tracer),
            Self::Thread(x) => x.trace(tracer),
            _ => (),
        }
    }
}

impl<'gc> Value<'gc> {
    pub fn fmt_bytes(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Self::Nil => f.write_all(b"nil"),
            Self::Boolean(x) => write!(f, "{}", x),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Number(x) => write!(f, "{}", x),
            Self::NativeFunction(x) => write!(f, "function: {:?}", x.as_ptr()),
            Self::String(x) => f.write_all(x.as_bytes()),
            Self::Table(x) => write!(f, "table: {:?}", x.as_ptr()),
            Self::LuaClosure(x) => {
                write!(f, "function: {:?}", x.as_ptr())
            }
            Self::NativeClosure(x) => {
                write!(f, "function: {:?}", x.as_ptr())
            }
            Self::UserData(x) => {
                write!(f, "userdata: {:?}", x.as_ptr())
            }
            Self::Thread(x) => {
                write!(f, "thread: {:?}", x.as_ptr())
            }
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::Nil => Type::Nil,
            Self::Boolean(_) => Type::Boolean,
            Self::Integer(_) | Self::Number(_) => Type::Number,
            Self::String(_) => Type::String,
            Self::Table(_) => Type::Table,
            Self::NativeFunction(_) | Self::LuaClosure(_) | Self::NativeClosure(_) => {
                Type::Function
            }
            Self::UserData(_) => Type::UserData,
            Self::Thread(_) => Type::Thread,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn to_boolean(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn to_number(&self) -> Option<Number> {
        match self {
            Self::Number(x) => Some(*x),
            Self::Integer(x) => Some(*x as Number),
            Self::String(x) => x.as_str().ok().and_then(|s| s.parse().ok()),
            _ => None,
        }
    }

    pub fn to_number_without_string_coercion(&self) -> Option<Number> {
        match self {
            Self::Number(x) => Some(*x),
            Self::Integer(x) => Some(*x as Number),
            _ => None,
        }
    }

    pub fn to_integer(&self) -> Option<Integer> {
        match self {
            Self::Number(x) => {
                let y = x.floor() as Integer;
                if *x == y as Number {
                    Some(y)
                } else {
                    None
                }
            }
            Self::Integer(x) => Some(*x),
            Self::String(x) => x.as_str().ok().and_then(|s| s.parse().ok()),
            _ => None,
        }
    }

    pub fn to_integer_without_string_coercion(&self) -> Option<Integer> {
        match self {
            Self::Number(x) => {
                let y = x.floor() as Integer;
                if *x == y as Number {
                    Some(y)
                } else {
                    None
                }
            }
            Self::Integer(x) => Some(*x),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<Cow<[u8]>> {
        match self {
            Self::String(x) => Some(Cow::Borrowed(x.as_bytes())),
            Self::Integer(x) => {
                let mut bytes = Vec::new();
                write!(&mut bytes, "{}", x).ok()?;
                Some(Cow::Owned(bytes))
            }
            Self::Number(x) => {
                let mut bytes = Vec::new();
                write!(&mut bytes, "{}", x).ok()?;
                Some(Cow::Owned(bytes))
            }
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<GcCell<'gc, Table<'gc>>> {
        if let Self::Table(x) = self {
            Some(*x)
        } else {
            None
        }
    }

    pub fn borrow_as_table(&self) -> Option<Ref<Table<'gc>>> {
        if let Self::Table(x) = self {
            Some(x.borrow())
        } else {
            None
        }
    }

    pub fn borrow_as_table_mut(&self, gc: &'gc GcContext) -> Option<RefMut<Table<'gc>>> {
        if let Self::Table(x) = self {
            Some(x.borrow_mut(gc))
        } else {
            None
        }
    }

    pub fn as_lua_closure(&self) -> Option<&LuaClosure<'gc>> {
        if let Self::LuaClosure(x) = self {
            Some(x.as_ref())
        } else {
            None
        }
    }

    pub fn as_native_closure(&self) -> Option<&NativeClosure<'gc>> {
        if let Self::NativeClosure(x) = self {
            Some(x.as_ref())
        } else {
            None
        }
    }

    pub fn as_thread(&self) -> Option<GcCell<'gc, LuaThread<'gc>>> {
        if let Self::Thread(x) = self {
            Some(*x)
        } else {
            None
        }
    }

    pub fn borrow_as_thread(&self) -> Option<Ref<LuaThread<'gc>>> {
        if let Self::Thread(x) = self {
            Some(x.borrow())
        } else {
            None
        }
    }

    pub fn borrow_as_thread_mut(&self, gc: &'gc GcContext) -> Option<RefMut<LuaThread<'gc>>> {
        if let Self::Thread(x) = self {
            Some(x.borrow_mut(gc))
        } else {
            None
        }
    }

    pub fn as_userdata<T: Any>(&self) -> Option<GcCell<'gc, UserData<'gc>>> {
        if let Self::UserData(x) = self {
            if x.borrow().is::<T>() {
                return Some(*x);
            }
        }
        None
    }

    pub fn metatable(&self) -> Option<GcCell<'gc, Table<'gc>>> {
        match self {
            Self::Table(table) => table.borrow().metatable(),
            Self::UserData(ud) => ud.borrow().metatable(),
            _ => None,
        }
    }
}
