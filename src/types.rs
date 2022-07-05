mod closure;
mod string;
mod table;

pub use closure::{
    LineRange, LuaClosure, LuaClosureProto, NativeClosure, StackKey, Upvalue, UpvalueDescription,
};
pub use string::LuaString;
pub use table::Table;

use crate::gc::{Gc, GcCell, GcHeap, Trace, Tracer};
use std::{
    borrow::Cow,
    cell::{Ref, RefMut},
    fmt::Display,
};

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Nil,
    Boolean,
    Number,
    String,
    Table,
    Function,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Nil => f.write_str("nil"),
            Type::Boolean => f.write_str("boolean"),
            Type::Number => f.write_str("number"),
            Type::String => f.write_str("string"),
            Type::Table => f.write_str("table"),
            Type::Function => f.write_str("function"),
        }
    }
}

pub type Integer = i64;
pub type Number = f64;

#[derive(Debug, Clone, Copy)]
pub enum Value<'a> {
    Nil,
    Boolean(bool),
    Integer(Integer),
    Number(Number),
    String(Gc<'a, LuaString>),
    Table(GcCell<'a, Table<'a>>),
    LuaClosure(Gc<'a, LuaClosure<'a>>),
    NativeClosure(Gc<'a, NativeClosure>),
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

impl<'a> From<Gc<'a, LuaString>> for Value<'a> {
    fn from(x: Gc<'a, LuaString>) -> Self {
        Self::String(x)
    }
}

impl<'a> From<GcCell<'a, Table<'a>>> for Value<'a> {
    fn from(x: GcCell<'a, Table<'a>>) -> Self {
        Self::Table(x)
    }
}

impl<'a> From<Gc<'a, LuaClosure<'a>>> for Value<'a> {
    fn from(x: Gc<'a, LuaClosure<'a>>) -> Self {
        Self::LuaClosure(x)
    }
}

impl<'a> From<Gc<'a, NativeClosure>> for Value<'a> {
    fn from(x: Gc<'a, NativeClosure>) -> Self {
        Self::NativeClosure(x)
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
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::Table(lhs), Self::Table(rhs)) => GcCell::ptr_eq(lhs, rhs),
            (Self::LuaClosure(lhs), Self::LuaClosure(rhs)) => Gc::ptr_eq(lhs, rhs),
            (Self::NativeClosure(lhs), Self::NativeClosure(rhs)) => Gc::ptr_eq(lhs, rhs),
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
            Self::String(x) => x.hash(state),
            Self::Table(x) => x.as_ptr().hash(state),
            Self::LuaClosure(x) => x.as_ptr().hash(state),
            Self::NativeClosure(x) => x.as_ptr().hash(state),
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Nil => f.write_str("nil"),
            Self::Boolean(x) => write!(f, "{}", x),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Number(x) => write!(f, "{:<14}", x),
            Self::String(x) => f.write_str(&String::from_utf8_lossy(x)),
            Self::Table(x) => write!(f, "table: {:?}", x.as_ptr()),
            Self::LuaClosure(x) => {
                write!(f, "function: {:?}", x.as_ptr())
            }
            Self::NativeClosure(x) => {
                write!(f, "function: {:?}", x.as_ptr())
            }
        }
    }
}

unsafe impl Trace for Value<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Value::String(x) => x.trace(tracer),
            Value::Table(x) => x.trace(tracer),
            Value::LuaClosure(x) => x.trace(tracer),
            Value::NativeClosure(x) => x.trace(tracer),
            _ => (),
        }
    }
}

impl<'a> Value<'a> {
    pub fn as_boolean(&self) -> bool {
        !matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Self::Number(x) => Some(*x),
            Self::Integer(x) => Some(*x as Number),
            Self::String(x) => x.as_str().ok().and_then(|s| s.parse().ok()),
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<Integer> {
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

    pub fn as_lua_string(&self) -> Option<Cow<LuaString>> {
        match self {
            Self::String(x) => Some(Cow::Borrowed(x)),
            Self::Integer(x) => Some(Cow::Owned(x.to_string().into())),
            Self::Number(x) => Some(Cow::Owned(x.to_string().into())),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<Ref<Table<'a>>> {
        if let Self::Table(x) = self {
            Some(x.borrow())
        } else {
            None
        }
    }

    pub fn as_table_mut(&self, heap: &GcHeap) -> Option<RefMut<Table<'a>>> {
        if let Self::Table(x) = self {
            Some(x.borrow_mut(heap))
        } else {
            None
        }
    }

    pub fn as_lua_closure(&self) -> Option<&LuaClosure<'a>> {
        if let Self::LuaClosure(x) = self {
            Some(x.as_ref())
        } else {
            None
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Boolean(_) => Type::Boolean,
            Value::Integer(_) | Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Table(_) => Type::Table,
            Value::LuaClosure(_) | Value::NativeClosure(_) => Type::Function,
        }
    }
}
