mod function;
mod string;
mod table;

pub use function::{
    LineRange, LuaClosure, LuaClosureProto, NativeClosure, NativeClosureFn, NativeFunction,
    NativeFunctionPtr, StackWindow, Upvalue, UpvalueDescription,
};
pub use string::LuaString;
pub use table::Table;

use crate::gc::{GarbageCollect, Gc, GcCell, GcHeap, Tracer};
use std::{
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
pub enum Value<'gc> {
    Nil,
    Boolean(bool),
    Integer(Integer),
    Number(Number),
    NativeFunction(NativeFunction),
    String(LuaString<'gc>),
    Table(GcCell<'gc, Table<'gc>>),
    LuaClosure(Gc<'gc, LuaClosure<'gc>>),
    NativeClosure(Gc<'gc, NativeClosure>),
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

impl<'gc> From<Gc<'gc, NativeClosure>> for Value<'gc> {
    fn from(x: Gc<'gc, NativeClosure>) -> Self {
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
            (Self::NativeFunction(lhs), Self::NativeFunction(rhs)) => lhs == rhs,
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
            Self::NativeFunction(x) => x.hash(state),
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
            Self::Number(x) => write!(f, "{}", x),
            Self::NativeFunction(x) => write!(f, "{}", x),
            Self::String(x) => write!(f, "{}", x),
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

unsafe impl GarbageCollect for Value<'_> {
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

impl<'gc> Value<'gc> {
    pub fn ty(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Boolean(_) => Type::Boolean,
            Value::Integer(_) | Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Table(_) => Type::Table,
            Value::NativeFunction(_) | Value::LuaClosure(_) | Value::NativeClosure(_) => {
                Type::Function
            }
        }
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

    pub fn to_lua_string(&self, heap: &'gc GcHeap) -> Option<LuaString<'gc>> {
        match self {
            Self::String(x) => Some(*x),
            Self::Integer(x) => Some(heap.allocate_string(x.to_string().into_bytes())),
            Self::Number(x) => Some(heap.allocate_string(x.to_string().into_bytes())),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<Ref<Table<'gc>>> {
        if let Self::Table(x) = self {
            Some(x.borrow())
        } else {
            None
        }
    }

    pub fn as_table_mut(&self, heap: &GcHeap) -> Option<RefMut<Table<'gc>>> {
        if let Self::Table(x) = self {
            Some(x.borrow_mut(heap))
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

    pub fn metatable(&self) -> Option<GcCell<'gc, Table<'gc>>> {
        if let Self::Table(table) = self {
            table.borrow().metatable()
        } else {
            unimplemented!()
        }
    }
}
