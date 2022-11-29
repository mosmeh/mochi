mod function;
mod string;
mod table;
mod thread;
mod user_data;

pub(crate) use function::Upvalue;
pub use function::{
    LineRange, LuaClosure, LuaClosureProto, NativeClosure, NativeFunction, NativeFunctionPtr,
    RegisterIndex, UpvalueDescription, UpvalueIndex,
};
pub use string::LuaString;
pub use table::{Table, TableError};
pub(crate) use thread::ThreadStatus;
pub use thread::{LuaThread, TracebackFrame};
pub use user_data::UserData;

use crate::{
    gc::{GarbageCollect, Gc, GcBind, GcCell, GcContext, Trace, Tracer},
    number_is_valid_integer,
    string::{parse_positive_hex_float, parse_positive_integer_with_base, trim_whitespaces},
};
use bstr::ByteSlice;
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
pub enum Value<'gc, 'a> {
    Nil,
    Boolean(bool),
    Integer(Integer),
    Number(Number),
    NativeFunction(NativeFunction),
    String(LuaString<'gc, 'a>),
    Table(GcCell<'gc, 'a, Table<'gc, 'a>>),
    LuaClosure(Gc<'gc, 'a, LuaClosure<'gc, 'a>>),
    NativeClosure(Gc<'gc, 'a, NativeClosure<'gc, 'a>>),
    UserData(GcCell<'gc, 'a, UserData<'gc, 'a>>),
    Thread(GcCell<'gc, 'a, LuaThread<'gc, 'a>>),
}

impl Default for Value<'_, '_> {
    fn default() -> Self {
        Self::Nil
    }
}

impl From<bool> for Value<'_, '_> {
    fn from(x: bool) -> Self {
        Self::Boolean(x)
    }
}

impl From<Integer> for Value<'_, '_> {
    fn from(x: Integer) -> Self {
        Self::Integer(x)
    }
}

impl From<Number> for Value<'_, '_> {
    fn from(x: Number) -> Self {
        Self::Number(x)
    }
}

impl From<NativeFunction> for Value<'_, '_> {
    fn from(x: NativeFunction) -> Self {
        Self::NativeFunction(x)
    }
}

impl<'gc, 'a> From<LuaString<'gc, 'a>> for Value<'gc, 'a> {
    fn from(x: LuaString<'gc, 'a>) -> Self {
        Self::String(x)
    }
}

impl<'gc, 'a> From<GcCell<'gc, 'a, Table<'gc, 'a>>> for Value<'gc, 'a> {
    fn from(x: GcCell<'gc, 'a, Table<'gc, 'a>>) -> Self {
        Self::Table(x)
    }
}

impl<'gc, 'a> From<Gc<'gc, 'a, LuaClosure<'gc, 'a>>> for Value<'gc, 'a> {
    fn from(x: Gc<'gc, 'a, LuaClosure<'gc, 'a>>) -> Self {
        Self::LuaClosure(x)
    }
}

impl<'gc, 'a> From<Gc<'gc, 'a, NativeClosure<'gc, 'a>>> for Value<'gc, 'a> {
    fn from(x: Gc<'gc, 'a, NativeClosure<'gc, 'a>>) -> Self {
        Self::NativeClosure(x)
    }
}

impl<'gc, 'a> From<GcCell<'gc, 'a, UserData<'gc, 'a>>> for Value<'gc, 'a> {
    fn from(x: GcCell<'gc, 'a, UserData<'gc, 'a>>) -> Self {
        Self::UserData(x)
    }
}

impl<'gc, 'a> From<GcCell<'gc, 'a, LuaThread<'gc, 'a>>> for Value<'gc, 'a> {
    fn from(x: GcCell<'gc, 'a, LuaThread<'gc, 'a>>) -> Self {
        Self::Thread(x)
    }
}

impl PartialEq for Value<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Integer(rhs)) => lhs == rhs,
            (Self::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Self::Integer(lhs), Self::Number(rhs)) => {
                number_is_valid_integer(*rhs) && (*lhs == *rhs as Integer)
            }
            (Self::Number(lhs), Self::Integer(rhs)) => {
                number_is_valid_integer(*lhs) && (*lhs as Integer == *rhs)
            }
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

impl Eq for Value<'_, '_> {}

impl std::hash::Hash for Value<'_, '_> {
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

unsafe impl Trace for Value<'_, '_> {
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

unsafe impl GarbageCollect for Value<'_, '_> {}

unsafe impl<'a, 'gc: 'a> GcBind<'gc, 'a> for Value<'gc, '_> {
    type Bound = Value<'gc, 'a>;
}

impl<'gc, 'a> Value<'gc, 'a> {
    pub fn fmt_bytes(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Self::Nil => f.write_all(b"nil"),
            Self::Boolean(x) => write!(f, "{x}"),
            Self::Integer(x) => write!(f, "{x}"),
            Self::Number(x) => fmt_number(f, *x),
            Self::NativeFunction(x) => write!(f, "function: {:p}", x.as_ptr()),
            Self::String(x) => f.write_all(x.as_bytes()),
            Self::Table(x) => write!(f, "table: {:p}", x.as_ptr()),
            Self::LuaClosure(x) => {
                write!(f, "function: {:p}", x.as_ptr())
            }
            Self::NativeClosure(x) => {
                write!(f, "function: {:p}", x.as_ptr())
            }
            Self::UserData(x) => {
                write!(f, "userdata: {:p}", x.as_ptr())
            }
            Self::Thread(x) => {
                write!(f, "thread: {:p}", x.as_ptr())
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
            Self::String(s) => trim_whitespaces(s).to_str().ok().and_then(parse_number),
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
            Self::Number(x) if number_is_valid_integer(*x) => Some(*x as Integer),
            Self::Integer(x) => Some(*x),
            Self::String(s) => {
                let s = trim_whitespaces(s).to_str().ok()?;
                if let Some(i) = parse_integer(s) {
                    return Some(i);
                }
                if let Some(x) = parse_number(s) {
                    if number_is_valid_integer(x) {
                        return Some(x as Integer);
                    }
                }
                None
            }
            _ => None,
        }
    }

    pub fn to_integer_without_string_coercion(&self) -> Option<Integer> {
        match self {
            Self::Number(x) if number_is_valid_integer(*x) => Some(*x as Integer),
            Self::Integer(x) => Some(*x),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<Cow<[u8]>> {
        match self {
            Self::String(x) => Some(Cow::Borrowed(x.as_bytes())),
            Self::Integer(x) => {
                let mut bytes = Vec::new();
                write!(&mut bytes, "{x}").ok()?;
                Some(Cow::Owned(bytes))
            }
            Self::Number(x) => {
                let mut bytes = Vec::new();
                fmt_number(&mut bytes, *x).ok()?;
                Some(Cow::Owned(bytes))
            }
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<GcCell<'gc, 'a, Table<'gc, 'a>>> {
        if let Self::Table(x) = self {
            Some(*x)
        } else {
            None
        }
    }

    pub fn borrow_as_table(&self, gc: &'a GcContext<'gc>) -> Option<Ref<Table<'gc, 'a>>> {
        if let Self::Table(x) = self {
            Some(x.borrow(gc))
        } else {
            None
        }
    }

    pub fn borrow_as_table_mut(&self, gc: &'a GcContext<'gc>) -> Option<RefMut<Table<'gc, 'a>>> {
        if let Self::Table(x) = self {
            Some(x.borrow_mut(gc))
        } else {
            None
        }
    }

    pub fn as_lua_closure(&self) -> Option<&LuaClosure<'gc, 'a>> {
        if let Self::LuaClosure(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn as_native_closure(&self) -> Option<&NativeClosure<'gc, 'a>> {
        if let Self::NativeClosure(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn as_thread(&self) -> Option<GcCell<'gc, 'a, LuaThread<'gc, 'a>>> {
        if let Self::Thread(x) = self {
            Some(*x)
        } else {
            None
        }
    }

    pub fn borrow_as_thread(&self, gc: &'a GcContext<'gc>) -> Option<Ref<LuaThread<'gc, 'a>>> {
        if let Self::Thread(x) = self {
            Some(x.borrow(gc))
        } else {
            None
        }
    }

    pub fn borrow_as_thread_mut(
        &self,
        gc: &'a GcContext<'gc>,
    ) -> Option<RefMut<LuaThread<'gc, 'a>>> {
        if let Self::Thread(x) = self {
            Some(x.borrow_mut(gc))
        } else {
            None
        }
    }

    pub fn as_userdata<T: Any>(
        &self,
        gc: &GcContext<'gc>,
    ) -> Option<GcCell<'gc, 'a, UserData<'gc, 'a>>> {
        match self {
            Self::UserData(ud) if ud.borrow(gc).is::<T>() => Some(*ud),
            _ => None,
        }
    }

    pub fn borrow_as_userdata<'b, T: Any>(&self, gc: &'b GcContext<'gc>) -> Option<Ref<'b, T>> {
        if let Self::UserData(ud) = self {
            Ref::filter_map(ud.borrow(gc), |ud| ud.get()).ok()
        } else {
            None
        }
    }

    pub fn borrow_as_userdata_mut<'b, T: Any>(
        &'b self,
        gc: &'a GcContext<'gc>,
    ) -> Option<RefMut<'b, T>> {
        if let Self::UserData(ud) = self {
            RefMut::filter_map(ud.borrow_mut(gc), |ud| ud.get_mut()).ok()
        } else {
            None
        }
    }

    pub(crate) fn as_ptr(&self) -> Option<*const ()> {
        match self {
            Self::Nil | Self::Boolean(_) | Self::Integer(_) | Self::Number(_) => None,
            Self::NativeFunction(n) => Some(n.as_ptr()),
            Self::String(s) => Some(s.0.as_ptr() as *const _),
            Self::Table(t) => Some(t.as_ptr() as *const _),
            Self::LuaClosure(l) => Some(l.as_ptr() as *const _),
            Self::NativeClosure(n) => Some(n.as_ptr() as *const _),
            Self::UserData(u) => Some(u.as_ptr() as *const _),
            Self::Thread(t) => Some(t.as_ptr() as *const _),
        }
    }
}

fn parse_integer<S: AsRef<str>>(s: S) -> Option<Integer> {
    let mut s = s.as_ref();
    let sign = match s.as_bytes() {
        [b'+', ..] => {
            s = &s[1..];
            true
        }
        [b'-', ..] => {
            s = &s[1..];
            false
        }
        _ => true,
    };
    let parsed = match s.as_bytes() {
        [b'0', b'x' | b'X', ..] => parse_positive_integer_with_base(&s[2..], 16),
        _ => s.parse().ok(),
    };
    match parsed {
        Some(i) if sign => Some(i),
        Some(i) => Some(i.wrapping_neg()),
        None => None,
    }
}

fn parse_number<S: AsRef<str>>(s: S) -> Option<Number> {
    let mut s = s.as_ref();
    let sign = match s.as_bytes() {
        [b'+', ..] => {
            s = &s[1..];
            true
        }
        [b'-', ..] => {
            s = &s[1..];
            false
        }
        _ => true,
    };
    let parsed = match s.as_bytes() {
        [b'0', b'x' | b'X', rest @ ..] => parse_positive_hex_float(rest),
        s if s.eq_ignore_ascii_case(b"inf")
            || s.eq_ignore_ascii_case(b"infinity")
            || s.eq_ignore_ascii_case(b"nan") =>
        {
            return None
        }
        _ => s.parse().ok(),
    };
    match parsed {
        Some(x) if sign => Some(x),
        Some(x) => Some(-x),
        None => None,
    }
}

// sprintf("%.14g") except it does not remove suffix ".0" when x is an integer
fn fmt_number<W: std::io::Write>(writer: &mut W, x: Number) -> std::io::Result<()> {
    const PRECISION: usize = 14;

    if x == 0.0 {
        return writer.write_all(b"0.0");
    } else if x.is_nan() {
        if x.is_sign_negative() {
            writer.write_all(b"-")?;
        }
        return writer.write_all(b"nan");
    }

    let mut precision = PRECISION - 1;
    let log_x = x.abs().log10();
    if log_x < -3.0 || (precision as Number) < log_x {
        return write!(writer, "{x:.precision$e}");
    }

    precision = (precision as isize - log_x.trunc() as isize) as usize;
    if log_x < 0.0 {
        precision += 1
    }

    let s = format!("{x:.precision$}");
    let mut s = s.as_bytes();
    if !s.contains(&b'.') {
        return writer.write_all(s);
    }

    s = s.trim_end_with(|ch| ch == '0');
    writer.write_all(s)?;
    if let Some(b'.') = s.last() {
        writer.write_all(b"0")?;
    }
    Ok(())
}
