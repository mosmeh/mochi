use crate::{
    gc::{GcCell, GcContext},
    runtime::ErrorKind,
    types::{
        Integer, LuaThread, NativeFunction, NativeFunctionPtr, Number, Table, Type, UserData, Value,
    },
};
use std::{
    any::Any,
    borrow::{Borrow, Cow},
    cell::RefMut,
};

pub trait ArgumentsExt<'gc> {
    fn callee(&self) -> Value<'gc>;
    fn without_callee(&self) -> &[Value<'gc>];
    fn nth(&self, nth: usize) -> Argument<'gc>;
}

impl<'gc, T> ArgumentsExt<'gc> for T
where
    T: Borrow<[Value<'gc>]>,
{
    fn callee(&self) -> Value<'gc> {
        self.borrow()[0]
    }

    fn without_callee(&self) -> &[Value<'gc>] {
        &self.borrow()[1..]
    }

    fn nth(&self, nth: usize) -> Argument<'gc> {
        Argument {
            value: self.borrow().get(nth).copied(),
            nth,
        }
    }
}

pub struct Argument<'gc> {
    value: Option<Value<'gc>>,
    nth: usize,
}

impl<'gc> Argument<'gc> {
    pub fn is_present(&self) -> bool {
        !matches!(self.value, Some(Value::Nil) | None)
    }

    pub fn is_none(&self) -> bool {
        self.value.is_none()
    }

    pub fn get(&self) -> Option<Value<'gc>> {
        self.value
    }

    pub fn as_value(&self) -> Result<Value<'gc>, ErrorKind> {
        self.to_type("value", |value| Some(*value))
    }

    pub fn to_boolean(&self) -> Result<bool, ErrorKind> {
        self.to_type("boolean", |value| Some(value.to_boolean()))
    }

    pub fn to_integer(&self) -> Result<Integer, ErrorKind> {
        self.to_type("integer", Value::to_integer)
    }

    pub fn to_integer_or(&self, default: Integer) -> Result<Integer, ErrorKind> {
        if self.is_present() {
            self.to_type("integer", Value::to_integer)
        } else {
            Ok(default)
        }
    }

    pub fn to_integer_or_else<F>(&self, f: F) -> Result<Integer, ErrorKind>
    where
        F: FnOnce() -> Integer,
    {
        if self.is_present() {
            self.to_type("integer", Value::to_integer)
        } else {
            Ok(f())
        }
    }

    pub fn to_number(&self) -> Result<Number, ErrorKind> {
        self.to_type("number", Value::to_number)
    }

    pub fn to_string(&self) -> Result<Cow<'_, [u8]>, ErrorKind> {
        self.to_type("string", Value::to_string)
    }

    pub fn to_string_or<'a, I>(&'a self, default: I) -> Result<Cow<'a, [u8]>, ErrorKind>
    where
        I: Into<Cow<'a, [u8]>>,
    {
        if self.is_present() {
            self.to_type("string", Value::to_string)
        } else {
            Ok(default.into())
        }
    }

    pub fn as_table(&self) -> Result<GcCell<'gc, Table<'gc>>, ErrorKind> {
        self.to_type("table", Value::as_table)
    }

    pub fn as_thread(&self) -> Result<GcCell<'gc, LuaThread<'gc>>, ErrorKind> {
        self.to_type("thread", Value::as_thread)
    }

    pub fn as_userdata<T: Any>(&self) -> Result<GcCell<'gc, UserData<'gc>>, ErrorKind> {
        self.to_type("userdata", |value| value.as_userdata::<T>())
    }

    pub fn borrow_as_userdata_mut<'a, T: Any>(
        &'a self,
        gc: &'gc GcContext,
    ) -> Result<RefMut<'a, T>, ErrorKind> {
        self.to_type("userdata", |value| value.borrow_as_userdata_mut(gc))
    }

    pub fn ensure_function(&self) -> Result<Value<'gc>, ErrorKind> {
        self.to_type("function", |value| {
            (value.ty() == Type::Function).then_some(*value)
        })
    }

    fn to_type<'a, F, T>(&'a self, name: &'static str, convert: F) -> Result<T, ErrorKind>
    where
        F: Fn(&'a Value<'gc>) -> Option<T> + 'a,
    {
        let got_type = if let Some(value) = &self.value {
            if let Some(value) = convert(value) {
                return Ok(value);
            }
            Some(value.ty().name())
        } else {
            None
        };
        Err(ErrorKind::ArgumentTypeError {
            nth: self.nth,
            expected_type: name,
            got_type,
        })
    }
}

pub fn set_functions_to_table<'gc>(
    gc: &'gc GcContext,
    table: &mut Table<'gc>,
    functions: &[(&[u8], NativeFunctionPtr)],
) {
    for (name, func) in functions {
        table.set_field(gc.allocate_string(*name), NativeFunction::new(*func));
    }
}
