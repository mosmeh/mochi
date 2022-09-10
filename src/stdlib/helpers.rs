use crate::{
    gc::GcContext,
    runtime::ErrorKind,
    types::{Integer, Number, Table, Value},
};
use std::{
    borrow::{Borrow, Cow},
    cell::{Ref, RefMut},
};

pub struct Argument<'gc> {
    value: Option<Value<'gc>>,
    nth: usize,
}

impl<'gc> Argument<'gc> {
    pub fn get(&self) -> Option<Value<'gc>> {
        self.value
    }

    pub fn to_value(&self) -> Result<Value<'gc>, ErrorKind> {
        self.to_type("value", |value| Some(*value))
    }

    pub fn to_integer(&self) -> Result<Integer, ErrorKind> {
        self.to_type("integer", Value::to_integer)
    }

    pub fn to_integer_or(&self, default: Integer) -> Result<Integer, ErrorKind> {
        if self.value.is_some() {
            self.to_type("integer", Value::to_integer)
        } else {
            Ok(default)
        }
    }

    pub fn to_integer_or_else<F>(&self, f: F) -> Result<Integer, ErrorKind>
    where
        F: FnOnce() -> Integer,
    {
        if self.value.is_some() {
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
        if self.value.is_some() {
            self.to_type("string", Value::to_string)
        } else {
            Ok(default.into())
        }
    }

    pub fn borrow_as_table(&self) -> Result<Ref<'_, Table<'gc>>, ErrorKind> {
        self.to_type("table", Value::borrow_as_table)
    }

    pub fn borrow_as_table_mut(
        &self,
        gc: &'gc GcContext,
    ) -> Result<RefMut<'_, Table<'gc>>, ErrorKind> {
        self.to_type("table", |value| value.borrow_as_table_mut(gc))
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

pub trait StackExt<'gc> {
    fn callee(&self) -> Value<'gc>;
    fn args(&self) -> &[Value<'gc>];
    fn arg(&self, nth: usize) -> Argument<'gc>;
}

impl<'gc, T> StackExt<'gc> for T
where
    T: Borrow<[Value<'gc>]>,
{
    fn callee(&self) -> Value<'gc> {
        self.borrow()[0]
    }

    fn args(&self) -> &[Value<'gc>] {
        &self.borrow()[1..]
    }

    fn arg(&self, nth: usize) -> Argument<'gc> {
        Argument {
            value: self.borrow().get(nth + 1).copied(),
            nth,
        }
    }
}
