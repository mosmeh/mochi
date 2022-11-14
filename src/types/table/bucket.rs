use super::{Integer, LuaString, NativeClosure, NativeFunction, Number, Table, Value};
use crate::{
    gc::{GarbageCollect, Gc, GcCell},
    types::{LuaClosure, LuaThread, UserData},
};

// for tighter packing,
// - Value is decomposed into Tag and Payload
// - next_index is limited to 32 bit
// - next_index: Option<u32> is decomposed into bool and u32
#[derive(Clone, Default)]
pub struct Bucket<'gc, 'a> {
    key_tag: Tag,
    key_payload: Payload<'gc, 'a>,
    value_tag: Tag,
    value_payload: Payload<'gc, 'a>,
    has_next: bool,
    next_index: u32,
}

unsafe impl GarbageCollect for Bucket<'_, '_> {
    fn trace(&self, tracer: &mut crate::gc::Tracer) {
        if self.has_value() {
            debug_assert!(self.has_key());
            self.key().trace(tracer);
            self.value().trace(tracer);
        }
    }
}

impl<'gc, 'a> Bucket<'gc, 'a> {
    pub fn key(&self) -> Value<'gc, 'a> {
        unsafe { Value::from_parts(self.key_tag, self.key_payload) }
    }

    pub fn value(&self) -> Value<'gc, 'a> {
        unsafe { Value::from_parts(self.value_tag, self.value_payload) }
    }

    pub fn has_key(&self) -> bool {
        self.key_tag != Tag::Nil
    }

    pub fn has_value(&self) -> bool {
        self.value_tag != Tag::Nil
    }

    pub fn matches(&self, key: Value<'gc, 'a>) -> bool {
        unsafe { key.eq_parts(self.key_tag, self.key_payload) }
    }

    pub fn matches_integer(&self, key: Integer) -> bool {
        self.key_tag == Tag::Integer && unsafe { self.key_payload.integer } == key
    }

    pub fn matches_string(&self, key: LuaString<'gc, 'a>) -> bool {
        self.key_tag == Tag::String && unsafe { self.key_payload.string } == key
    }

    pub fn has_next(&self) -> bool {
        self.has_next
    }

    pub fn next_index(&self) -> Option<usize> {
        self.has_next.then_some(self.next_index as usize)
    }

    pub fn set_next_index(&mut self, next_index: impl Into<Option<usize>>) {
        if let Some(next_index) = next_index.into() {
            self.has_next = true;
            self.next_index = next_index.try_into().unwrap();
        } else {
            self.has_next = false;
        }
    }

    pub fn set_new_item(&mut self, key: Value<'gc, 'a>, value: Value<'gc, 'a>) {
        debug_assert!(!key.is_nil());
        debug_assert!(!value.is_nil());
        (self.key_tag, self.key_payload) = key.into_parts();
        (self.value_tag, self.value_payload) = value.into_parts();
    }

    pub fn update_or_remove_item(&mut self, value: Value<'gc, 'a>) {
        debug_assert!(self.has_key());
        if value.is_nil() {
            self.value_tag = Tag::Nil;
        } else {
            (self.value_tag, self.value_payload) = value.into_parts();
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tag {
    Nil,
    Boolean,
    Integer,
    Number,
    NativeFunction,
    String,
    Table,
    LuaClosure,
    NativeClosure,
    UserData,
    Thread,
}

impl Default for Tag {
    fn default() -> Self {
        Self::Nil
    }
}

#[derive(Clone, Copy)]
union Payload<'gc, 'a> {
    nil: (),
    boolean: bool,
    integer: Integer,
    number: Number,
    native_func: NativeFunction,
    string: LuaString<'gc, 'a>,
    table: GcCell<'gc, 'a, Table<'gc, 'a>>,
    lua_closure: Gc<'gc, 'a, LuaClosure<'gc, 'a>>,
    native_closure: Gc<'gc, 'a, NativeClosure<'gc, 'a>>,
    user_data: GcCell<'gc, 'a, UserData<'gc, 'a>>,
    thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
}

impl Default for Payload<'_, '_> {
    fn default() -> Self {
        Self { nil: () }
    }
}

impl<'gc, 'a> Value<'gc, 'a> {
    unsafe fn from_parts(tag: Tag, payload: Payload<'gc, 'a>) -> Self {
        match tag {
            Tag::Nil => Self::Nil,
            Tag::Boolean => Self::Boolean(payload.boolean),
            Tag::Integer => Self::Integer(payload.integer),
            Tag::Number => Self::Number(payload.number),
            Tag::NativeFunction => Self::NativeFunction(payload.native_func),
            Tag::String => Self::String(payload.string),
            Tag::Table => Self::Table(payload.table),
            Tag::LuaClosure => Self::LuaClosure(payload.lua_closure),
            Tag::NativeClosure => Self::NativeClosure(payload.native_closure),
            Tag::UserData => Self::UserData(payload.user_data),
            Tag::Thread => Self::Thread(payload.thread),
        }
    }

    unsafe fn eq_parts(&self, tag: Tag, payload: Payload<'gc, 'a>) -> bool {
        match self {
            Value::Nil => tag == Tag::Nil,
            Value::Boolean(b) => tag == Tag::Boolean && *b == payload.boolean,
            Value::Integer(i) => tag == Tag::Integer && *i == payload.integer,
            Value::Number(x) => tag == Tag::Number && *x == payload.number,
            Value::NativeFunction(f) => tag == Tag::NativeFunction && *f == payload.native_func,
            Value::String(s) => tag == Tag::String && *s == payload.string,
            Value::Table(t) => tag == Tag::Table && GcCell::ptr_eq(t, &payload.table),
            Value::LuaClosure(l) => tag == Tag::LuaClosure && Gc::ptr_eq(l, &payload.lua_closure),
            Value::NativeClosure(n) => {
                tag == Tag::NativeClosure && Gc::ptr_eq(n, &payload.native_closure)
            }
            Value::UserData(u) => tag == Tag::UserData && GcCell::ptr_eq(u, &payload.user_data),
            Value::Thread(t) => tag == Tag::Thread && GcCell::ptr_eq(t, &payload.thread),
        }
    }

    fn into_parts(self) -> (Tag, Payload<'gc, 'a>) {
        match self {
            Self::Nil => (Tag::Nil, Payload::default()),
            Self::Boolean(boolean) => (Tag::Boolean, Payload { boolean }),
            Self::Integer(integer) => (Tag::Integer, Payload { integer }),
            Self::Number(number) => (Tag::Number, Payload { number }),
            Self::NativeFunction(native_func) => (Tag::NativeFunction, Payload { native_func }),
            Self::String(string) => (Tag::String, Payload { string }),
            Self::Table(table) => (Tag::Table, Payload { table }),
            Self::LuaClosure(lua_closure) => (Tag::LuaClosure, Payload { lua_closure }),
            Self::NativeClosure(native_closure) => (Tag::NativeClosure, Payload { native_closure }),
            Self::UserData(user_data) => (Tag::UserData, Payload { user_data }),
            Self::Thread(thread) => (Tag::Thread, Payload { thread }),
        }
    }
}
