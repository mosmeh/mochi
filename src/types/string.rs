use crate::gc::{BoxedString, GarbageCollect, Gc, Tracer};
use std::{cmp::Ordering, fmt::Write, hash::Hash, ops::Deref, str::Utf8Error};

#[derive(Clone, Copy)]
pub struct LuaString<'gc>(pub(crate) Gc<'gc, BoxedString>);

impl std::fmt::Debug for LuaString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_char('"')?;
        for ch in self.0.as_bytes() {
            match *ch {
                b'"' => f.write_str("\\\"")?,
                b'\\' => f.write_str("\\\\")?,
                0x7 => f.write_str("\\a")?,
                0x8 => f.write_str("\\b")?,
                0xc => f.write_str("\\f")?,
                b'\n' => f.write_str("\\n")?,
                b'\r' => f.write_str("\\r")?,
                b'\t' => f.write_str("\\t")?,
                0xb => f.write_str("\\v")?,
                ch if ch == b' ' || ch.is_ascii_graphic() => f.write_char(char::from(ch))?,
                ch => write!(f, "\\{ch:03}")?,
            }
        }
        f.write_char('"')
    }
}

impl Deref for LuaString<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

impl AsRef<[u8]> for LuaString<'_> {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl PartialEq for LuaString<'_> {
    fn eq(&self, other: &LuaString) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LuaString<'_> {}

impl PartialOrd for LuaString<'_> {
    fn partial_cmp(&self, other: &LuaString) -> Option<Ordering> {
        if Gc::ptr_eq(&self.0, &other.0) {
            Some(Ordering::Equal)
        } else {
            self.as_bytes().partial_cmp(other.as_ref())
        }
    }
}

impl Hash for LuaString<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

unsafe impl GarbageCollect for LuaString<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

impl LuaString<'_> {
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn as_str(&self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(self.as_bytes())
    }
}
