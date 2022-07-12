use crate::gc::Trace;
use bstr::ByteVec;
use std::{
    borrow::Cow,
    ffi::OsString,
    ops::{Deref, DerefMut},
    path::PathBuf,
    str::Utf8Error,
};

#[derive(Clone)]
pub struct LuaString(pub Box<[u8]>);

impl std::fmt::Debug for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_tuple("LuaString")
            .field(&String::from_utf8_lossy(&self.0))
            .finish()
    }
}

impl From<&[u8]> for LuaString {
    fn from(s: &[u8]) -> Self {
        Self(s.into())
    }
}

impl From<Vec<u8>> for LuaString {
    fn from(s: Vec<u8>) -> Self {
        Self(s.into())
    }
}

impl From<&str> for LuaString {
    fn from(s: &str) -> Self {
        Self(s.to_owned().into_bytes().into_boxed_slice())
    }
}

impl From<String> for LuaString {
    fn from(s: String) -> Self {
        Self(s.into_bytes().into_boxed_slice())
    }
}

impl From<Cow<'_, [u8]>> for LuaString {
    fn from(x: Cow<'_, [u8]>) -> Self {
        x.to_vec().into()
    }
}

impl From<Cow<'_, str>> for LuaString {
    fn from(x: Cow<'_, str>) -> Self {
        x.to_owned().into()
    }
}

impl From<Cow<'_, LuaString>> for LuaString {
    fn from(x: Cow<LuaString>) -> Self {
        x.into()
    }
}

impl TryFrom<OsString> for LuaString {
    type Error = &'static str;

    fn try_from(x: OsString) -> Result<Self, Self::Error> {
        if let Ok(vec) = Vec::from_os_string(x) {
            Ok(vec.into())
        } else {
            Err("Invalid UTF-8")
        }
    }
}

impl TryFrom<PathBuf> for LuaString {
    type Error = &'static str;

    fn try_from(x: PathBuf) -> Result<Self, Self::Error> {
        if let Ok(vec) = Vec::from_path_buf(x) {
            Ok(vec.into())
        } else {
            Err("Invalid UTF-8")
        }
    }
}

impl Deref for LuaString {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

impl DerefMut for LuaString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_bytes_mut()
    }
}

impl AsRef<[u8]> for LuaString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<T: AsRef<[u8]>> PartialEq<T> for LuaString {
    fn eq(&self, other: &T) -> bool {
        self.as_bytes() == other.as_ref()
    }
}

impl<T: AsRef<[u8]>> PartialOrd<T> for LuaString {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.as_bytes().partial_cmp(other.as_ref())
    }
}

unsafe impl Trace for LuaString {
    fn needs_trace() -> bool {
        false
    }
}

impl LuaString {
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }

    pub fn as_str(&self) -> Result<&str, Utf8Error> {
        std::str::from_utf8(self.as_bytes())
    }
}
