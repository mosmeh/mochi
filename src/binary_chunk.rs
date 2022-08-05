mod deserialize;
mod serialize;

pub use deserialize::{load, DeserializeError};
pub use serialize::dump;

const LUA_VERSION_MAJOR: u8 = 5;
const LUA_VERSION_MINOR: u8 = 4;
const LUAC_VERSION: u8 = LUA_VERSION_MAJOR * 16 + LUA_VERSION_MINOR;
const LUAC_FORMAT: u8 = 0;

const LUA_TNIL: u8 = 0;
const LUA_TBOOLEAN: u8 = 1;
const LUA_TNUMBER: u8 = 3;
const LUA_TSTRING: u8 = 4;

const LUA_VNIL: u8 = LUA_TNIL;
const LUA_VFALSE: u8 = LUA_TBOOLEAN;
const LUA_VTRUE: u8 = LUA_TBOOLEAN | (1 << 4);
const LUA_VNUMINT: u8 = LUA_TNUMBER;
const LUA_VNUMFLT: u8 = LUA_TNUMBER | (1 << 4);
const LUA_VSHRSHR: u8 = LUA_TSTRING;
const LUA_VLNGSHR: u8 = LUA_TSTRING | (1 << 4);
