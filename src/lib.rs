pub mod binary_chunk;
pub mod gc;
pub mod runtime;
pub mod types;

#[cfg(not(feature = "luac"))]
mod codegen;
#[cfg(not(feature = "luac"))]
mod lexer;
#[cfg(not(feature = "luac"))]
mod parser;

mod stdlib;

use bstr::ByteVec;
use gc::GcContext;
use std::{
    fmt::Debug,
    fs::File,
    io::{BufReader, Cursor, Read},
    path::Path,
};
use types::{LuaClosure, LuaClosureProto};

pub const LUA_VERSION: (u8, u8) = (5, 4);

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Deserialize(#[from] binary_chunk::DeserializeError),

    #[cfg(not(feature = "luac"))]
    #[error(transparent)]
    Parse(#[from] parser::ParseError),

    #[cfg(not(feature = "luac"))]
    #[error(transparent)]
    Codegen(#[from] codegen::CodegenError),

    #[error(transparent)]
    Runtime(#[from] runtime::RuntimeError),

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[cfg(feature = "luac")]
    #[error(transparent)]
    RLua(#[from] rlua::Error),
}

pub fn load<B, S>(gc: &GcContext, bytes: B, source: S) -> Result<LuaClosureProto, Error>
where
    B: AsRef<[u8]>,
    S: AsRef<[u8]>,
{
    let mut reader = Cursor::new(&bytes);
    if let Ok(closure) = binary_chunk::load(gc, &mut reader) {
        return Ok(closure);
    }

    #[cfg(feature = "luac")]
    {
        let bin_bytes = rlua::Lua::new()
            .context(|ctx| ctx.load(&bytes).set_name(&source)?.into_function()?.dump())?;
        let mut reader = Cursor::new(bin_bytes);
        let proto = binary_chunk::load(gc, &mut reader)?;
        Ok(proto)
    }

    #[cfg(not(feature = "luac"))]
    {
        let reader = Cursor::new(&bytes);
        let chunk = parser::parse(gc, reader)?;
        let source = gc.allocate_string(source.as_ref());
        let proto = codegen::codegen(gc, source, chunk)?;
        Ok(proto)
    }
}

pub fn load_file<P: AsRef<Path>>(gc: &GcContext, path: P) -> Result<LuaClosureProto, Error> {
    let mut reader = BufReader::new(File::open(&path)?);
    let mut bytes = Vec::new();
    reader.read_to_end(&mut bytes)?;
    let source = Vec::from_path_lossy(path.as_ref());
    load(gc, bytes, source)
}
