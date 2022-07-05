pub mod deserialize;
pub mod gc;
pub mod types;
pub mod vm;

mod stdlib;

pub use stdlib::create_global_table;

use gc::GcHeap;
use std::{
    fmt::Debug,
    fs::File,
    io::{BufReader, Cursor, Read},
    path::Path,
};
use types::LuaClosure;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Deserialize(#[from] deserialize::DeserializeError),

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    RLua(#[from] rlua::Error),
}

pub fn load<B, S>(heap: &GcHeap, bytes: B, source: S) -> Result<LuaClosure, Error>
where
    B: AsRef<[u8]>,
    S: AsRef<[u8]>,
{
    let mut reader = Cursor::new(&bytes);
    let closure = if let Ok(closure) = deserialize::load(heap, &mut reader) {
        closure
    } else {
        let bin_bytes = rlua::Lua::new()
            .context(|ctx| ctx.load(&bytes).set_name(&source)?.into_function()?.dump())?;
        let mut reader = Cursor::new(bin_bytes);
        deserialize::load(heap, &mut reader)?
    };

    Ok(closure)
}

pub fn load_file<P: AsRef<Path>>(heap: &GcHeap, path: P) -> Result<LuaClosure, Error> {
    let mut reader = BufReader::new(File::open(&path)?);
    let mut bytes = Vec::new();
    reader.read_to_end(&mut bytes)?;
    let source = path.as_ref().as_os_str();
    #[cfg(unix)]
    use std::os::unix::ffi::OsStrExt;
    #[cfg(not(unix))]
    let source = source.to_string_lossy();
    load(heap, bytes, source.as_bytes())
}
