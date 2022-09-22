pub mod binary_chunk;
pub mod gc;
pub mod runtime;
pub mod types;

#[cfg(not(feature = "luac"))]
pub mod codegen;
#[cfg(not(feature = "luac"))]
mod lexer;
#[cfg(not(feature = "luac"))]
pub mod parser;

mod stdlib;

use bstr::ByteVec;
use gc::GcContext;
use std::{
    borrow::Cow,
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
        let chunk = parser::parse(gc, String::from_utf8_lossy(source.as_ref()), reader)?;
        let source = gc.allocate_string(source.as_ref());
        let proto = codegen::codegen(gc, source, chunk)?;
        Ok(proto)
    }
}

pub fn load_file<P: AsRef<Path>>(gc: &GcContext, path: P) -> Result<LuaClosureProto, Error> {
    let mut reader = BufReader::new(File::open(&path)?);
    let mut bytes = Vec::new();
    reader.read_to_end(&mut bytes)?;
    let mut source = b"@".to_vec();
    source.extend_from_slice(&Vec::from_path_lossy(path.as_ref()));
    load(gc, bytes, source)
}

macro_rules! count  {
    () => (0);
    ($x:tt $($xs:tt)*) => (1 + crate::count!($($xs)*));
}

pub(crate) use count;

fn chunk_id_from_source(source: &str) -> Cow<str> {
    const LUA_IDSIZE: usize = 60;
    const RETS: &str = "...";
    const PRE: &str = "[string \"";
    const POS: &str = "\"]";

    match source.chars().next() {
        Some('=') => source.chars().take(LUA_IDSIZE).skip(1).collect(),
        Some('@') => {
            let filename_len = source.len() - 1;
            if filename_len < LUA_IDSIZE {
                source.strip_prefix('@').unwrap().into()
            } else {
                let reversed: String = source
                    .chars()
                    .rev()
                    .take(filename_len.min(LUA_IDSIZE - RETS.len() - 1))
                    .collect();
                let mut ellipsized = RETS.to_owned();
                ellipsized.extend(reversed.chars().rev());
                ellipsized.into()
            }
        }
        _ => {
            const MAX_STR_LEN: usize = LUA_IDSIZE - PRE.len() - RETS.len() - POS.len() - 1;
            let mut lines = source.lines();
            let first_line = lines.next().unwrap_or_default();
            let is_multiline = lines.next().is_some();
            if !is_multiline && first_line.len() < MAX_STR_LEN {
                format!("{PRE}{first_line}{POS}").into()
            } else {
                let truncated: String = first_line.chars().take(MAX_STR_LEN).collect();
                format!("{PRE}{truncated}{RETS}{POS}").into()
            }
        }
    }
}
