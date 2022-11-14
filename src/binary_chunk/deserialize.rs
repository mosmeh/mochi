use crate::{
    gc::GcContext,
    runtime::Instruction,
    types::{
        Integer, LineRange, LuaClosureProto, LuaString, Number, RegisterIndex, UpvalueDescription,
        UpvalueIndex, Value,
    },
};
use bstr::B;
use byteorder::{NativeEndian, ReadBytesExt};
use std::{io::Read, mem::size_of};

#[derive(thiserror::Error, Debug)]
pub enum DeserializeError {
    #[error("bad magic")]
    BadMagic,

    #[error("unsupported bytecode Lua version")]
    UnsupportedVersion,

    #[error("unsupported bytecode format")]
    UnsupportedFormat,

    #[error("integer overflow")]
    IntegerOverflow,

    #[error("bad format for constant string")]
    BadStringConstant,

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub fn load<'gc, 'a, R: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut R,
) -> Result<LuaClosureProto<'gc, 'a>, DeserializeError> {
    if reader.read_u32::<NativeEndian>()? != u32::from_ne_bytes(super::LUA_SIGNATURE) {
        return Err(DeserializeError::BadMagic);
    }

    if reader.read_u8()? != super::LUAC_VERSION {
        return Err(DeserializeError::UnsupportedVersion);
    }
    if reader.read_u8()? != super::LUAC_FORMAT {
        return Err(DeserializeError::UnsupportedFormat);
    }

    let mut data = [0u8; 6];
    reader.read_exact(&mut data)?;
    if data != super::LUAC_DATA {
        return Err(DeserializeError::UnsupportedFormat);
    }

    if reader.read_u8()? as usize != size_of::<Instruction>()
        || reader.read_u8()? as usize != size_of::<Integer>()
        || reader.read_u8()? as usize != size_of::<Number>()
    {
        return Err(DeserializeError::UnsupportedFormat);
    }

    if reader.read_i64::<NativeEndian>()? != super::LUAC_INT
        || reader.read_f64::<NativeEndian>()? != super::LUAC_NUM
    {
        return Err(DeserializeError::UnsupportedFormat);
    }

    let num_upvalues = reader.read_u8()?;
    let default_source = gc.allocate_string(B("=?"));
    let proto = load_function(gc, reader, default_source)?;
    assert_eq!(num_upvalues as usize, proto.upvalues.len());

    Ok(proto)
}

fn load_function<'gc, 'a, R: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut R,
    parent_source: LuaString<'gc, 'a>,
) -> Result<LuaClosureProto<'gc, 'a>, DeserializeError> {
    let source = load_nullable_str(gc, reader)?.unwrap_or(parent_source);
    let line_defined = load_int(reader)?;
    let last_line_defined = load_int(reader)?;
    reader.read_u8()?; // numparams
    reader.read_u8()?; // is_vararg
    let max_stack_size = reader.read_u8()?;

    let code = load_code(reader)?;
    let constants = load_constants(gc, reader)?;
    let upvalues = load_upvalues(reader)?;
    let protos = load_protos(gc, reader, source)?;

    let n = load_int(reader)?;
    let mut line_info = vec![0u8; n as usize];
    reader.read_exact(&mut line_info)?;

    // AbsLineInfo
    let n = load_int(reader)?;
    for _ in 0..n {
        load_int(reader)?; // pc
        load_int(reader)?; // line
    }

    // LocVar
    let n = load_int(reader)?;
    for _ in 0..n {
        load_nullable_str(gc, reader)?; // varname
        load_int(reader)?; // startpc
        load_int(reader)?; // endpc
    }

    // Upvalue
    let n = load_int(reader)?;
    for _ in 0..n {
        load_nullable_str(gc, reader)?; // name
    }

    Ok(LuaClosureProto {
        max_stack_size,
        lines_defined: if line_defined > 0 {
            LineRange::Lines(line_defined..=last_line_defined)
        } else {
            LineRange::File
        },
        constants: constants.into(),
        code: code.into(),
        protos: protos.into_iter().map(|proto| gc.allocate(proto)).collect(),
        upvalues: upvalues.into(),
        source,
    })
}

fn load_protos<'gc, 'a, T: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut T,
    parent_source: LuaString<'gc, 'a>,
) -> Result<Vec<LuaClosureProto<'gc, 'a>>, DeserializeError> {
    let n = load_int(reader)?;
    let mut protos = Vec::with_capacity(n as usize);
    for _ in 0..n {
        protos.push(load_function(gc, reader, parent_source)?);
    }
    Ok(protos)
}

fn load_unsigned<T: Read>(reader: &mut T, mut limit: usize) -> Result<usize, DeserializeError> {
    let mut x: usize = 0;
    limit >>= 7;
    loop {
        let b = reader.read_u8()?;
        if x >= limit {
            return Err(DeserializeError::IntegerOverflow);
        }
        x = (x << 7) | (b & 0x7f) as usize;
        if (b & 0x80) != 0 {
            return Ok(x);
        }
    }
}

fn load_size<R: Read>(reader: &mut R) -> Result<usize, DeserializeError> {
    load_unsigned(reader, !0)
}

fn load_nullable_str<'gc, 'a, R: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut R,
) -> Result<Option<LuaString<'gc, 'a>>, DeserializeError> {
    let size = load_size(reader)?;
    if size == 0 {
        return Ok(None);
    }
    let mut buf = vec![0u8; size - 1];
    reader.read_exact(&mut buf)?;
    Ok(Some(gc.allocate_string(buf)))
}

fn load_str<'gc, 'a, R: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut R,
) -> Result<LuaString<'gc, 'a>, DeserializeError> {
    match load_nullable_str(gc, reader) {
        Ok(Some(s)) => Ok(s),
        Ok(None) => Err(DeserializeError::BadStringConstant),
        Err(e) => Err(e),
    }
}

fn load_int<R: Read>(reader: &mut R) -> Result<u32, DeserializeError> {
    let int = load_unsigned(reader, u32::MAX as usize)?
        .try_into()
        .unwrap();
    Ok(int)
}

fn load_code<R: Read>(reader: &mut R) -> Result<Vec<Instruction>, DeserializeError> {
    let n = load_int(reader)?;
    let mut code = Vec::<Instruction>::with_capacity(n as usize);
    for _ in 0..n {
        code.push(Instruction(reader.read_u32::<NativeEndian>()?));
    }
    Ok(code)
}

fn load_constants<'gc, 'a, R: Read>(
    gc: &'a GcContext<'gc>,
    reader: &mut R,
) -> Result<Vec<Value<'gc, 'a>>, DeserializeError> {
    let n = load_int(reader)?;
    let mut constants = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let ty = reader.read_u8()?;
        let value = match ty {
            super::LUA_VNIL => Value::Nil,
            super::LUA_VFALSE => Value::Boolean(false),
            super::LUA_VTRUE => Value::Boolean(true),
            super::LUA_VNUMFLT => Value::Number(reader.read_f64::<NativeEndian>()?),
            super::LUA_VNUMINT => Value::Integer(reader.read_i64::<NativeEndian>()?),
            super::LUA_VSHRSHR | super::LUA_VLNGSHR => Value::String(load_str(gc, reader)?),
            _ => unreachable!(),
        };
        constants.push(value);
    }
    Ok(constants)
}

fn load_upvalues<R: Read>(reader: &mut R) -> Result<Vec<UpvalueDescription>, DeserializeError> {
    let n = load_int(reader)?;
    let mut upvalues = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let in_stack = reader.read_u8()? != 0;
        let index = reader.read_u8()?;
        reader.read_u8()?; // kind

        let upvalue = if in_stack {
            UpvalueDescription::Register(RegisterIndex(index))
        } else {
            UpvalueDescription::Upvalue(UpvalueIndex(index))
        };
        upvalues.push(upvalue);
    }
    Ok(upvalues)
}
