use super::{
    LUAC_FORMAT, LUAC_VERSION, LUA_VFALSE, LUA_VLNGSHR, LUA_VNIL, LUA_VNUMFLT, LUA_VNUMINT,
    LUA_VTRUE,
};
use crate::{
    gc::Gc,
    runtime::Instruction,
    types::{Integer, LineRange, LuaClosureProto, LuaString, Number, UpvalueDescription, Value},
};
use byteorder::{NativeEndian, WriteBytesExt};
use std::io::Write;

pub fn dump<W: Write>(writer: &mut W, proto: &LuaClosureProto) -> std::io::Result<()> {
    writer.write_all(b"\x1bLua")?;
    writer.write_u8(LUAC_VERSION)?;
    writer.write_u8(LUAC_FORMAT)?;

    const LUAC_DATA: &[u8; 6] = b"\x19\x93\r\n\x1a\n";
    writer.write_all(LUAC_DATA)?;

    writer.write_u8(std::mem::size_of::<Instruction>() as u8)?;
    writer.write_u8(std::mem::size_of::<Integer>() as u8)?;
    writer.write_u8(std::mem::size_of::<Number>() as u8)?;

    const LUAC_INT: Integer = 0x5678;
    const LUAC_NUM: Number = 370.5;
    writer.write_i64::<NativeEndian>(LUAC_INT)?;
    writer.write_f64::<NativeEndian>(LUAC_NUM)?;

    writer.write_u8(proto.upvalues.len() as u8)?;
    dump_function(writer, proto)?;

    Ok(())
}

fn dump_function<W: Write>(writer: &mut W, proto: &LuaClosureProto) -> std::io::Result<()> {
    let (line_defined, last_line_defined) = match &proto.lines_defined {
        LineRange::File => (0, 0),
        LineRange::Lines(range) => (*range.start(), *range.end()),
    };

    dump_string(writer, proto.source)?; // source
    dump_int(writer, line_defined)?;
    dump_int(writer, last_line_defined)?;
    writer.write_u8(0)?; // numparams
    writer.write_u8(0)?; // is_vararg
    writer.write_u8(proto.max_stack_size)?;

    dump_code(writer, &proto.code)?;
    dump_constants(writer, &proto.constants)?;
    dump_upvalues(writer, &proto.upvalues)?;
    dump_protos(writer, &proto.protos)?;

    dump_int(writer, 0)?; // lineinfo
    dump_int(writer, 0)?; // abslineinfo
    dump_int(writer, 0)?; // locvar
    dump_int(writer, 0)?; // upvalue

    Ok(())
}

fn dump_protos<W: Write>(writer: &mut W, protos: &[Gc<LuaClosureProto>]) -> std::io::Result<()> {
    dump_size(writer, protos.len())?;
    for proto in protos {
        dump_function(writer, proto)?;
    }
    Ok(())
}

fn dump_string<'gc, W, S>(writer: &mut W, string: S) -> std::io::Result<()>
where
    W: Write,
    S: Into<Option<LuaString<'gc>>>,
{
    if let Some(string) = string.into() {
        dump_size(writer, string.len() + 1)?;
        writer.write_all(string.as_ref())?;
    } else {
        dump_size(writer, 0)?;
    }
    Ok(())
}

fn dump_size<W: Write>(writer: &mut W, mut x: usize) -> std::io::Result<()> {
    let mut buf = Vec::new();
    loop {
        buf.push((x & 0x7f) as u8);
        x >>= 7;
        if x == 0 {
            break;
        }
    }
    buf.reverse();
    *buf.last_mut().unwrap() |= 0x80;
    writer.write_all(&buf)?;
    Ok(())
}

fn dump_int<W: Write>(writer: &mut W, int: u32) -> std::io::Result<()> {
    dump_size(writer, int as usize)
}

fn dump_code<W: Write>(writer: &mut W, instructions: &[Instruction]) -> std::io::Result<()> {
    dump_size(writer, instructions.len())?;
    for insn in instructions {
        writer.write_u32::<NativeEndian>(insn.0)?;
    }
    Ok(())
}

fn dump_constants<W: Write>(writer: &mut W, constants: &[Value]) -> std::io::Result<()> {
    dump_size(writer, constants.len())?;
    for constant in constants {
        match constant {
            Value::Nil => {
                writer.write_u8(LUA_VNIL)?;
            }
            Value::Boolean(false) => {
                writer.write_u8(LUA_VFALSE)?;
            }
            Value::Boolean(true) => {
                writer.write_u8(LUA_VTRUE)?;
            }
            Value::Integer(i) => {
                writer.write_u8(LUA_VNUMINT)?;
                writer.write_i64::<NativeEndian>(*i)?;
            }
            Value::Number(x) => {
                writer.write_u8(LUA_VNUMFLT)?;
                writer.write_f64::<NativeEndian>(*x)?;
            }
            Value::String(s) => {
                writer.write_u8(LUA_VLNGSHR)?;
                dump_string(writer, *s)?;
            }
            _ => unreachable!(),
        };
    }
    Ok(())
}

fn dump_upvalues<W: Write>(writer: &mut W, upvalues: &[UpvalueDescription]) -> std::io::Result<()> {
    dump_size(writer, upvalues.len())?;
    for upvalue in upvalues {
        match upvalue {
            UpvalueDescription::Register(index) => {
                writer.write_u8(1)?; // instack
                writer.write_u8(index.0)?; // idx
                writer.write_u8(0)?; // kind
            }
            UpvalueDescription::Upvalue(index) => {
                writer.write_u8(0)?; // instack
                writer.write_u8(index.0)?; // idx
                writer.write_u8(0)?; // kind
            }
        }
    }
    Ok(())
}
