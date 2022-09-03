use anyhow::{Error, Result};
use bstr::{ByteSlice, ByteVec, B};
use clap::{Parser, Subcommand};
use mochi_lua::{
    gc::GcHeap,
    runtime::{OpCode, Runtime},
    types::{Integer, LineRange, LuaClosureProto, Table, UpvalueDescription, Value},
};
use rustyline::{error::ReadlineError, Editor};
use std::{fs::File, io::BufWriter, path::PathBuf};

#[cfg(all(feature = "jemalloc", not(target_env = "msvc")))]
#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, Parser)]
#[clap(version, about, args_conflicts_with_subcommands = true)]
struct Args {
    #[clap(value_parser)]
    script: Option<PathBuf>,

    #[clap(value_parser)]
    args: Vec<String>,

    /// Enter interactive mode after executing <SCRIPT>
    #[clap(value_parser, short, default_value_t = false)]
    interactive: bool,

    #[clap(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
    Compile(CompileCommand),
}

#[derive(Debug, Parser)]
struct CompileCommand {
    #[clap(value_parser)]
    filename: PathBuf,

    /// List (use -l -l for full listing)
    #[clap(parse(from_occurrences), short)]
    list: usize,

    /// Output to file <OUTPUT>
    #[clap(value_parser, short, default_value = "luac.out")]
    output: PathBuf,

    /// Parse only
    #[clap(value_parser, short)]
    parse_only: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let Some(command) = args.command {
        match command {
            Command::Compile(command) => command.run()?,
        }
        return Ok(());
    }

    let mut runtime = Runtime::new();
    runtime.heap().with(|gc, vm| -> Result<()> {
        let vm = vm.borrow();
        vm.load_stdlib(gc);

        let mut arg = Table::new();
        if let Some(script) = &args.script {
            let script = Vec::from_path_lossy(script);
            arg.set(0, gc.allocate_string(script))?;
        }
        for (i, x) in args.args.into_iter().enumerate() {
            arg.set((i + 1) as Integer, gc.allocate_string(x.into_bytes()))?;
        }
        vm.globals()
            .borrow_mut(gc)
            .set_field(gc.allocate_string(B("arg")), gc.allocate_cell(arg));

        Ok(())
    })?;

    if let Some(script) = args.script {
        runtime
            .execute(|gc, _| mochi_lua::load_file(gc, script).map_err(Into::into))
            .map_err(Error::msg)?;

        if !args.interactive {
            return Ok(());
        }
    }

    let mut rl = Editor::<()>::new()?;
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                if let Err(err) = evaluate(&mut runtime, &line) {
                    eprintln!("{}", err);
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => return Ok(()),
            Err(err) => return Err(err.into()),
        }
    }
}

fn evaluate(runtime: &mut Runtime, line: &str) -> Result<()> {
    const SOURCE: &str = "=stdin";
    runtime
        .execute(|gc, _| {
            if let Ok(proto) = mochi_lua::load(gc, format!("print({})", line), SOURCE) {
                Ok(proto)
            } else {
                mochi_lua::load(gc, line, SOURCE).map_err(Into::into)
            }
        })
        .map_err(Error::msg)
}

impl CompileCommand {
    fn run(self) -> Result<()> {
        let mut heap = GcHeap::new();
        heap.with(|gc, _| -> Result<()> {
            let proto = mochi_lua::load_file(gc, &self.filename)?;

            if self.list > 0 {
                let mut stdout = std::io::stdout().lock();
                self.dump_proto(&mut stdout, &proto)?;
            }
            if self.parse_only {
                return Ok(());
            }

            let mut writer = BufWriter::new(File::create(self.output)?);
            mochi_lua::binary_chunk::dump(&mut writer, &proto)?;
            Ok(())
        })
    }

    fn dump_proto(&self, w: &mut impl std::io::Write, proto: &LuaClosureProto) -> Result<()> {
        fn format_counter(word: &str, n: usize) -> String {
            format!("{} {}{}", n, word, if n == 1 { "" } else { "s" })
        }

        let source = proto.source.as_bstr();
        match &proto.lines_defined {
            LineRange::File => write!(w, "main <{}:0,0> ", source)?,
            LineRange::Lines(range) => write!(
                w,
                "function <{}:{},{}> ",
                source,
                range.start(),
                range.end()
            )?,
        };
        writeln!(
            w,
            "({})\n{}, {}, {}, {}",
            format_counter("instruction", proto.code.len()),
            format_counter("slot", proto.max_stack_size as usize),
            format_counter("upvalue", proto.upvalues.len()),
            format_counter("constant", proto.constants.len()),
            format_counter("function", proto.protos.len())
        )?;

        for (i, insn) in proto.code.iter().enumerate() {
            let opcode = insn.opcode();
            write!(w, "\t{}\t{:9}\t", i + 1, opcode)?;
            match opcode {
                OpCode::Return0 => w.write_all(b"\n")?,
                OpCode::LoadKX
                | OpCode::LoadFalse
                | OpCode::LFalseSkip
                | OpCode::LoadTrue
                | OpCode::Close
                | OpCode::Tbc
                | OpCode::Return1
                | OpCode::VarArgPrep => writeln!(w, "{}", insn.a())?,
                OpCode::Jmp => writeln!(w, "{}", insn.sj())?,
                OpCode::ExtraArg => writeln!(w, "{}", insn.ax())?,
                OpCode::Test => writeln!(w, "{} {}", insn.a(), insn.k() as u8)?,
                OpCode::Move
                | OpCode::LoadNil
                | OpCode::GetUpval
                | OpCode::SetUpval
                | OpCode::Unm
                | OpCode::BNot
                | OpCode::Not
                | OpCode::Len
                | OpCode::Concat => writeln!(w, "{} {}", insn.a(), insn.b())?,
                OpCode::LoadK
                | OpCode::Closure
                | OpCode::ForLoop
                | OpCode::ForPrep
                | OpCode::TForPrep
                | OpCode::TForLoop => writeln!(w, "{} {}", insn.a(), insn.bx())?,
                OpCode::LoadI | OpCode::LoadF => writeln!(w, "{} {}", insn.a(), insn.sbx())?,
                OpCode::TForCall | OpCode::VarArg => writeln!(w, "{} {}", insn.a(), insn.c())?,
                OpCode::Eq | OpCode::Lt | OpCode::Le | OpCode::EqK | OpCode::TestSet => {
                    writeln!(w, "{} {} {}", insn.a(), insn.b(), insn.k() as u8)?
                }
                OpCode::EqI | OpCode::LtI | OpCode::LeI | OpCode::GtI | OpCode::GeI => {
                    writeln!(w, "{} {} {}", insn.a(), insn.sb(), insn.k() as u8)?
                }
                OpCode::GetTabUp
                | OpCode::GetTable
                | OpCode::GetI
                | OpCode::GetField
                | OpCode::NewTable
                | OpCode::AddK
                | OpCode::SubK
                | OpCode::MulK
                | OpCode::ModK
                | OpCode::PowK
                | OpCode::DivK
                | OpCode::IDivK
                | OpCode::BAndK
                | OpCode::BOrK
                | OpCode::BXorK
                | OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Mod
                | OpCode::Pow
                | OpCode::Div
                | OpCode::IDiv
                | OpCode::BAnd
                | OpCode::BOr
                | OpCode::BXor
                | OpCode::Shl
                | OpCode::Shr
                | OpCode::MmBin
                | OpCode::Call
                | OpCode::SetList => writeln!(w, "{} {} {}", insn.a(), insn.b(), insn.c())?,
                OpCode::MmBinK => writeln!(
                    w,
                    "{} {} {} {}",
                    insn.a(),
                    insn.b(),
                    insn.c(),
                    insn.k() as u8
                )?,
                OpCode::MmBinI => writeln!(
                    w,
                    "{} {} {} {}",
                    insn.a(),
                    insn.sb(),
                    insn.c(),
                    insn.k() as u8
                )?,
                OpCode::AddI | OpCode::ShrI | OpCode::ShlI => {
                    writeln!(w, "{} {} {}", insn.a(), insn.b(), insn.sc())?
                }
                OpCode::SetTabUp
                | OpCode::SetTable
                | OpCode::SetI
                | OpCode::SetField
                | OpCode::Self_
                | OpCode::TailCall
                | OpCode::Return => writeln!(
                    w,
                    "{} {} {}{}",
                    insn.a(),
                    insn.b(),
                    insn.c(),
                    if insn.k() { "k" } else { "" }
                )?,
            };
        }

        if self.list > 1 {
            writeln!(w, "constants ({}):", proto.constants.len())?;
            for (i, constant) in proto.constants.iter().enumerate() {
                write!(w, "\t{}\t", i)?;
                w.write_all(match constant {
                    Value::Nil => b"N",
                    Value::Boolean(_) => b"B",
                    Value::Integer(_) => b"I",
                    Value::Number(_) => b"F",
                    Value::String(_) => b"S",
                    _ => unreachable!(),
                })?;
                match constant {
                    Value::Nil => w.write_all(b"\tnil\n")?,
                    Value::Boolean(b) => writeln!(w, "\t{}", b)?,
                    Value::Integer(i) => writeln!(w, "\t{}", i)?,
                    Value::Number(x) => writeln!(w, "\t{}", x)?,
                    Value::String(s) => writeln!(w, "\t\"{}\"", s.as_bstr())?,
                    _ => unreachable!(),
                };
            }

            writeln!(w, "upvalues ({}):", proto.upvalues.len())?;
            for (i, desc) in proto.upvalues.iter().enumerate() {
                write!(w, "\t{}\t", i)?;
                match desc {
                    UpvalueDescription::Register(i) => writeln!(w, "1\t{}", i.0)?,
                    UpvalueDescription::Upvalue(i) => writeln!(w, "0\t{}", i.0)?,
                }
            }
        }

        writeln!(w)?;

        for proto in proto.protos.iter() {
            self.dump_proto(w, proto)?;
        }

        Ok(())
    }
}
