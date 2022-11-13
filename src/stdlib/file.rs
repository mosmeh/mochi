use super::process::Process;
use crate::{
    gc::GcContext,
    runtime::ErrorKind,
    types::{Integer, Value},
};
use std::{
    fs::File,
    io::{
        self, BufRead, BufReader, BufWriter, LineWriter, Read, Seek, SeekFrom, Stderr, Stdin,
        Stdout, Write,
    },
    process::ExitStatus,
};

#[derive(Debug, thiserror::Error)]
pub enum FileError {
    #[error("attempt to use a closed file")]
    Closed,

    #[error("default {kind} file is closed")]
    DefaultFileClosed { kind: &'static str },

    #[error("cannot close standard file")]
    CannotCloseStandardFile,

    #[error("invalid offset")]
    InvalidOffset,

    #[error(transparent)]
    Runtime(#[from] ErrorKind),

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    BstrUtf8(#[from] bstr::Utf8Error),
}

#[derive(Default)]
pub struct FileHandle(Option<LuaFile>);

impl From<LuaFile> for FileHandle {
    fn from(inner: LuaFile) -> Self {
        Self(Some(inner))
    }
}

impl FileHandle {
    pub fn is_open(&self) -> bool {
        self.0.is_some()
    }

    pub fn get_mut(&mut self) -> Option<&mut LuaFile> {
        self.0.as_mut()
    }

    pub fn replace_with<F>(&mut self, f: F) -> Result<(), FileError>
    where
        F: FnOnce(File) -> LuaFile,
    {
        match &mut self.0 {
            Some(
                LuaFile::Stdin(_) | LuaFile::Stdout(_) | LuaFile::Stderr(_) | LuaFile::Process(_),
            ) => Ok(()),
            None => Err(FileError::Closed),
            inner => match inner.take().unwrap().into_inner() {
                Ok(file) => {
                    *inner = Some(f(file));
                    Ok(())
                }
                Err((err, original)) => {
                    *inner = Some(original);
                    Err(err.into())
                }
            },
        }
    }

    pub fn close(&mut self) -> Result<Option<ExitStatus>, FileError> {
        match &mut self.0 {
            Some(LuaFile::Stdin(_) | LuaFile::Stdout(_) | LuaFile::Stderr(_)) => {
                Err(FileError::CannotCloseStandardFile)
            }
            Some(LuaFile::Process(process)) => {
                let status = process.close()?;
                Ok(Some(status))
            }
            None => Err(FileError::Closed),
            inner => {
                inner.take();
                Ok(None)
            }
        }
    }
}

pub enum LuaFile {
    NonBuffered(File),
    FullyBuffered(Box<FullyBufferedFile>),
    LineBuffered(Box<LineBufferedFile>),
    Stdin(Stdin),
    Stdout(Stdout),
    Stderr(Stderr),
    Process(Box<Process>),
}

impl From<FullyBufferedFile> for LuaFile {
    fn from(inner: FullyBufferedFile) -> Self {
        Self::FullyBuffered(Box::new(inner))
    }
}

impl From<Process> for LuaFile {
    fn from(inner: Process) -> Self {
        Self::Process(Box::new(inner))
    }
}

impl Read for LuaFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self.reader() {
            Some(reader) => reader.read(buf),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }
}

impl Write for LuaFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.writer() {
            Some(writer) => writer.write(buf),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self.writer() {
            Some(writer) => writer.flush(),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }
}

impl Seek for LuaFile {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        match self.seeker() {
            Some(seeker) => seeker.seek(pos),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }
}

impl LuaFile {
    pub fn stdin() -> Self {
        Self::Stdin(io::stdin())
    }

    pub fn stdout() -> Self {
        Self::Stdout(io::stdout())
    }

    pub fn stderr() -> Self {
        Self::Stderr(io::stderr())
    }

    pub fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> io::Result<usize> {
        fn naive_read_until<R: Read>(
            reader: &mut R,
            byte: u8,
            buf: &mut Vec<u8>,
        ) -> io::Result<usize> {
            let mut read = 0;
            let mut b = [0; 1];
            loop {
                match reader.read(&mut b) {
                    Ok(0) => break,
                    Ok(1) => {
                        let ch = b[0];
                        buf.push(ch);
                        read += 1;
                        if ch == byte {
                            break;
                        }
                    }
                    Ok(_) => unreachable!(),
                    Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                    Err(e) => return Err(e),
                }
            }
            Ok(read)
        }

        match self {
            Self::NonBuffered(inner) => naive_read_until(inner, byte, buf),
            Self::FullyBuffered(inner) => inner.read_until(byte, buf),
            Self::LineBuffered(inner) => inner.read_until(byte, buf),
            Self::Stdin(inner) => inner.lock().read_until(byte, buf),
            Self::Process(inner) => naive_read_until(inner, byte, buf),
            Self::Stdout(_) | Self::Stderr(_) => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }

    fn reader(&mut self) -> Option<&mut dyn Read> {
        match self {
            Self::NonBuffered(inner) => Some(inner),
            Self::FullyBuffered(inner) => Some(inner),
            Self::LineBuffered(inner) => Some(inner),
            Self::Stdin(inner) => Some(inner),
            Self::Process(inner) => Some(inner),
            Self::Stdout(_) | Self::Stderr(_) => None,
        }
    }

    fn writer(&mut self) -> Option<&mut dyn Write> {
        match self {
            Self::NonBuffered(inner) => Some(inner),
            Self::FullyBuffered(inner) => Some(inner),
            Self::LineBuffered(inner) => Some(inner),
            Self::Stdout(inner) => Some(inner),
            Self::Stderr(inner) => Some(inner),
            Self::Process(inner) => Some(inner),
            Self::Stdin(_) => None,
        }
    }

    fn seeker(&mut self) -> Option<&mut dyn Seek> {
        match self {
            Self::NonBuffered(inner) => Some(inner),
            Self::FullyBuffered(inner) => Some(inner),
            Self::LineBuffered(inner) => Some(inner),
            Self::Stdin(_) | Self::Stdout(_) | Self::Stderr(_) | Self::Process(_) => None,
        }
    }

    fn into_inner(self) -> Result<File, (io::Error, Self)> {
        match self {
            Self::NonBuffered(inner) => Ok(inner),
            Self::FullyBuffered(inner) => match inner.0.into_inner() {
                Ok(inner) => Ok(inner.0.into_inner()),
                Err(err) => {
                    let (err, inner) = err.into_parts();
                    Err((err, Self::FullyBuffered(FullyBufferedFile(inner).into())))
                }
            },
            Self::LineBuffered(inner) => match inner.0.into_inner() {
                Ok(inner) => Ok(inner.0.into_inner()),
                Err(err) => {
                    let (err, inner) = err.into_parts();
                    Err((err, Self::LineBuffered(LineBufferedFile(inner).into())))
                }
            },
            Self::Stdin(_) | Self::Stdout(_) | Self::Stderr(_) | Self::Process(_) => {
                Err((io::Error::from(io::ErrorKind::Unsupported), self))
            }
        }
    }
}

pub struct FullyBufferedFile(BufWriter<InnerReader>);

impl Read for FullyBufferedFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.get_mut().read(buf)
    }
}

impl BufRead for FullyBufferedFile {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.0.get_mut().fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.0.get_mut().consume(amt)
    }
}

impl Write for FullyBufferedFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl Seek for FullyBufferedFile {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        self.0.seek(pos)
    }
}

impl FullyBufferedFile {
    pub fn new(file: File) -> Self {
        let reader = InnerReader(BufReader::new(file));
        Self(BufWriter::new(reader))
    }

    pub fn with_capacity(capacity: usize, file: File) -> Self {
        let reader = InnerReader(BufReader::with_capacity(capacity, file));
        Self(BufWriter::with_capacity(capacity, reader))
    }
}

pub struct LineBufferedFile(LineWriter<InnerReader>);

impl Read for LineBufferedFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.get_mut().read(buf)
    }
}

impl BufRead for LineBufferedFile {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.0.get_mut().fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.0.get_mut().consume(amt)
    }
}

impl Write for LineBufferedFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl Seek for LineBufferedFile {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        self.0.get_mut().seek(pos)
    }
}

impl LineBufferedFile {
    pub fn new(file: File) -> Self {
        let reader = InnerReader(BufReader::new(file));
        Self(LineWriter::new(reader))
    }

    pub fn with_capacity(capacity: usize, file: File) -> Self {
        let reader = InnerReader(BufReader::with_capacity(capacity, file));
        Self(LineWriter::with_capacity(capacity, reader))
    }
}

struct InnerReader(BufReader<File>);

impl Read for InnerReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

impl BufRead for InnerReader {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.0.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
}

impl Write for InnerReader {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.get_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.get_mut().flush()
    }
}

impl Seek for InnerReader {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        self.0.seek(pos)
    }
}

pub fn translate_and_raise_error<'gc, F>(f: F) -> Result<Vec<Value<'gc>>, ErrorKind>
where
    F: FnOnce() -> Result<Vec<Value<'gc>>, FileError>,
{
    match f() {
        Ok(values) => Ok(values),
        Err(FileError::Runtime(kind)) => Err(kind),
        Err(err) => Err(ErrorKind::Other(err.to_string())),
    }
}

pub fn translate_and_return_error<'gc, F>(
    gc: &'gc GcContext,
    f: F,
) -> Result<Vec<Value<'gc>>, ErrorKind>
where
    F: FnOnce() -> Result<Vec<Value<'gc>>, FileError>,
{
    match f() {
        Ok(values) => Ok(values),
        Err(FileError::Runtime(kind)) => Err(kind),
        Err(FileError::Io(err)) => Ok(vec![
            Value::Nil,
            gc.allocate_string(err.to_string().into_bytes()).into(),
            err.raw_os_error()
                .map(|errno| (errno as Integer).into())
                .unwrap_or_default(),
        ]),
        Err(err @ (FileError::Closed | FileError::DefaultFileClosed { .. })) => {
            Err(ErrorKind::Other(err.to_string()))
        }
        Err(err) => Ok(vec![
            Value::Nil,
            gc.allocate_string(err.to_string().into_bytes()).into(),
        ]),
    }
}
