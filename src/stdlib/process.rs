use super::file::{self, FileError};
use crate::{
    gc::GcContext,
    runtime::{Action, ErrorKind},
    types::{Integer, Value},
};
use bstr::B;
use std::{
    ffi::OsStr,
    io::{self, Read, Write},
    process::{Child, Command, ExitStatus},
};

pub fn system<S: AsRef<OsStr>>(line: S) -> Command {
    let mut command = {
        #[cfg(windows)]
        {
            let mut command = Command::new("cmd");
            command.arg("/C");
            command
        }
        #[cfg(not(windows))]
        {
            let mut command = Command::new("/bin/sh");
            command.arg("-c");
            #[cfg(unix)]
            {
                use std::os::unix::process::CommandExt;
                command.arg0("sh");
            }
            command
        }
    };
    command.arg(line);
    command
}

pub struct Process(Child);

impl From<Child> for Process {
    fn from(inner: Child) -> Self {
        Self(inner)
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        let _ = self.0.wait();
    }
}

impl Read for Process {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match &mut self.0.stdout {
            Some(stdout) => stdout.read(buf),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }
}

impl Write for Process {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match &mut self.0.stdin {
            Some(stdin) => stdin.write(buf),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match &mut self.0.stdin {
            Some(stdin) => stdin.flush(),
            None => Err(io::Error::from(io::ErrorKind::Unsupported)),
        }
    }
}

impl Process {
    pub fn close(&mut self) -> io::Result<ExitStatus> {
        self.0.wait()
    }
}

pub fn translate_and_return_error<F>(gc: &GcContext, f: F) -> Result<Action, ErrorKind>
where
    F: FnOnce() -> Result<Option<ExitStatus>, FileError>,
{
    match f() {
        Ok(Some(status)) => {
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                if let Some(signal) = status.signal() {
                    return Ok(Action::Return(vec![
                        Value::Nil,
                        gc.allocate_string(B("signal")).into(),
                        (signal as Integer).into(),
                    ]));
                }
            }
            Ok(Action::Return(vec![
                if status.success() {
                    true.into()
                } else {
                    Value::Nil
                },
                gc.allocate_string(B("exit")).into(),
                status
                    .code()
                    .map(|code| (code as Integer).into())
                    .unwrap_or_default(),
            ]))
        }
        Ok(None) => Ok(Action::Return(vec![true.into()])),
        Err(err) => file::translate_and_return_error(gc, || Err(err)),
    }
}
