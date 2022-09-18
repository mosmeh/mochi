use crate::types::{TableError, TracebackFrame, Type, Value};
use std::fmt::Display;

#[derive(Debug, thiserror::Error)]
pub struct RuntimeError {
    #[source]
    pub kind: ErrorKind,

    pub traceback: Vec<TracebackFrame>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{}\nstack traceback:", self.kind,)?;
        if let Some((last, frames)) = self.traceback.split_last() {
            for frame in frames {
                writeln!(f, "\t{}", frame)?;
            }
            write!(f, "\t{}", last)?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("{0}")]
    ExplicitError(String),

    #[error("attempt to {operation} a {ty} value")]
    TypeError { operation: Operation, ty: Type },

    #[error("bad argument #{nth} ({message})", nth = nth + 1)]
    ArgumentError { nth: usize, message: &'static str },

    #[error("bad argument #{nth} ({expected_type} expected, got {got})",
        nth = nth + 1,
        got = got_type.unwrap_or("no value")
    )]
    ArgumentTypeError {
        nth: usize,
        expected_type: &'static str,
        got_type: Option<&'static str>,
    },

    #[error(transparent)]
    Table(#[from] TableError),

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl Clone for ErrorKind {
    fn clone(&self) -> Self {
        match self {
            Self::ExplicitError(s) => Self::ExplicitError(s.clone()),
            Self::TypeError { operation, ty } => Self::TypeError {
                operation: *operation,
                ty: *ty,
            },
            Self::ArgumentError { nth, message } => Self::ArgumentError { nth: *nth, message },
            Self::ArgumentTypeError {
                nth,
                expected_type,
                got_type,
            } => Self::ArgumentTypeError {
                nth: *nth,
                expected_type,
                got_type: *got_type,
            },
            Self::Table(e) => Self::Table(e.clone()),
            Self::Io(e) => Self::Io(std::io::Error::new(e.kind(), e.to_string())),
        }
    }
}

impl ErrorKind {
    pub fn from_error_object(error_object: Value) -> Self {
        let msg = if let Some(s) = error_object.to_string() {
            String::from_utf8_lossy(&s).to_string()
        } else {
            format!("(error object is a {} value)", error_object.ty().name())
        };
        Self::ExplicitError(msg)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Index,
    Call,
    Concatenate,
    Arithmetic,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Index => f.write_str("index"),
            Self::Call => f.write_str("call"),
            Self::Concatenate => f.write_str("concatenate"),
            Self::Arithmetic => f.write_str("perform arithmetic"),
        }
    }
}
