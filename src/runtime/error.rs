use crate::types::{TableError, TracebackFrame, Type, Value};
use std::{borrow::Cow, fmt::Display, sync::Arc};

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
                writeln!(f, "\t{frame}")?;
            }
            write!(f, "\t{last}")?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("attempt to {operation} a {ty} value")]
    TypeError { operation: Operation, ty: Type },

    #[error("bad argument #{nth} ({message})")]
    ArgumentError { nth: usize, message: &'static str },

    #[error("bad argument #{nth} ({expected_type} expected, got {got})",
        got = got_type.unwrap_or("no value")
    )]
    ArgumentTypeError {
        nth: usize,
        expected_type: &'static str,
        got_type: Option<&'static str>,
    },

    #[error("bad 'for' {what} (number expected, got {got_type})")]
    ForError {
        what: &'static str,
        got_type: &'static str,
    },

    #[error(transparent)]
    Table(#[from] TableError),

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Other(String),

    #[error(transparent)]
    External(Arc<dyn std::error::Error + Send + Sync>),
}

impl Clone for ErrorKind {
    fn clone(&self) -> Self {
        match self {
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
            Self::ForError { what, got_type } => Self::ForError { what, got_type },
            Self::Table(e) => Self::Table(e.clone()),
            Self::Io(e) => Self::Io(std::io::Error::new(e.kind(), e.to_string())),
            Self::Other(s) => Self::Other(s.clone()),
            Self::External(err) => Self::External(err.clone()),
        }
    }
}

impl ErrorKind {
    pub fn other<'a, S: Into<Cow<'a, str>>>(s: S) -> Self {
        Self::Other(s.into().into_owned())
    }

    pub fn from_error_object(error_object: Value) -> Self {
        let msg = if let Some(s) = error_object.to_string() {
            String::from_utf8_lossy(&s).to_string()
        } else {
            format!("(error object is a {} value)", error_object.ty().name())
        };
        Self::Other(msg)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Index,
    Call,
    Concatenate,
    Arithmetic,
    BitwiseOp,
    Compare,
    Length,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Index => f.write_str("index"),
            Self::Call => f.write_str("call"),
            Self::Concatenate => f.write_str("concatenate"),
            Self::Arithmetic => f.write_str("perform arithmetic on"),
            Self::BitwiseOp => f.write_str("perform bitwise operation on"),
            Self::Compare => f.write_str("compare"),
            Self::Length => f.write_str("get length of"),
        }
    }
}
