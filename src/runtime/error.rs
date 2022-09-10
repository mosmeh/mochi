use crate::types::{LineRange, TableError, Type, Value};
use std::{borrow::Cow, fmt::Display};

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

#[derive(Debug)]
pub enum TracebackFrame {
    Lua {
        source: String,
        lines_defined: LineRange,
    },
    Native,
}

impl Display for TracebackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lua {
                source,
                lines_defined,
            } => {
                let source = format_source(source);
                match &lines_defined {
                    LineRange::File => write!(f, "{}: in main chunk", source),
                    LineRange::Lines(range) => {
                        write!(f, "{}: in function <{}:{}>", source, source, range.start())
                    }
                }
            }
            Self::Native => f.write_str("[C]: in function"),
        }
    }
}

fn format_source(source: &str) -> Cow<str> {
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
