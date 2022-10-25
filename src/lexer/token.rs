use crate::types::{Integer, LuaString, Number};
use bstr::ByteSlice;

#[derive(Clone, PartialEq, Debug)]
pub enum Token<'gc> {
    Len,               // #
    Mod,               // %
    BAnd,              // &
    LeftParen,         // (
    RightParen,        // )
    Mul,               // *
    Add,               // +
    Comma,             // ,
    Minus,             // -
    Dot,               // .
    Div,               // /
    Colon,             // :
    Semicolon,         // ;
    Lt,                // <
    Assign,            // =
    Gt,                // >
    LeftBracket,       // [
    RightBracket,      // ]
    Pow,               // ^
    LeftCurlyBracket,  // {
    BOr,               // |
    RightCurlyBracket, // }
    Tilde,             // ~

    And,
    Break,
    Else,
    Do,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    IDiv,        // //
    Concat,      // ..
    Dots,        // ...
    Eq,          // ==
    Ge,          // >=
    Le,          // <=
    Ne,          // ~=
    Shl,         // <<
    Shr,         // >>
    DoubleColon, // ::

    Float(Number),
    Integer(Integer),
    Name(LuaString<'gc>),
    String(LuaString<'gc>),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Len => f.write_str("#"),
            Self::Mod => f.write_str("%"),
            Self::BAnd => f.write_str("&"),
            Self::LeftParen => f.write_str("("),
            Self::RightParen => f.write_str(")"),
            Self::Mul => f.write_str("*"),
            Self::Add => f.write_str("+"),
            Self::Comma => f.write_str(","),
            Self::Minus => f.write_str("-"),
            Self::Dot => f.write_str("."),
            Self::Div => f.write_str("/"),
            Self::Colon => f.write_str(":"),
            Self::Semicolon => f.write_str(";"),
            Self::Lt => f.write_str("<"),
            Self::Assign => f.write_str("="),
            Self::Gt => f.write_str(">"),
            Self::LeftBracket => f.write_str("["),
            Self::RightBracket => f.write_str("]"),
            Self::Pow => f.write_str("^"),
            Self::LeftCurlyBracket => f.write_str("{"),
            Self::BOr => f.write_str("|"),
            Self::RightCurlyBracket => f.write_str("}"),
            Self::Tilde => f.write_str("~"),
            Self::And => f.write_str("&"),
            Self::Break => f.write_str("break"),
            Self::Else => f.write_str("else"),
            Self::Do => f.write_str("do"),
            Self::ElseIf => f.write_str("elseif"),
            Self::End => f.write_str("end"),
            Self::False => f.write_str("false"),
            Self::For => f.write_str("for"),
            Self::Function => f.write_str("function"),
            Self::Goto => f.write_str("goto"),
            Self::If => f.write_str("if"),
            Self::In => f.write_str("in"),
            Self::Local => f.write_str("local"),
            Self::Nil => f.write_str("nil"),
            Self::Not => f.write_str("not"),
            Self::Or => f.write_str("or"),
            Self::Repeat => f.write_str("repeat"),
            Self::Return => f.write_str("return"),
            Self::Then => f.write_str("then"),
            Self::True => f.write_str("true"),
            Self::Until => f.write_str("until"),
            Self::While => f.write_str("while"),
            Self::IDiv => f.write_str("//"),
            Self::Concat => f.write_str(".."),
            Self::Dots => f.write_str("..."),
            Self::Eq => f.write_str("=="),
            Self::Ge => f.write_str(">="),
            Self::Le => f.write_str("<="),
            Self::Ne => f.write_str("~="),
            Self::Shl => f.write_str("<<"),
            Self::Shr => f.write_str(">>"),
            Self::DoubleColon => f.write_str("::"),
            Self::Float(x) => write!(f, "{x}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Name(name) => write!(f, "{}", name.as_bstr()),
            Self::String(s) => write!(f, "{}", s.as_bstr()),
        }
    }
}

impl Token<'_> {
    pub fn from_reserved_word<T: AsRef<[u8]>>(word: T) -> Option<Self> {
        match word.as_ref() {
            b"and" => Some(Self::And),
            b"break" => Some(Self::Break),
            b"do" => Some(Self::Do),
            b"else" => Some(Self::Else),
            b"elseif" => Some(Self::ElseIf),
            b"end" => Some(Self::End),
            b"false" => Some(Self::False),
            b"for" => Some(Self::For),
            b"function" => Some(Self::Function),
            b"goto" => Some(Self::Goto),
            b"if" => Some(Self::If),
            b"in" => Some(Self::In),
            b"local" => Some(Self::Local),
            b"nil" => Some(Self::Nil),
            b"not" => Some(Self::Not),
            b"or" => Some(Self::Or),
            b"repeat" => Some(Self::Repeat),
            b"return" => Some(Self::Return),
            b"then" => Some(Self::Then),
            b"true" => Some(Self::True),
            b"until" => Some(Self::Until),
            b"while" => Some(Self::While),
            _ => None,
        }
    }
}
