use crate::types::{Integer, LuaString, Number};

#[derive(Debug, Clone)]
pub struct Chunk<'gc>(pub Block<'gc>);

#[derive(Debug, Clone)]
pub struct Block<'gc> {
    pub statements: Vec<Statement<'gc>>,
    pub return_statement: Option<ReturnStatement<'gc>>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement<'gc>(pub Vec<Expression<'gc>>);

#[derive(Debug, Clone)]
pub enum Statement<'gc> {
    If(IfStatement<'gc>),
    While(WhileStatement<'gc>),
    Do(Block<'gc>),
    For(ForStatement<'gc>),
    Repeat(RepeatStatement<'gc>),
    Function(FunctionStatement<'gc>),
    LocalFunction(FunctionStatement<'gc>),
    LocalVariable(LocalVariableStatement<'gc>),
    Label(LuaString<'gc>),
    Break,
    Goto(LuaString<'gc>),
    FunctionCall(FunctionCallStatement<'gc>),
    Assignment(AssignmentStatement<'gc>),
}

#[derive(Debug, Clone)]
pub struct IfStatement<'gc> {
    pub condition: Expression<'gc>,
    pub body: Block<'gc>,
    pub else_if_parts: Vec<(Expression<'gc>, Block<'gc>)>,
    pub else_part: Option<Block<'gc>>,
}

#[derive(Debug, Clone)]
pub struct WhileStatement<'gc> {
    pub condition: Expression<'gc>,
    pub body: Block<'gc>,
}

#[derive(Debug, Clone)]
pub enum ForStatement<'gc> {
    Numerical {
        control: LuaString<'gc>,
        initial_value: Box<Expression<'gc>>,
        limit: Box<Expression<'gc>>,
        step: Option<Box<Expression<'gc>>>,
        body: Block<'gc>,
    },
    Generic {
        variables: Vec<LuaString<'gc>>,
        expressions: Vec<Expression<'gc>>,
        body: Block<'gc>,
    },
}

#[derive(Debug, Clone)]
pub struct RepeatStatement<'gc> {
    pub body: Block<'gc>,
    pub condition: Expression<'gc>,
}

#[derive(Debug, Clone)]
pub struct FunctionStatement<'gc> {
    pub name: LuaString<'gc>,
    pub fields: Vec<LuaString<'gc>>,
    pub method: Option<LuaString<'gc>>,
    pub expression: FunctionExpression<'gc>,
}

#[derive(Debug, Clone)]
pub struct LocalVariableStatement<'gc> {
    pub variables: Vec<LocalVariable<'gc>>,
    pub values: Vec<Expression<'gc>>,
}

#[derive(Debug, Clone)]
pub struct LocalVariable<'gc> {
    pub name: LuaString<'gc>,
    pub attribute: Option<LuaString<'gc>>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallStatement<'gc>(pub SuffixedExpression<'gc>);

#[derive(Debug, Clone)]
pub struct AssignmentStatement<'gc> {
    pub lhs: Vec<Variable<'gc>>,
    pub rhs: Vec<Expression<'gc>>,
}

#[derive(Debug, Clone)]
pub enum Variable<'gc> {
    Name(LuaString<'gc>),
    TableIndex {
        table: SuffixedExpression<'gc>,
        index: Expression<'gc>,
    },
    Field {
        table: SuffixedExpression<'gc>,
        field: LuaString<'gc>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression<'gc> {
    Float(Number),
    Integer(Integer),
    String(LuaString<'gc>),
    Nil,
    Boolean(bool),
    VarArg,
    TableConstructor(TableConstructorExpression<'gc>),
    Function(FunctionExpression<'gc>),
    Suffixed(SuffixedExpression<'gc>),
    UnaryOp(UnaryOpExpression<'gc>),
    BinaryOp(BinaryOpExpression<'gc>),
}

#[derive(Debug, Clone)]
pub struct TableConstructorExpression<'gc>(pub Vec<TableField<'gc>>);

#[derive(Debug, Clone)]
pub enum TableField<'gc> {
    List(Expression<'gc>),
    Record {
        key: TableRecordKey<'gc>,
        value: Expression<'gc>,
    },
}

#[derive(Debug, Clone)]
pub enum TableRecordKey<'gc> {
    Name(LuaString<'gc>),
    Index(Expression<'gc>),
}

#[derive(Debug, Clone)]
pub struct FunctionExpression<'gc> {
    pub params: Vec<LuaString<'gc>>,
    pub is_vararg: bool,
    pub body: Block<'gc>,
}

#[derive(Debug, Clone)]
pub struct SuffixedExpression<'gc> {
    pub primary: Primary<'gc>,
    pub suffixes: Vec<Suffix<'gc>>,
}

#[derive(Debug, Clone)]
pub enum Primary<'gc> {
    Name(LuaString<'gc>),
    Expression(Box<Expression<'gc>>),
}

#[derive(Debug, Clone)]
pub enum Suffix<'gc> {
    Field(LuaString<'gc>),
    Index(Expression<'gc>),
    MethodCall {
        name: LuaString<'gc>,
        args: FunctionArguments<'gc>,
    },
    FunctionCall {
        args: FunctionArguments<'gc>,
    },
}

#[derive(Debug, Clone)]
pub enum FunctionArguments<'gc> {
    Expressions(Vec<Expression<'gc>>),
    TableConstructor(TableConstructorExpression<'gc>),
    String(LuaString<'gc>),
}

#[derive(Debug, Clone)]
pub struct UnaryOpExpression<'gc> {
    pub op: UnaryOp,
    pub inner: Box<Expression<'gc>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Unm,
    Not,
    Len,
    BNot,
}

#[derive(Debug, Clone)]
pub struct BinaryOpExpression<'gc> {
    pub op: BinaryOp,
    pub lhs: Box<Expression<'gc>>,
    pub rhs: Box<Expression<'gc>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Pow,
    Mod,
    BAnd,
    BXor,
    BOr,
    Shr,
    Shl,
    Concat,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}
