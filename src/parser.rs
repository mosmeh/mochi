pub mod ast;

pub use crate::lexer::LexerError;

use crate::{
    gc::GcContext,
    lexer::{Lexer, Token},
    types::LuaString,
};
use ast::{
    AssignmentStatement, BinaryOp, BinaryOpExpression, Block, Chunk, Expression, ForStatement,
    FunctionArguments, FunctionCallStatement, FunctionExpression, FunctionStatement, IfStatement,
    LocalVariable, LocalVariableStatement, Primary, RepeatStatement, ReturnStatement, Statement,
    Suffix, SuffixedExpression, TableConstructorExpression, TableField, TableRecordKey, UnaryOp,
    UnaryOpExpression, Variable, WhileStatement,
};
use std::{borrow::Cow, io::Read};

#[derive(Debug, thiserror::Error)]
pub struct ParseError {
    #[source]
    pub kind: ErrorKind,

    pub source: String,
    pub lineno: usize,
    pub next_token: Option<String>,

    pub incomplete_input: bool,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.source, self.lineno, self.kind)?;
        if let Some(token) = &self.next_token {
            write!(f, " near {token}")?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("syntax error")]
    SyntaxError,

    #[error("{expected} expected")]
    UnexpectedToken { expected: String },

    #[error("unexpected symbol")]
    UnexpectedSymbol,

    #[error(transparent)]
    Lexer(#[from] LexerError),
}

impl ErrorKind {
    fn unexpected_token<'a, I: Into<Cow<'a, str>>>(expected: I) -> Self {
        Self::UnexpectedToken {
            expected: expected.into().to_string(),
        }
    }
}

fn stringify_token_or_eof<S: ToString>(token: &Option<S>) -> String {
    if let Some(token) = token {
        format!("'{}'", token.to_string())
    } else {
        "<eof>".to_owned()
    }
}

pub fn parse<R: Read, S: AsRef<str>>(
    gc: &GcContext,
    source: S,
    reader: R,
) -> Result<Chunk, ParseError> {
    let mut parser = Parser::new(gc, reader);
    match parser.parse_chunk() {
        Ok(chunk) => Ok(chunk),
        Err(kind) => {
            let source = crate::chunk_id_from_source(source.as_ref()).to_string();
            let lineno = parser.lexer.lineno();
            let (next_token, incomplete_input) = if let Ok(t) = parser.lexer.peek() {
                (Some(stringify_token_or_eof(&t)), t.is_none())
            } else {
                (None, false)
            };
            Err(ParseError {
                kind,
                source,
                lineno,
                next_token,
                incomplete_input,
            })
        }
    }
}

struct Parser<'gc, R: Read> {
    lexer: Lexer<'gc, R>,
}

impl<'gc, R: Read> Parser<'gc, R> {
    fn new(gc: &'gc GcContext, reader: R) -> Self {
        Self {
            lexer: Lexer::new(gc, reader),
        }
    }

    fn parse_chunk(&mut self) -> Result<Chunk<'gc>, ErrorKind> {
        let block = self.parse_block()?;
        self.expect(None)?;
        Ok(Chunk(block))
    }

    fn parse_block(&mut self) -> Result<Block<'gc>, ErrorKind> {
        let mut statements = Vec::new();
        loop {
            match self.lexer.peek()? {
                Some(Token::Semicolon) => {
                    self.lexer.consume()?;
                }
                None | Some(Token::Else | Token::ElseIf | Token::End | Token::Until) => {
                    return Ok(Block {
                        statements,
                        return_statement: None,
                    })
                }
                Some(Token::Return) => {
                    return Ok(Block {
                        statements,
                        return_statement: Some(self.parse_return_statement()?),
                    })
                }
                _ => statements.push(self.parse_statement()?),
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement<'gc>, ErrorKind> {
        self.expect(Token::Return)?;
        let list = match self.lexer.peek()? {
            None
            | Some(Token::Else | Token::ElseIf | Token::End | Token::Until | Token::Semicolon) => {
                Vec::new()
            }
            _ => self.parse_expr_list()?,
        };
        self.lexer.consume_if_eq(Token::Semicolon)?;
        Ok(ReturnStatement(list))
    }

    fn parse_statement(&mut self) -> Result<Statement<'gc>, ErrorKind> {
        match self.lexer.peek()?.unwrap() {
            Token::If => Ok(Statement::If(self.parse_if_statement()?)),
            Token::While => Ok(Statement::While(self.parse_while_statement()?)),
            Token::Do => Ok(Statement::Do(self.parse_do_statement()?)),
            Token::For => Ok(Statement::For(self.parse_for_statement()?)),
            Token::Repeat => Ok(Statement::Repeat(self.parse_repeat_statement()?)),
            Token::Function => Ok(Statement::Function(self.parse_func_statement()?)),
            Token::Local => {
                self.expect(Token::Local)?;
                Ok(if let Some(Token::Function) = self.lexer.peek()? {
                    Statement::LocalFunction(self.parse_func_statement()?)
                } else {
                    Statement::LocalVariable(self.parse_local_variable_statement()?)
                })
            }
            Token::DoubleColon => Ok(Statement::Label(self.parse_label()?)),
            Token::Break => {
                self.lexer.consume()?;
                Ok(Statement::Break)
            }
            Token::Goto => Ok(Statement::Goto(self.parse_goto_statement()?)),
            Token::Return => unreachable!(),
            _ => self.parse_expr_statement(),
        }
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement<'gc>, ErrorKind> {
        self.expect(Token::If)?;
        let condition = self.parse_expr()?;
        self.expect(Token::Then)?;
        let body = self.parse_block()?;
        let mut else_if_parts = Vec::new();
        while self.lexer.consume_if_eq(Token::ElseIf)? {
            let condition = self.parse_expr()?;
            self.expect(Token::Then)?;
            let body = self.parse_block()?;
            else_if_parts.push((condition, body));
        }
        let else_part = self
            .lexer
            .consume_if_eq(Token::Else)?
            .then(|| self.parse_block())
            .transpose()?;
        self.expect(Token::End)?;
        Ok(IfStatement {
            condition,
            body,
            else_if_parts,
            else_part,
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement<'gc>, ErrorKind> {
        self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        self.expect(Token::Do)?;
        let body = self.parse_block()?;
        self.expect(Token::End)?;
        Ok(WhileStatement { condition, body })
    }

    fn parse_do_statement(&mut self) -> Result<Block<'gc>, ErrorKind> {
        self.expect(Token::Do)?;
        let body = self.parse_block()?;
        self.expect(Token::End)?;
        Ok(body)
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement<'gc>, ErrorKind> {
        self.expect(Token::For)?;
        let first_variable = self.expect_name()?;
        match self.lexer.peek()? {
            Some(Token::Assign) => {
                self.lexer.consume()?;
                let initial_value = self.parse_expr()?;
                self.expect(Token::Comma)?;
                let limit = self.parse_expr()?;
                let step = if self.lexer.consume_if_eq(Token::Comma)? {
                    Some(self.parse_expr()?.into())
                } else {
                    None
                };
                self.expect(Token::Do)?;
                let body = self.parse_block()?;
                self.expect(Token::End)?;
                Ok(ForStatement::Numerical {
                    control: first_variable,
                    initial_value: initial_value.into(),
                    limit: limit.into(),
                    step,
                    body,
                })
            }
            Some(Token::Comma | Token::In) => {
                let mut variables = vec![first_variable];
                while self.lexer.consume_if_eq(Token::Comma)? {
                    variables.push(self.expect_name()?);
                }
                self.expect(Token::In)?;
                let expressions = self.parse_expr_list()?;
                self.expect(Token::Do)?;
                let body = self.parse_block()?;
                self.expect(Token::End)?;
                Ok(ForStatement::Generic {
                    variables,
                    expressions,
                    body,
                })
            }
            _ => Err(ErrorKind::unexpected_token("'=' or 'in'")),
        }
    }

    fn parse_repeat_statement(&mut self) -> Result<RepeatStatement<'gc>, ErrorKind> {
        self.expect(Token::Repeat)?;
        let body = self.parse_block()?;
        self.expect(Token::Until)?;
        let condition = self.parse_expr()?;
        Ok(RepeatStatement { body, condition })
    }

    fn parse_func_statement(&mut self) -> Result<FunctionStatement<'gc>, ErrorKind> {
        self.expect(Token::Function)?;
        let name = self.expect_name()?;
        let mut fields = Vec::new();
        while self.lexer.consume_if_eq(Token::Dot)? {
            fields.push(self.expect_name()?);
        }
        let method = if self.lexer.consume_if_eq(Token::Colon)? {
            Some(self.expect_name()?)
        } else {
            None
        };
        self.expect(Token::LeftParen)?;

        let mut params = Vec::new();
        let mut is_vararg = false;
        if !self.lexer.consume_if_eq(Token::RightParen)? {
            loop {
                match self.lexer.consume()? {
                    Some(Token::Name(name)) => params.push(name),
                    Some(Token::Dots) => {
                        is_vararg = true;
                        break;
                    }
                    _ => return Err(ErrorKind::unexpected_token("<name> or '...'")),
                }
                if !self.lexer.consume_if_eq(Token::Comma)? {
                    break;
                }
            }
            self.expect(Token::RightParen)?;
        }

        let body = self.parse_block()?;
        self.expect(Token::End)?;

        Ok(FunctionStatement {
            name,
            fields,
            method,
            expression: FunctionExpression {
                params,
                is_vararg,
                body,
            },
        })
    }

    fn parse_local_variable_statement(&mut self) -> Result<LocalVariableStatement<'gc>, ErrorKind> {
        let mut variables = Vec::new();
        loop {
            let name = self.expect_name()?;
            let attribute = if self.lexer.consume_if_eq(Token::Lt)? {
                let attr = self.expect_name()?;
                self.expect(Token::Gt)?;
                Some(attr)
            } else {
                None
            };
            variables.push(LocalVariable { name, attribute });
            if !self.lexer.consume_if_eq(Token::Comma)? {
                break;
            }
        }
        let values = if self.lexer.consume_if_eq(Token::Assign)? {
            self.parse_expr_list()?
        } else {
            Vec::new()
        };
        Ok(LocalVariableStatement { variables, values })
    }

    fn parse_label(&mut self) -> Result<LuaString<'gc>, ErrorKind> {
        self.expect(Token::DoubleColon)?;
        let label = self.expect_name()?;
        self.expect(Token::DoubleColon)?;
        Ok(label)
    }

    fn parse_goto_statement(&mut self) -> Result<LuaString<'gc>, ErrorKind> {
        self.expect(Token::Goto)?;
        self.expect_name()
    }

    fn parse_expr_statement(&mut self) -> Result<Statement<'gc>, ErrorKind> {
        fn suffixed_to_variable(mut suffixed: SuffixedExpression) -> Result<Variable, ErrorKind> {
            match suffixed.suffixes.pop() {
                None => match suffixed.primary {
                    Primary::Name(name) => Ok(Variable::Name(name)),
                    Primary::Expression(_) => Err(ErrorKind::SyntaxError),
                },
                Some(Suffix::Field(field)) => Ok(Variable::Field {
                    table: suffixed,
                    field,
                }),
                Some(Suffix::Index(index)) => Ok(Variable::TableIndex {
                    table: suffixed,
                    index,
                }),
                Some(Suffix::FunctionCall { .. } | Suffix::MethodCall { .. }) => {
                    Err(ErrorKind::SyntaxError)
                }
            }
        }

        let suffixed = self.parse_suffixed_expr()?;

        if let Some(Token::Assign | Token::Comma) = self.lexer.peek()? {
            let mut lhs = vec![suffixed_to_variable(suffixed)?];
            while self.lexer.consume_if_eq(Token::Comma)? {
                let suffixed = self.parse_suffixed_expr()?;
                lhs.push(suffixed_to_variable(suffixed)?);
            }
            self.expect(Token::Assign)?;
            let rhs = self.parse_expr_list()?;
            let statement = AssignmentStatement { lhs, rhs };
            return Ok(Statement::Assignment(statement));
        }

        if matches!(
            suffixed.suffixes.last(),
            Some(Suffix::MethodCall { .. } | Suffix::FunctionCall { .. })
        ) {
            Ok(Statement::FunctionCall(FunctionCallStatement(suffixed)))
        } else {
            Err(ErrorKind::SyntaxError)
        }
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Expression<'gc>>, ErrorKind> {
        let mut list = vec![self.parse_expr()?];
        while self.lexer.consume_if_eq(Token::Comma)? {
            list.push(self.parse_expr()?);
        }
        Ok(list)
    }

    fn parse_expr(&mut self) -> Result<Expression<'gc>, ErrorKind> {
        self.parse_sub_expr(0)
    }

    fn parse_sub_expr(&mut self, min_priority: usize) -> Result<Expression<'gc>, ErrorKind> {
        const UNARY_PRIORITY: usize = 12;
        fn binary_priority(op: BinaryOp) -> (usize, usize) {
            match op {
                BinaryOp::Add | BinaryOp::Sub => (10, 10),
                BinaryOp::Mul | BinaryOp::Mod => (11, 11),
                BinaryOp::Pow => (14, 13),
                BinaryOp::Div | BinaryOp::IDiv => (11, 11),
                BinaryOp::BAnd => (6, 6),
                BinaryOp::BOr => (4, 4),
                BinaryOp::BXor => (5, 5),
                BinaryOp::Shl | BinaryOp::Shr => (7, 7),
                BinaryOp::Concat => (9, 8),
                BinaryOp::Eq
                | BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Ne
                | BinaryOp::Gt
                | BinaryOp::Ge => (3, 3),
                BinaryOp::And => (2, 2),
                BinaryOp::Or => (1, 1),
            }
        }

        let unary_op = match self.lexer.peek()? {
            Some(Token::Not) => Some(UnaryOp::Not),
            Some(Token::Minus) => Some(UnaryOp::Unm),
            Some(Token::Tilde) => Some(UnaryOp::BNot),
            Some(Token::Len) => Some(UnaryOp::Len),
            _ => None,
        };
        let mut expr = if let Some(op) = unary_op {
            self.lexer.consume()?;
            let expr = UnaryOpExpression {
                op,
                inner: self.parse_sub_expr(UNARY_PRIORITY)?.into(),
            };
            Expression::UnaryOp(expr)
        } else {
            match self.lexer.peek()?.cloned() {
                Some(Token::Float(f)) => {
                    self.lexer.consume()?;
                    Expression::Float(f)
                }
                Some(Token::Integer(i)) => {
                    self.lexer.consume()?;
                    Expression::Integer(i)
                }
                Some(Token::String(s)) => {
                    self.lexer.consume()?;
                    Expression::String(s)
                }
                Some(Token::Nil) => {
                    self.lexer.consume()?;
                    Expression::Nil
                }
                Some(Token::True) => {
                    self.lexer.consume()?;
                    Expression::Boolean(true)
                }
                Some(Token::False) => {
                    self.lexer.consume()?;
                    Expression::Boolean(false)
                }
                Some(Token::Dots) => {
                    self.lexer.consume()?;
                    Expression::VarArg
                }
                Some(Token::LeftCurlyBracket) => {
                    Expression::TableConstructor(self.parse_table_constructor()?)
                }
                Some(Token::Function) => Expression::Function(self.parse_func_expr()?),
                _ => Expression::Suffixed(self.parse_suffixed_expr()?),
            }
        };

        loop {
            let op = match self.lexer.peek()? {
                Some(Token::Add) => BinaryOp::Add,
                Some(Token::Minus) => BinaryOp::Sub,
                Some(Token::Mul) => BinaryOp::Mul,
                Some(Token::Mod) => BinaryOp::Mod,
                Some(Token::Pow) => BinaryOp::Pow,
                Some(Token::Div) => BinaryOp::Div,
                Some(Token::IDiv) => BinaryOp::IDiv,
                Some(Token::BAnd) => BinaryOp::BAnd,
                Some(Token::BOr) => BinaryOp::BOr,
                Some(Token::Tilde) => BinaryOp::BXor,
                Some(Token::Shl) => BinaryOp::Shl,
                Some(Token::Shr) => BinaryOp::Shr,
                Some(Token::Concat) => BinaryOp::Concat,
                Some(Token::Ne) => BinaryOp::Ne,
                Some(Token::Eq) => BinaryOp::Eq,
                Some(Token::Lt) => BinaryOp::Lt,
                Some(Token::Le) => BinaryOp::Le,
                Some(Token::Gt) => BinaryOp::Gt,
                Some(Token::Ge) => BinaryOp::Ge,
                Some(Token::And) => BinaryOp::And,
                Some(Token::Or) => BinaryOp::Or,
                _ => break,
            };
            let (left_priority, right_priority) = binary_priority(op);
            if left_priority <= min_priority {
                break;
            }
            self.lexer.consume()?;
            let rhs = self.parse_sub_expr(right_priority)?;
            expr = Expression::BinaryOp(BinaryOpExpression {
                op,
                lhs: expr.into(),
                rhs: rhs.into(),
            })
        }

        Ok(expr)
    }

    fn parse_func_expr(&mut self) -> Result<FunctionExpression<'gc>, ErrorKind> {
        self.expect(Token::Function)?;
        self.expect(Token::LeftParen)?;

        let mut params = Vec::new();
        let mut is_vararg = false;
        if !self.lexer.consume_if_eq(Token::RightParen)? {
            loop {
                match self.lexer.consume()? {
                    Some(Token::Name(name)) => params.push(name),
                    Some(Token::Dots) => {
                        is_vararg = true;
                        break;
                    }
                    _ => return Err(ErrorKind::unexpected_token("<name> or '...'")),
                }
                if !self.lexer.consume_if_eq(Token::Comma)? {
                    break;
                }
            }
            self.expect(Token::RightParen)?;
        }

        let body = self.parse_block()?;
        self.expect(Token::End)?;

        Ok(FunctionExpression {
            params,
            is_vararg,
            body,
        })
    }

    fn parse_func_args(&mut self) -> Result<FunctionArguments<'gc>, ErrorKind> {
        match self.lexer.peek()?.cloned() {
            Some(Token::LeftParen) => {
                self.lexer.consume()?;
                let list = if !self.lexer.consume_if_eq(Token::RightParen)? {
                    let list = self.parse_expr_list()?;
                    self.expect(Token::RightParen)?;
                    list
                } else {
                    Vec::new()
                };
                Ok(FunctionArguments::Expressions(list))
            }
            Some(Token::LeftCurlyBracket) => Ok(FunctionArguments::TableConstructor(
                self.parse_table_constructor()?,
            )),
            Some(Token::String(s)) => {
                self.lexer.consume()?;
                Ok(FunctionArguments::String(s))
            }
            _ => Err(ErrorKind::unexpected_token("function arguments")),
        }
    }

    fn parse_suffixed_expr(&mut self) -> Result<SuffixedExpression<'gc>, ErrorKind> {
        let primary = self.parse_primary()?;
        let mut suffixes = Vec::new();
        loop {
            let suffix = match self.lexer.peek()? {
                Some(Token::Dot) => {
                    self.lexer.consume()?;
                    Suffix::Field(self.expect_name()?)
                }
                Some(Token::LeftBracket) => {
                    self.lexer.consume()?;
                    let expr = self.parse_expr()?;
                    self.expect(Token::RightBracket)?;
                    Suffix::Index(expr)
                }
                Some(Token::Colon) => {
                    self.lexer.consume()?;
                    Suffix::MethodCall {
                        name: self.expect_name()?,
                        args: self.parse_func_args()?,
                    }
                }
                Some(Token::LeftParen | Token::String(_) | Token::LeftCurlyBracket) => {
                    Suffix::FunctionCall {
                        args: self.parse_func_args()?,
                    }
                }
                _ => break,
            };
            suffixes.push(suffix);
        }
        Ok(SuffixedExpression { primary, suffixes })
    }

    fn parse_primary(&mut self) -> Result<Primary<'gc>, ErrorKind> {
        match self.lexer.peek()? {
            Some(Token::LeftParen) => {
                self.expect(Token::LeftParen)?;
                let expr = self.parse_expr()?;
                self.expect(Token::RightParen)?;
                Ok(Primary::Expression(expr.into()))
            }
            Some(Token::Name(_)) => Ok(Primary::Name(self.expect_name()?)),
            _ => Err(ErrorKind::UnexpectedSymbol),
        }
    }

    fn parse_table_constructor(&mut self) -> Result<TableConstructorExpression<'gc>, ErrorKind> {
        self.expect(Token::LeftCurlyBracket)?;
        let mut fields = Vec::new();
        loop {
            match self.lexer.peek()?.cloned() {
                Some(Token::Comma | Token::Semicolon) => {
                    self.lexer.consume()?;
                }
                Some(Token::RightCurlyBracket) => {
                    self.lexer.consume()?;
                    return Ok(TableConstructorExpression(fields));
                }
                Some(Token::Name(name)) if self.lexer.peek2()? == Some(&Token::Assign) => {
                    self.lexer.consume()?;
                    self.lexer.consume()?;
                    fields.push(TableField::Record {
                        key: TableRecordKey::Name(name),
                        value: self.parse_expr()?,
                    });
                }
                Some(Token::LeftBracket) => {
                    self.lexer.consume()?;
                    let key = TableRecordKey::Index(self.parse_expr()?);
                    self.expect(Token::RightBracket)?;
                    self.expect(Token::Assign)?;
                    fields.push(TableField::Record {
                        key,
                        value: self.parse_expr()?,
                    });
                }
                _ => fields.push(TableField::List(self.parse_expr()?)),
            }
        }
    }

    fn expect(&mut self, expected: impl Into<Option<Token<'gc>>>) -> Result<(), ErrorKind> {
        let got = self.lexer.consume()?;
        let expected = expected.into();
        if got == expected {
            Ok(())
        } else {
            Err(ErrorKind::unexpected_token(stringify_token_or_eof(
                &expected,
            )))
        }
    }

    fn expect_name(&mut self) -> Result<LuaString<'gc>, ErrorKind> {
        match self.lexer.consume()? {
            Some(Token::Name(name)) => Ok(name),
            _ => Err(ErrorKind::unexpected_token("<name>")),
        }
    }
}
