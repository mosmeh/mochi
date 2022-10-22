mod ast;
mod instruction;
mod ir;

use crate::{
    gc::GcContext,
    number_is_valid_integer,
    parser::ast::{
        BinaryOp, Block, Chunk, Expression, FunctionArguments, FunctionExpression, UnaryOp,
    },
    runtime::Metamethod,
    types::{
        Integer, LuaClosureProto, LuaString, RegisterIndex, UpvalueDescription, UpvalueIndex, Value,
    },
};
use ir::{ConstantIndex25, ConstantIndex8, IrAddress, IrInstruction, Label, ProtoIndex, RkIndex};
use std::{
    collections::{hash_map, HashMap},
    num::NonZeroU8,
};

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("function or expression needs too many registers")]
    TooManyRegisters,

    #[error("too many upvalues")]
    TooManyUpvalues,

    #[error("too many constants")]
    TooManyConstants,

    #[error("too many protos")]
    TooManyProtos,

    #[error("control structure too long")]
    ControlStructureTooLong,

    #[error("cannot use '...' outside a vararg function")]
    VarArgExpressionOutsideVarArgFunction,

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub fn codegen<'gc>(
    gc: &'gc GcContext,
    source: LuaString<'gc>,
    chunk: Chunk<'gc>,
) -> Result<LuaClosureProto<'gc>, CodegenError> {
    let mut generator = CodeGenerator::new(gc, source);
    generator.enter_frame();
    generator.current_frame().is_vararg = true;
    generator.codegen_chunk(chunk)?;
    let proto = generator.finish_frame()?;
    assert!(generator.frames.is_empty());
    Ok(proto)
}

const LUA_ENV: &[u8] = b"_ENV";

#[must_use]
#[derive(Debug)]
enum LazyLValue {
    Register(RegisterIndex),
    Upvalue(UpvalueIndex),
    UpvalueField {
        table: UpvalueIndex,
        field: ConstantIndex8,
    },
    TableGenericKey {
        table: RegisterIndex,
        key: RegisterIndex,
    },
    TableIntegerKey {
        table: RegisterIndex,
        key: u8,
    },
    TableField {
        table: RegisterIndex,
        field: ConstantIndex8,
    },
}

impl From<RegisterIndex> for LazyLValue {
    fn from(r: RegisterIndex) -> Self {
        Self::Register(r)
    }
}

impl From<UpvalueIndex> for LazyLValue {
    fn from(u: UpvalueIndex) -> Self {
        Self::Upvalue(u)
    }
}

impl From<LValue> for LazyLValue {
    fn from(lvalue: LValue) -> Self {
        match lvalue {
            LValue::Register(r) => Self::Register(r),
            LValue::Upvalue(u) => Self::Upvalue(u),
        }
    }
}

#[must_use]
#[derive(Debug)]
enum LValue {
    Register(RegisterIndex),
    Upvalue(UpvalueIndex),
}

impl From<RegisterIndex> for LValue {
    fn from(r: RegisterIndex) -> Self {
        Self::Register(r)
    }
}

impl From<UpvalueIndex> for LValue {
    fn from(u: UpvalueIndex) -> Self {
        Self::Upvalue(u)
    }
}

#[must_use]
#[derive(Debug)]
enum LazyRValue<'gc> {
    LValue(LazyLValue),
    Constant(Value<'gc>),
    Proto(ProtoIndex),
    FunctionCall {
        callee: Box<LazyRValue<'gc>>,
        args: FunctionArguments<'gc>,
        may_return_multiple_values: bool,
    },
    MethodCall {
        table: Box<LazyRValue<'gc>>,
        name: LuaString<'gc>,
        args: FunctionArguments<'gc>,
        may_return_multiple_values: bool,
    },
    UnaryOp {
        op: UnaryOp,
        inner: Box<LazyRValue<'gc>>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<LazyRValue<'gc>>,
        rhs: Box<LazyRValue<'gc>>,
        flipped: bool,
    },
    ShortCircuit {
        op: BinaryOp,
        lhs: Box<LazyRValue<'gc>>,
        rhs: Expression<'gc>,
    },
    Comparison {
        op: BinaryOp,
        lhs: Box<LazyRValue<'gc>>,
        rhs: Box<LazyRValue<'gc>>,
    },
    VarArg {
        may_have_multiple_values: bool,
    },
}

impl From<LazyLValue> for LazyRValue<'_> {
    fn from(r: LazyLValue) -> Self {
        Self::LValue(r)
    }
}

impl<'gc, T: Into<Value<'gc>>> From<T> for LazyRValue<'gc> {
    fn from(v: T) -> Self {
        Self::Constant(v.into())
    }
}

impl From<ProtoIndex> for LazyRValue<'_> {
    fn from(p: ProtoIndex) -> Self {
        Self::Proto(p)
    }
}

impl LazyRValue<'_> {
    fn may_have_multiple_values(&self) -> bool {
        match self {
            Self::FunctionCall {
                may_return_multiple_values,
                ..
            } => *may_return_multiple_values,
            Self::MethodCall {
                may_return_multiple_values,
                ..
            } => *may_return_multiple_values,
            Self::VarArg {
                may_have_multiple_values,
            } => *may_have_multiple_values,
            _ => false,
        }
    }
}

#[derive(Default)]
struct Frame<'gc> {
    register_top: RegisterIndex,
    max_stack_size: u8,

    ir_code: Vec<IrInstruction>,
    label_ir_addresses: Vec<Option<IrAddress>>,

    constants: HashMap<Value<'gc>, usize>,
    upvalues: HashMap<UpvalueDescription, UpvalueIndex>,
    protos: Vec<LuaClosureProto<'gc>>,

    local_variable_stack: Vec<(Option<LuaString<'gc>>, RegisterIndex)>,

    num_fixed_args: u8,
    is_vararg: bool,
    needs_to_close_upvalues: bool,
}

impl Frame<'_> {
    fn allocate_upvalue(
        &mut self,
        upvalue: UpvalueDescription,
    ) -> Result<UpvalueIndex, CodegenError> {
        let i = self.upvalues.len();
        match self.upvalues.entry(upvalue) {
            hash_map::Entry::Occupied(entry) => Ok(*entry.get()),
            hash_map::Entry::Vacant(entry) => {
                if let Ok(i) = i.try_into() {
                    let i = UpvalueIndex(i);
                    entry.insert(i);
                    Ok(i)
                } else {
                    Err(CodegenError::TooManyUpvalues)
                }
            }
        }
    }
}

struct CodeGenerator<'gc> {
    gc: &'gc GcContext,
    source: LuaString<'gc>,
    frames: Vec<Frame<'gc>>,
}

impl<'gc> CodeGenerator<'gc> {
    fn new(gc: &'gc GcContext, source: LuaString<'gc>) -> Self {
        Self {
            gc,
            source,
            frames: Default::default(),
        }
    }

    fn enter_frame(&mut self) {
        self.frames.push(Frame::default());
    }

    fn finish_frame(&mut self) -> Result<LuaClosureProto<'gc>, CodegenError> {
        ir::lower_ir(self.gc, self.source, self.frames.pop().unwrap())
    }

    fn current_frame(&mut self) -> &mut Frame<'gc> {
        self.frames.last_mut().unwrap()
    }

    fn emit(&mut self, insn: IrInstruction) {
        self.current_frame().ir_code.push(insn);
    }

    fn declare_label(&mut self) -> Label {
        let current = self.current_frame();
        let label = Label(current.label_ir_addresses.len());
        current.label_ir_addresses.push(None);
        label
    }

    fn place_label_here(&mut self, label: Label) {
        let current = self.current_frame();
        current.label_ir_addresses[label.0] = Some(IrAddress(current.ir_code.len()));
    }

    fn resolve_name(&mut self, name: LuaString<'gc>) -> Result<LazyLValue, CodegenError> {
        match self.try_resolve_name(name)? {
            Some(LValue::Register(r)) => Ok(r.into()),
            Some(LValue::Upvalue(u)) => Ok(u.into()),
            None => {
                let env = match self.try_resolve_name(self.gc.allocate_string(LUA_ENV))? {
                    Some(env) => env,
                    None => unreachable!(),
                };
                self.resolve_table_field(env, name)
            }
        }
    }

    fn try_resolve_name(&mut self, name: LuaString) -> Result<Option<LValue>, CodegenError> {
        self.try_resolve_name_at_level(name, self.frames.len() - 1)
    }

    fn try_resolve_name_at_level(
        &mut self,
        name: LuaString,
        level: usize,
    ) -> Result<Option<LValue>, CodegenError> {
        if let Some((_, register)) = self.frames[level]
            .local_variable_stack
            .iter()
            .rfind(|(n, _)| *n == Some(name))
        {
            return Ok(Some((*register).into()));
        }

        if level > 0 {
            return match self.try_resolve_name_at_level(name, level - 1)? {
                Some(LValue::Register(index)) => {
                    let desc = UpvalueDescription::Register(index);
                    let index = self.frames[level].allocate_upvalue(desc)?;
                    self.frames[level - 1].needs_to_close_upvalues = true;
                    Ok(Some(LValue::Upvalue(index)))
                }
                Some(LValue::Upvalue(index)) => {
                    let desc = UpvalueDescription::Upvalue(index);
                    let index = self.frames[level].allocate_upvalue(desc)?;
                    Ok(Some(LValue::Upvalue(index)))
                }
                None => Ok(None),
            };
        }

        if name.as_ref() == LUA_ENV {
            let desc = UpvalueDescription::Upvalue(UpvalueIndex(0));
            let index = self.frames[0].allocate_upvalue(desc)?;
            Ok(Some(LValue::Upvalue(index)))
        } else {
            Ok(None)
        }
    }

    fn emit_test_then_block_else_fallthrough(
        &mut self,
        condition: Expression<'gc>,
        block: Block<'gc>,
        break_label: impl Into<Option<Label>>,
    ) -> Result<(), CodegenError> {
        let condition = self.evaluate_expr(condition)?;
        let break_label = break_label.into();
        match condition {
            LazyRValue::Constant(Value::Nil | Value::Boolean(false)) => {
                return Ok(());
            }
            LazyRValue::Constant(_) | LazyRValue::Proto(_) => {
                self.codegen_block(block)?;
                if let Some(break_label) = break_label {
                    self.emit(IrInstruction::Jump {
                        target: break_label,
                    });
                }
                return Ok(());
            }
            LazyRValue::Comparison { op, lhs, rhs } => {
                self.emit_comparison(op, *lhs, *rhs, false)?;
            }
            condition => {
                let condition = self.discharge_to_any_register(condition)?;
                self.emit(IrInstruction::Test {
                    condition,
                    jump_on: false,
                });
            }
        }
        let false_label = self.declare_label();
        self.emit(IrInstruction::Jump {
            target: false_label,
        });
        self.codegen_block(block)?;
        if let Some(break_label) = break_label {
            self.emit(IrInstruction::Jump {
                target: break_label,
            });
        }
        self.place_label_here(false_label);
        Ok(())
    }

    fn emit_comparison(
        &mut self,
        op: BinaryOp,
        lhs: impl Into<LazyRValue<'gc>>,
        rhs: impl Into<LazyRValue<'gc>>,
        jump_on: bool,
    ) -> Result<(), CodegenError> {
        let lhs = self.discharge_to_any_register(lhs)?;
        let rhs = rhs.into();

        if let LazyRValue::Constant(constant) = rhs {
            match constant {
                Value::Integer(i) => {
                    if let Ok(rhs) = i.try_into() {
                        self.emit(IrInstruction::CompareImmediate {
                            op,
                            lhs,
                            rhs,
                            rhs_is_float: false,
                            jump_on,
                        });
                        return Ok(());
                    }
                }
                Value::Number(x) if number_is_valid_integer(x) => {
                    if let Ok(rhs) = (x as Integer).try_into() {
                        self.emit(IrInstruction::CompareImmediate {
                            op,
                            lhs,
                            rhs,
                            rhs_is_float: true,
                            jump_on,
                        });
                        return Ok(());
                    }
                }
                _ => (),
            }

            let op_has_constant_variant = matches!(op, BinaryOp::Eq | BinaryOp::Ne);
            if op_has_constant_variant {
                if let Some(rhs) = self.try_allocate_rk_constant(constant) {
                    self.emit(IrInstruction::CompareConstant {
                        op,
                        lhs,
                        rhs,
                        jump_on,
                    });
                    return Ok(());
                }
            }
        }

        let rhs = self.discharge_to_any_register(rhs)?;
        self.emit(IrInstruction::Compare {
            op,
            lhs,
            rhs,
            jump_on,
        });
        Ok(())
    }

    fn emit_function(&mut self, expr: FunctionExpression<'gc>) -> Result<ProtoIndex, CodegenError> {
        self.enter_frame();

        let num_fixed_args = expr.params.len().try_into().unwrap();
        let current = self.current_frame();
        current.num_fixed_args = num_fixed_args;
        current.is_vararg = expr.is_vararg;
        if expr.is_vararg {
            self.emit(IrInstruction::PrepareVarArg { num_fixed_args });
        }

        for param in expr.params {
            let register = self.allocate_register()?;
            self.current_frame()
                .local_variable_stack
                .push((Some(param), register));
        }

        let has_return = expr.body.return_statement.is_some();
        self.codegen_block(expr.body)?;
        if !has_return {
            let Frame {
                num_fixed_args,
                is_vararg,
                needs_to_close_upvalues: close_upvalues,
                ..
            } = *self.current_frame();
            self.emit(IrInstruction::Return {
                base: RegisterIndex(0),
                count: Some(0),
                num_fixed_args,
                is_vararg,
                close_upvalues,
            });
        }

        let proto = self.finish_frame()?;
        self.allocate_proto(proto)
    }

    fn emit_func_call(
        &mut self,
        callee: impl Into<LazyRValue<'gc>>,
        args: FunctionArguments<'gc>,
        dest: RegisterIndex,
    ) -> Result<(), CodegenError> {
        self.discharge_to_register(callee, dest)?;
        let num_fixed_args = self.emit_func_args(args, RegisterIndex(dest.0 + 1))?;
        self.emit(IrInstruction::Call {
            callee: dest,
            num_fixed_args,
        });
        self.current_frame().register_top = RegisterIndex(dest.0 + 1);
        Ok(())
    }

    fn emit_method_call(
        &mut self,
        table: impl Into<LazyRValue<'gc>>,
        name: LuaString<'gc>,
        args: FunctionArguments<'gc>,
        dest: RegisterIndex,
    ) -> Result<(), CodegenError> {
        let table = self.discharge_to_any_register(table)?;
        let key = self.discharge_to_rk(name)?;
        self.ensure_register_window(dest, 2)?;
        self.emit(IrInstruction::GetSelf { dest, table, key });

        let num_fixed_args = self.emit_func_args(args, RegisterIndex(dest.0 + 2))?;
        self.emit(IrInstruction::Call {
            callee: dest,
            num_fixed_args: num_fixed_args.map(|n| n + 1),
        });

        self.current_frame().register_top = RegisterIndex(dest.0 + 1);
        Ok(())
    }

    fn emit_func_args(
        &mut self,
        args: FunctionArguments<'gc>,
        dest: RegisterIndex,
    ) -> Result<Option<u8>, CodegenError> {
        let num_fixed_args = match args {
            FunctionArguments::Expressions(expressions) => {
                let count = self.emit_open_expr_list(expressions, dest)?;
                count.map(|c| c.try_into().unwrap())
            }
            FunctionArguments::TableConstructor(expr) => {
                self.ensure_register_window(dest, 1)?;
                let table = self.evaluate_table_constructor_expr(expr)?;
                self.discharge_to_register(table, dest)?;
                Some(1)
            }
            FunctionArguments::String(string) => {
                self.discharge_to_register(string, dest)?;
                Some(1)
            }
        };
        Ok(num_fixed_args)
    }

    fn emit_open_expr_list(
        &mut self,
        mut expressions: Vec<Expression<'gc>>,
        dest: RegisterIndex,
    ) -> Result<Option<usize>, CodegenError> {
        let num_expressions = expressions.len();
        self.ensure_register_window(dest, num_expressions)?;

        let last_expr = expressions.pop();
        for (i, expr) in expressions.into_iter().enumerate() {
            let value = self.evaluate_expr(expr)?;
            self.discharge_to_register(value, RegisterIndex(dest.0 + i as u8))?;
        }

        let mut is_open = false;
        if let Some(last_expr) = last_expr {
            let value = self.evaluate_expr(last_expr)?;
            if value.may_have_multiple_values() {
                is_open = true;
            }
            self.discharge_to_register(value, RegisterIndex(dest.0 + (num_expressions - 1) as u8))?;
        }

        Ok((!is_open).then_some(num_expressions))
    }

    fn emit_assignment(
        &mut self,
        lhs: impl Into<LazyLValue>,
        rhs: impl Into<LazyRValue<'gc>>,
    ) -> Result<(), CodegenError> {
        match lhs.into() {
            LazyLValue::Register(dest) => {
                self.discharge_to_register(rhs, dest)?;
            }
            LazyLValue::Upvalue(upvalue) => {
                let value = self.discharge_to_any_register(rhs)?;
                self.emit(IrInstruction::SetUpvalue { upvalue, value });
            }
            LazyLValue::UpvalueField { table, field } => {
                let value = self.discharge_to_rk(rhs)?;
                self.emit(IrInstruction::SetUpvalueField {
                    table,
                    field,
                    value,
                });
            }
            LazyLValue::TableGenericKey { table, key } => {
                let value = self.discharge_to_rk(rhs)?;
                self.emit(IrInstruction::SetTableGenericKey { table, key, value });
            }
            LazyLValue::TableIntegerKey { table, key } => {
                let value = self.discharge_to_rk(rhs)?;
                self.emit(IrInstruction::SetTableIntegerKey { table, key, value });
            }
            LazyLValue::TableField { table, field } => {
                let value = self.discharge_to_rk(rhs)?;
                self.emit(IrInstruction::SetTableField {
                    table,
                    field,
                    value,
                });
            }
        }
        Ok(())
    }

    fn emit_assigned_values(
        &mut self,
        mut values: Vec<Expression<'gc>>,
        num_expected: usize,
    ) -> Result<Vec<RegisterIndex>, CodegenError> {
        let num_values = values.len();
        let mut registers = Vec::with_capacity(num_values);

        let last_value = values.pop();
        for value in values {
            let value = self.evaluate_expr(value)?;
            let register = self.discharge_to_new_register(value)?;
            registers.push(register);
        }

        if let Some(last_value) = last_value {
            let value = self.evaluate_expr(last_value)?;
            if value.may_have_multiple_values() {
                let base = self.discharge_to_new_register(value)?;
                if let Some(num_extra) = num_expected.checked_sub(num_values) {
                    for i in 0..=num_extra {
                        let i: u8 = i.try_into().unwrap();
                        let register = RegisterIndex(base.0 + i);
                        registers.push(register);
                    }
                }
            } else {
                let register = self.discharge_to_new_register(value)?;
                registers.push(register);
            }
        }

        Ok(registers)
    }

    fn force_lvalue(&mut self, lvalue: impl Into<LazyLValue>) -> Result<LValue, CodegenError> {
        match lvalue.into() {
            LazyLValue::Upvalue(u) => Ok(LValue::Upvalue(u)),
            lvalue => Ok(self.discharge_to_any_register(lvalue)?.into()),
        }
    }

    fn wrap_rvalue(
        &mut self,
        rvalue: impl Into<LazyRValue<'gc>>,
    ) -> Result<LazyLValue, CodegenError> {
        match rvalue.into() {
            LazyRValue::LValue(l) => Ok(l),
            rvalue => Ok(self.discharge_to_any_register(rvalue)?.into()),
        }
    }

    fn discharge_to_register(
        &mut self,
        rvalue: impl Into<LazyRValue<'gc>>,
        dest: RegisterIndex,
    ) -> Result<(), CodegenError> {
        match rvalue.into() {
            LazyRValue::LValue(lvalue) => match lvalue {
                LazyLValue::Register(source) => {
                    if source != dest {
                        self.emit(IrInstruction::Move { dest, source });
                    }
                }
                LazyLValue::Upvalue(upvalue) => {
                    self.emit(IrInstruction::GetUpvalue { dest, upvalue });
                }
                LazyLValue::UpvalueField { table, field } => {
                    self.emit(IrInstruction::GetUpvalueField { dest, table, field });
                }
                LazyLValue::TableGenericKey { table, key } => {
                    self.emit(IrInstruction::GetTableGenericKey { dest, table, key });
                }
                LazyLValue::TableIntegerKey { table, key } => {
                    self.emit(IrInstruction::GetTableIntegerKey { dest, table, key });
                }
                LazyLValue::TableField { table, field } => {
                    self.emit(IrInstruction::GetTableField { dest, table, field });
                }
            },
            LazyRValue::Constant(constant) => {
                match constant {
                    Value::Nil => {
                        self.emit(IrInstruction::LoadNil { dest });
                        return Ok(());
                    }
                    Value::Boolean(immediate) => {
                        self.emit(IrInstruction::LoadBoolean { dest, immediate });
                        return Ok(());
                    }
                    Value::Integer(i) => {
                        if let Ok(immediate) = i.try_into() {
                            self.emit(IrInstruction::LoadInteger { dest, immediate });
                            return Ok(());
                        }
                    }
                    Value::Number(x) => {
                        let floor = x as Integer;
                        if floor as f64 == x {
                            if let Ok(immediate) = floor.try_into() {
                                self.emit(IrInstruction::LoadFloat { dest, immediate });
                                return Ok(());
                            }
                        }
                    }
                    Value::String(_) => (),
                    _ => unreachable!(),
                };
                let constant = self.allocate_constant(constant)?;
                self.emit(IrInstruction::LoadConstant { dest, constant })
            }
            LazyRValue::Proto(proto) => self.emit(IrInstruction::GetClosure { dest, proto }),
            LazyRValue::FunctionCall {
                callee,
                args,
                may_return_multiple_values: _,
            } => {
                self.emit_func_call(*callee, args, dest)?;
            }
            LazyRValue::MethodCall {
                table,
                name,
                args,
                may_return_multiple_values: _,
            } => {
                self.emit_method_call(*table, name, args, dest)?;
            }
            LazyRValue::UnaryOp { op, inner } => {
                let operand = if let LazyRValue::LValue(LazyLValue::Register(lhs)) = *inner {
                    lhs
                } else {
                    self.discharge_to_register(*inner, dest)?;
                    dest
                };
                self.emit(IrInstruction::UnaryOp { op, dest, operand });
            }
            LazyRValue::BinaryOp {
                op,
                lhs,
                rhs,
                flipped,
            } => {
                if op == BinaryOp::Concat {
                    let lhs = if dest.0 + 1 == self.current_frame().register_top.0 {
                        self.discharge_to_register(*lhs, dest)?;
                        dest
                    } else {
                        self.discharge_to_new_register(*lhs)?
                    };
                    self.ensure_register_window(lhs, 2)?;
                    self.discharge_to_register(*rhs, RegisterIndex(lhs.0 + 1))?;
                    self.emit(IrInstruction::Concatenate { dest: lhs });
                    if lhs != dest {
                        self.emit(IrInstruction::Move { dest, source: lhs });
                    }
                    return Ok(());
                };

                let (lhs, rhs) = match (*lhs, *rhs) {
                    (lhs, LazyRValue::LValue(LazyLValue::Register(rhs))) if rhs == dest => (
                        self.discharge_to_any_register(lhs)?,
                        LazyRValue::LValue(rhs.into()),
                    ),
                    (LazyRValue::LValue(LazyLValue::Register(lhs)), rhs) => (lhs, rhs),
                    (lhs, rhs) => {
                        self.discharge_to_register(lhs, dest)?;
                        (dest, rhs)
                    }
                };

                if let LazyRValue::Constant(constant) = rhs {
                    match constant {
                        Value::Integer(i) => {
                            match op {
                                BinaryOp::Add | BinaryOp::Shr => {
                                    if let Ok(rhs) = i.try_into() {
                                        self.emit(IrInstruction::BinaryOpImmediate {
                                            op,
                                            dest,
                                            lhs,
                                            rhs,
                                            metamethod: op.metamethod(),
                                            flipped,
                                        });
                                        return Ok(());
                                    }
                                }
                                BinaryOp::Sub => {
                                    if let Ok(rhs) = (-i).try_into() {
                                        self.emit(IrInstruction::BinaryOpImmediate {
                                            op: BinaryOp::Add,
                                            dest,
                                            lhs,
                                            rhs,
                                            metamethod: Metamethod::Sub,
                                            flipped,
                                        });
                                        return Ok(());
                                    }
                                }
                                BinaryOp::Shl => {
                                    if let Ok(rhs) = (-i).try_into() {
                                        self.emit(IrInstruction::BinaryOpImmediate {
                                            op: BinaryOp::Shr,
                                            dest,
                                            lhs,
                                            rhs,
                                            metamethod: Metamethod::Shl,
                                            flipped,
                                        });
                                        return Ok(());
                                    }
                                }
                                _ => (),
                            }

                            let op_has_constant_variant = matches!(
                                op,
                                BinaryOp::Add
                                    | BinaryOp::Sub
                                    | BinaryOp::Mul
                                    | BinaryOp::Div
                                    | BinaryOp::IDiv
                                    | BinaryOp::Pow
                                    | BinaryOp::Mod
                                    | BinaryOp::BAnd
                                    | BinaryOp::BXor
                                    | BinaryOp::BOr
                            );
                            if op_has_constant_variant {
                                if let Some(rhs) = self.try_allocate_rk_constant(constant) {
                                    self.emit(IrInstruction::BinaryOpConstant {
                                        op,
                                        dest,
                                        lhs,
                                        rhs,
                                        flipped,
                                    });
                                    return Ok(());
                                }
                            }
                        }
                        Value::Number(_) => {
                            let op_has_constant_variant = matches!(
                                op,
                                BinaryOp::Add
                                    | BinaryOp::Sub
                                    | BinaryOp::Mul
                                    | BinaryOp::Div
                                    | BinaryOp::IDiv
                                    | BinaryOp::Pow
                                    | BinaryOp::Mod
                            );
                            if op_has_constant_variant {
                                if let Some(rhs) = self.try_allocate_rk_constant(constant) {
                                    self.emit(IrInstruction::BinaryOpConstant {
                                        op,
                                        dest,
                                        lhs,
                                        rhs,
                                        flipped,
                                    });
                                    return Ok(());
                                }
                            }
                        }
                        _ => (),
                    }
                }

                let rhs = self.discharge_to_any_register(rhs)?;
                self.emit(IrInstruction::BinaryOp { op, dest, lhs, rhs });
            }
            LazyRValue::ShortCircuit { op, lhs, rhs } => {
                let source = self.discharge_to_any_register(*lhs)?;
                self.emit(IrInstruction::ConditionalMove {
                    dest,
                    source,
                    move_and_jump_on: op == BinaryOp::Or,
                });
                let end_label = self.declare_label();
                self.emit(IrInstruction::Jump { target: end_label });
                let rhs = self.evaluate_expr(rhs)?;
                self.discharge_to_register(rhs, dest)?;
                self.place_label_here(end_label);
            }
            LazyRValue::Comparison { op, lhs, rhs } => {
                self.emit_comparison(op, *lhs, *rhs, true)?;
                let true_label = self.declare_label();
                self.emit(IrInstruction::Jump { target: true_label });
                self.emit(IrInstruction::LoadFalseAndSkip { dest });
                self.place_label_here(true_label);
                self.emit(IrInstruction::LoadBoolean {
                    dest,
                    immediate: true,
                });
            }
            LazyRValue::VarArg {
                may_have_multiple_values,
            } => self.emit(IrInstruction::GetVarArg {
                dest,
                num_wanted: if may_have_multiple_values {
                    None
                } else {
                    NonZeroU8::new(1)
                },
            }),
        };
        Ok(())
    }

    fn discharge_to_new_register(
        &mut self,
        rvalue: impl Into<LazyRValue<'gc>>,
    ) -> Result<RegisterIndex, CodegenError> {
        let register = self.allocate_register()?;
        self.discharge_to_register(rvalue, register)?;
        Ok(register)
    }

    fn discharge_to_any_register(
        &mut self,
        rvalue: impl Into<LazyRValue<'gc>>,
    ) -> Result<RegisterIndex, CodegenError> {
        let rvalue = rvalue.into();
        if let LazyRValue::LValue(LazyLValue::Register(register)) = rvalue {
            Ok(register)
        } else {
            Ok(self.discharge_to_new_register(rvalue)?)
        }
    }

    fn discharge_to_rk(
        &mut self,
        rvalue: impl Into<LazyRValue<'gc>>,
    ) -> Result<RkIndex, CodegenError> {
        let rvalue = rvalue.into();
        if let LazyRValue::Constant(constant) = rvalue {
            if let Some(constant) = self.try_allocate_rk_constant(constant) {
                return Ok(constant.into());
            }
        }
        let register = self.discharge_to_any_register(rvalue)?;
        Ok(register.into())
    }

    fn allocate_register(&mut self) -> Result<RegisterIndex, CodegenError> {
        let current = self.current_frame();
        let current_top = current.register_top.0;
        if current_top < u8::MAX - 1 {
            let new_top = current_top + 1;
            current.register_top.0 = new_top;
            current.max_stack_size = current.max_stack_size.max(new_top + 1);
            Ok(RegisterIndex(current_top))
        } else {
            Err(CodegenError::TooManyRegisters)
        }
    }

    fn ensure_register_window(
        &mut self,
        base: RegisterIndex,
        len: usize,
    ) -> Result<(), CodegenError> {
        let current = self.current_frame();
        if let Ok(requested_top) = (base.0 as usize + len).try_into() {
            let new_top = current.register_top.0.max(requested_top);
            if let Some(new_stack_size) = requested_top.checked_add(1) {
                current.register_top.0 = new_top;
                current.max_stack_size = current.max_stack_size.max(new_stack_size);
                return Ok(());
            }
        }
        Err(CodegenError::TooManyRegisters)
    }

    fn allocate_constant(
        &mut self,
        value: impl Into<Value<'gc>>,
    ) -> Result<ConstantIndex25, CodegenError> {
        let constants = &mut self.current_frame().constants;
        let i = constants.len();
        match constants.entry(value.into()) {
            hash_map::Entry::Occupied(entry) => Ok((*entry.get()).try_into().unwrap()),
            hash_map::Entry::Vacant(entry) => {
                if let Ok(constant) = i.try_into() {
                    entry.insert(i);
                    Ok(constant)
                } else {
                    Err(CodegenError::TooManyConstants)
                }
            }
        }
    }

    fn try_allocate_rk_constant(&mut self, value: impl Into<Value<'gc>>) -> Option<ConstantIndex8> {
        let constants = &mut self.current_frame().constants;
        let i = constants.len();
        match constants.entry(value.into()) {
            hash_map::Entry::Occupied(entry) => (*entry.get()).try_into().ok(),
            hash_map::Entry::Vacant(entry) => {
                if let Ok(constant) = i.try_into() {
                    entry.insert(i);
                    Some(constant)
                } else {
                    None
                }
            }
        }
    }

    fn allocate_proto(&mut self, proto: LuaClosureProto<'gc>) -> Result<ProtoIndex, CodegenError> {
        let protos = &mut self.current_frame().protos;
        if let Ok(i) = protos.len().try_into() {
            protos.push(proto);
            Ok(i)
        } else {
            Err(CodegenError::TooManyProtos)
        }
    }
}

impl BinaryOp {
    fn metamethod(&self) -> Metamethod {
        match self {
            Self::Add => Metamethod::Add,
            Self::Sub => Metamethod::Sub,
            Self::Mul => Metamethod::Mul,
            Self::Div => Metamethod::Div,
            Self::IDiv => Metamethod::IDiv,
            Self::Pow => Metamethod::Pow,
            Self::Mod => Metamethod::Mod,
            Self::BAnd => Metamethod::BAnd,
            Self::BXor => Metamethod::BXor,
            Self::BOr => Metamethod::BOr,
            Self::Shr => Metamethod::Shr,
            Self::Shl => Metamethod::Shl,
            _ => unreachable!(),
        }
    }
}
