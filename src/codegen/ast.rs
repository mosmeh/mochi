use super::{
    ir::{IrInstruction, RkIndex},
    CodeGenerator, CodegenError, LValue, LazyLValue, LazyRValue,
};
use crate::{
    parser::ast::{
        AssignmentStatement, BinaryOp, BinaryOpExpression, Block, Chunk, Expression, ForStatement,
        FunctionCallStatement, FunctionExpression, FunctionParameter, FunctionStatement,
        IfStatement, LocalVariableStatement, Primary, RepeatStatement, Statement, Suffix,
        SuffixedExpression, TableConstructorExpression, TableField, TableRecordKey,
        UnaryOpExpression, Variable, WhileStatement,
    },
    types::{Integer, LuaString, RegisterIndex, Value},
};
use bstr::B;

impl<'gc> CodeGenerator<'gc> {
    pub fn codegen_chunk(&mut self, chunk: Chunk<'gc>) -> Result<(), CodegenError> {
        self.emit(IrInstruction::PrepareVarArg { num_fixed_args: 0 });
        let has_return = chunk.0.return_statement.is_some();
        self.codegen_block(chunk.0)?;
        if !has_return {
            self.emit(IrInstruction::Return {
                base: RegisterIndex(0),
                count: Some(0),
                close_upvalues: false,
            });
        }
        Ok(())
    }

    pub fn codegen_block(&mut self, block: Block<'gc>) -> Result<(), CodegenError> {
        for statement in block.statements {
            self.codegen_statement(statement)?;
        }
        if let Some(mut return_statement) = block.return_statement {
            let (base, count) = match return_statement.0.len() {
                0 => (RegisterIndex(0), Some(0)),
                1 => {
                    let expr = return_statement.0.pop().unwrap();
                    let value = self.evaluate_expr(expr)?;
                    let count = (!value.may_have_multiple_values()).then_some(1);
                    let base = self.discharge_to_any_register(value)?;
                    (base, count)
                }
                _ => {
                    let base = self.allocate_register()?;
                    let count = self.emit_open_expr_list(return_statement.0, base)?;
                    (base, count.map(|c| c.try_into().unwrap()))
                }
            };
            let close_upvalues = self.current_frame().needs_to_close_upvalues;
            self.emit(IrInstruction::Return {
                base,
                count,
                close_upvalues,
            });
        }
        Ok(())
    }

    pub fn evaluate_expr(
        &mut self,
        expr: Expression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        match expr {
            Expression::Float(x) => Ok(x.into()),
            Expression::Integer(i) => Ok(i.into()),
            Expression::String(s) => Ok(s.into()),
            Expression::Nil => Ok(Value::Nil.into()),
            Expression::Boolean(b) => Ok(b.into()),
            Expression::VarArg => todo!("vararg"),
            Expression::TableConstructor(t) => self.evaluate_table_constructor_expr(t),
            Expression::Function(f) => self.evaluate_func_expr(f),
            Expression::Suffixed(s) => self.evaluate_suffixed_expr(s),
            Expression::UnaryOp(u) => self.evaluate_unary_op_expr(u),
            Expression::BinaryOp(b) => self.evaluate_binary_op_expr(b),
        }
    }

    pub fn evaluate_table_constructor_expr(
        &mut self,
        expr: TableConstructorExpression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        let table = self.allocate_register()?;
        self.emit(IrInstruction::CreateTable { dest: table });

        const MAX_NUM_FIELDS_PER_FLUSH: u8 = 50;
        let mut next_index_offset = 0;
        let mut num_pending_fields = 0;

        for field in expr.0 {
            match field {
                TableField::List(expr) => {
                    if num_pending_fields >= MAX_NUM_FIELDS_PER_FLUSH {
                        self.emit(IrInstruction::SetList {
                            table,
                            count: num_pending_fields,
                            index_offset: next_index_offset,
                        });
                        next_index_offset += num_pending_fields as usize;
                        num_pending_fields = 0;
                        self.current_frame().register_top.0 = table.0 + 1;
                    }
                    num_pending_fields += 1;
                    self.ensure_register_window(table, num_pending_fields as usize + 1)?;
                    let value = self.evaluate_expr(expr)?;
                    self.discharge_to_register(value, RegisterIndex(table.0 + num_pending_fields))?;
                }
                TableField::Record { key, value } => {
                    let lhs = match key {
                        TableRecordKey::Name(name) => self.resolve_table_field(table, name)?,
                        TableRecordKey::Index(index) => self.resolve_table_index(table, index)?,
                    };
                    let rhs = self.evaluate_expr(value)?;
                    self.emit_assignment(lhs, rhs)?;
                }
            }
        }

        if num_pending_fields > 0 {
            self.emit(IrInstruction::SetList {
                table,
                count: num_pending_fields,
                index_offset: next_index_offset,
            });
        }

        Ok(LazyLValue::Register(table).into())
    }

    fn codegen_statement(&mut self, statement: Statement<'gc>) -> Result<(), CodegenError> {
        match statement {
            Statement::If(s) => self.codegen_if_statement(s)?,
            Statement::While(s) => self.codegen_while_statement(s)?,
            Statement::Do(b) => self.codegen_block(b)?,
            Statement::For(s) => self.codegen_for_statement(s)?,
            Statement::Repeat(s) => self.codegen_repeat_statement(s)?,
            Statement::Function(s) => self.codegen_func_statement(s)?,
            Statement::LocalFunction(s) => self.codegen_local_func_statement(s)?,
            Statement::LocalVariable(s) => self.codegen_local_variable_statement(s)?,
            Statement::Label(_) => todo!("label"),
            Statement::Break => todo!("break"),
            Statement::Goto(_) => todo!("goto"),
            Statement::FunctionCall(s) => self.codegen_func_call_statement(s)?,
            Statement::Assignment(s) => self.codegen_assignment_statement(s)?,
        };

        let current = self.current_frame();
        current.register_top.0 = current
            .local_variable_stack
            .iter()
            .map(|(_, i)| i.0)
            .max()
            .map(|i| i + 1)
            .unwrap_or_default();

        Ok(())
    }

    fn codegen_if_statement(
        &mut self,
        mut statement: IfStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let end_label = self.declare_label();

        // if condition then body
        let break_label = (statement.else_part.is_some() || !statement.else_if_parts.is_empty())
            .then_some(end_label);
        self.emit_test_then_block_else_fallthrough(
            statement.condition,
            statement.body,
            break_label,
        )?;

        // elseif condition then block
        if let Some(last) = statement.else_if_parts.pop() {
            for (condition, block) in statement.else_if_parts {
                self.emit_test_then_block_else_fallthrough(condition, block, end_label)?;
            }

            let (condition, block) = last;
            let break_label = statement.else_part.is_some().then_some(end_label);
            self.emit_test_then_block_else_fallthrough(condition, block, break_label)?;
        }

        // else block
        if let Some(else_part) = statement.else_part {
            self.codegen_block(else_part)?;
        }

        self.place_label_here(end_label);

        Ok(())
    }

    fn codegen_while_statement(
        &mut self,
        statement: WhileStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let start_label = self.declare_label();
        self.place_label_here(start_label);
        self.emit_test_then_block_else_fallthrough(statement.condition, statement.body, start_label)
    }

    fn codegen_for_statement(&mut self, statement: ForStatement<'gc>) -> Result<(), CodegenError> {
        let prev_num_local_vars = self.current_frame().local_variable_stack.len();
        let base = self.allocate_register()?;

        let (is_generic, body) = match statement {
            ForStatement::Numerical {
                control,
                initial_value,
                limit,
                step,
                body,
            } => {
                self.ensure_register_window(base, 4)?;

                let init_register = base;
                self.current_frame()
                    .local_variable_stack
                    .push((None, init_register));
                let initial_value = self.evaluate_expr(initial_value)?;
                self.discharge_to_register(initial_value, init_register)?;

                let limit_register = RegisterIndex(base.0 + 1);
                self.current_frame()
                    .local_variable_stack
                    .push((None, limit_register));
                let limit = self.evaluate_expr(limit)?;
                self.discharge_to_register(limit, limit_register)?;

                let step_register = RegisterIndex(base.0 + 2);
                self.current_frame()
                    .local_variable_stack
                    .push((None, step_register));
                let step = if let Some(step) = step {
                    self.evaluate_expr(step)?
                } else {
                    1.into()
                };
                self.discharge_to_register(step, step_register)?;

                let control_register = RegisterIndex(base.0 + 3);
                self.current_frame()
                    .local_variable_stack
                    .push((Some(control), control_register));

                (false, body)
            }
            ForStatement::Generic {
                variables,
                expressions,
                body,
            } => {
                self.ensure_register_window(base, 4 + variables.len())?;

                let mut expr_registers = self.emit_assigned_values(expressions, 4)?.into_iter();
                for i in 0..4 {
                    let expr_rvalue: LazyRValue = if let Some(register) = expr_registers.next() {
                        LazyLValue::Register(register).into()
                    } else {
                        Value::Nil.into()
                    };
                    let register = RegisterIndex(base.0 + i);
                    self.discharge_to_register(expr_rvalue, register)?;
                    self.current_frame()
                        .local_variable_stack
                        .push((None, register));
                }

                for (i, variable) in variables.into_iter().enumerate() {
                    self.current_frame()
                        .local_variable_stack
                        .push((Some(variable), RegisterIndex(base.0 + 4 + i as u8)));
                }

                (true, body)
            }
        };

        let end_label = self.declare_label();
        self.emit(IrInstruction::PrepareForLoop {
            base,
            skip_target: end_label,
            is_generic,
        });

        let start_label = self.declare_label();
        self.place_label_here(start_label);
        self.codegen_block(body)?;
        self.place_label_here(end_label);

        if is_generic {
            self.emit(IrInstruction::GenericForCall { base });
        }

        self.emit(IrInstruction::ForLoop {
            base,
            next_target: start_label,
            is_generic,
        });

        self.current_frame()
            .local_variable_stack
            .truncate(prev_num_local_vars);

        Ok(())
    }

    fn codegen_repeat_statement(
        &mut self,
        statement: RepeatStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let start_label = self.declare_label();
        self.place_label_here(start_label);

        self.codegen_block(statement.body)?;

        let condition = self.evaluate_expr(statement.condition)?;
        match condition {
            LazyRValue::Constant(Value::Nil | Value::Boolean(false)) => (),
            LazyRValue::Constant(_) | LazyRValue::Proto(_) => return Ok(()),
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
        self.emit(IrInstruction::Jump {
            target: start_label,
        });

        Ok(())
    }

    fn codegen_func_statement(
        &mut self,
        statement: FunctionStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let mut lvalue = self.resolve_name(statement.name)?;
        for field in statement.fields {
            lvalue = self.resolve_table_field(lvalue, field)?;
        }

        let proto = if let Some(method) = statement.method {
            lvalue = self.resolve_table_field(lvalue, method)?;

            let mut params = vec![FunctionParameter::Name(self.gc.allocate_string(B("self")))];
            params.extend_from_slice(&statement.params);
            self.emit_function(params, statement.body)?
        } else {
            self.emit_function(statement.params, statement.body)?
        };

        self.emit_assignment(lvalue, proto)?;
        Ok(())
    }

    fn codegen_local_func_statement(
        &mut self,
        statement: FunctionStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let register = self.allocate_register()?;
        self.current_frame()
            .local_variable_stack
            .push((Some(statement.name), register));
        self.codegen_func_statement(statement)
    }

    fn codegen_local_variable_statement(
        &mut self,
        statement: LocalVariableStatement<'gc>,
    ) -> Result<(), CodegenError> {
        if statement
            .variables
            .iter()
            .any(|var| var.attribute.is_some())
        {
            todo!("attribute")
        }

        let mut value_registers = self
            .emit_assigned_values(statement.values, statement.variables.len())?
            .into_iter();

        for variable in statement.variables {
            let register = if let Some(register) = value_registers.next() {
                register
            } else {
                self.discharge_to_new_register(Value::Nil)?
            };
            self.current_frame()
                .local_variable_stack
                .push((Some(variable.name), register));
        }

        Ok(())
    }

    fn codegen_func_call_statement(
        &mut self,
        statement: FunctionCallStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let _ = self.resolve_suffixed_expr(statement.0)?;
        Ok(())
    }

    fn codegen_assignment_statement(
        &mut self,
        statement: AssignmentStatement<'gc>,
    ) -> Result<(), CodegenError> {
        let mut rhs_registers = self
            .emit_assigned_values(statement.rhs, statement.lhs.len())?
            .into_iter();

        for lhs in statement.lhs {
            let rhs: LazyRValue = if let Some(register) = rhs_registers.next() {
                LazyLValue::Register(register).into()
            } else {
                Value::Nil.into()
            };
            let lhs = self.resolve_variable(lhs)?;
            self.emit_assignment(lhs, rhs)?;
        }

        Ok(())
    }

    fn evaluate_func_expr(
        &mut self,
        expr: FunctionExpression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        let proto = self.emit_function(expr.params, expr.body)?;
        Ok(proto.into())
    }

    fn evaluate_suffixed_expr(
        &mut self,
        suffixed: SuffixedExpression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        let mut rvalue = self.evaluate_primary(suffixed.primary)?;
        for suffix in suffixed.suffixes {
            rvalue = match suffix {
                Suffix::Field(field) => {
                    let table = self.wrap_rvalue(rvalue)?;
                    let value = self.resolve_table_field(table, field)?;
                    LazyRValue::LValue(value)
                }
                Suffix::Index(index) => {
                    let table = self.wrap_rvalue(rvalue)?;
                    let value = self.resolve_table_index(table, index)?;
                    LazyRValue::LValue(value)
                }
                Suffix::FunctionCall { args } => LazyRValue::FunctionCall {
                    callee: rvalue.into(),
                    args,
                    may_return_multiple_values: true,
                },
                Suffix::MethodCall { name, args } => LazyRValue::MethodCall {
                    table: rvalue.into(),
                    name,
                    args,
                    may_return_multiple_values: true,
                },
            };
        }
        Ok(rvalue)
    }

    fn evaluate_primary(&mut self, primary: Primary<'gc>) -> Result<LazyRValue<'gc>, CodegenError> {
        match primary {
            Primary::Name(name) => {
                let name = self.resolve_name(name)?;
                Ok(name.into())
            }
            Primary::Expression(expr) => {
                let value = self.evaluate_expr(*expr)?;
                let value = match value {
                    LazyRValue::FunctionCall {
                        callee,
                        args,
                        may_return_multiple_values: _,
                    } => LazyRValue::FunctionCall {
                        callee,
                        args,
                        may_return_multiple_values: false,
                    },
                    LazyRValue::MethodCall {
                        table,
                        name,
                        args,
                        may_return_multiple_values: _,
                    } => LazyRValue::MethodCall {
                        table,
                        name,
                        args,
                        may_return_multiple_values: false,
                    },
                    _ => value,
                };
                Ok(value)
            }
        }
    }

    fn evaluate_unary_op_expr(
        &mut self,
        expr: UnaryOpExpression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        let inner = self.evaluate_expr(*expr.inner)?;
        Ok(LazyRValue::UnaryOp {
            op: expr.op,
            inner: inner.into(),
        })
    }

    fn evaluate_binary_op_expr(
        &mut self,
        expr: BinaryOpExpression<'gc>,
    ) -> Result<LazyRValue<'gc>, CodegenError> {
        let mut op = expr.op;
        if op == BinaryOp::Concat {
            todo!("concat")
        }

        let mut lhs = self.evaluate_expr(*expr.lhs)?;

        let op_is_shortcircuit = matches!(op, BinaryOp::And | BinaryOp::Or);
        if op_is_shortcircuit {
            return Ok(LazyRValue::ShortCircuit {
                op,
                lhs: lhs.into(),
                rhs: *expr.rhs,
            });
        }

        let mut rhs = self.evaluate_expr(*expr.rhs)?;

        let op_is_commutative = matches!(
            op,
            BinaryOp::Add | BinaryOp::Mul | BinaryOp::Eq | BinaryOp::Ne
        );
        let op_is_anticommutative = matches!(
            op,
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge
        );

        let mut flipped = false;
        if op_is_commutative || op_is_anticommutative {
            let should_flip = match (&lhs, &rhs) {
                (
                    LazyRValue::Constant(Value::Integer(_)),
                    LazyRValue::Constant(Value::Integer(_)),
                ) => false,
                (LazyRValue::Constant(Value::Integer(_)), LazyRValue::Constant(_)) => true,
                (LazyRValue::Constant(_), LazyRValue::Constant(_)) => false,
                (LazyRValue::Constant(_), _) => true,
                _ => false,
            };
            if should_flip {
                match op {
                    BinaryOp::Lt => op = BinaryOp::Ge,
                    BinaryOp::Le => op = BinaryOp::Gt,
                    BinaryOp::Gt => op = BinaryOp::Le,
                    BinaryOp::Ge => op = BinaryOp::Lt,
                    _ => (),
                };
                std::mem::swap(&mut lhs, &mut rhs);
                flipped = true;
            }
        }

        let op_is_comparison = matches!(
            op,
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge | BinaryOp::Eq | BinaryOp::Ne
        );
        if op_is_comparison {
            return Ok(LazyRValue::Comparison {
                op,
                lhs: lhs.into(),
                rhs: rhs.into(),
            });
        }

        Ok(LazyRValue::BinaryOp {
            op,
            lhs: lhs.into(),
            rhs: rhs.into(),
            flipped,
        })
    }

    fn resolve_expr(&mut self, expr: Expression<'gc>) -> Result<LazyLValue, CodegenError> {
        if let Expression::Suffixed(suffixed) = expr {
            self.resolve_suffixed_expr(suffixed)
        } else {
            let rvalue = self.evaluate_expr(expr)?;
            self.wrap_rvalue(rvalue)
        }
    }

    fn resolve_suffixed_expr(
        &mut self,
        suffixed: SuffixedExpression<'gc>,
    ) -> Result<LazyLValue, CodegenError> {
        let mut lvalue = self.resolve_primary(suffixed.primary)?;
        for suffix in suffixed.suffixes {
            lvalue = match suffix {
                Suffix::Field(field) => self.resolve_table_field(lvalue, field)?,
                Suffix::Index(index) => self.resolve_table_index(lvalue, index)?,
                Suffix::FunctionCall { args } => {
                    let dest = self.allocate_register()?;
                    self.emit_func_call(lvalue, args, dest)?;
                    dest.into()
                }
                Suffix::MethodCall { name, args } => {
                    let dest = self.allocate_register()?;
                    self.emit_method_call(lvalue, name, args, dest)?;
                    dest.into()
                }
            };
        }
        Ok(lvalue)
    }

    fn resolve_primary(&mut self, primary: Primary<'gc>) -> Result<LazyLValue, CodegenError> {
        match primary {
            Primary::Name(name) => self.resolve_name(name),
            Primary::Expression(expr) => self.resolve_expr(*expr),
        }
    }

    fn resolve_variable(&mut self, variable: Variable<'gc>) -> Result<LazyLValue, CodegenError> {
        match variable {
            Variable::Name(name) => self.resolve_name(name),
            Variable::TableIndex { table, index } => {
                let table = self.resolve_suffixed_expr(table)?;
                self.resolve_table_index(table, index)
            }
            Variable::Field { table, field } => {
                let table = self.resolve_suffixed_expr(table)?;
                self.resolve_table_field(table, field)
            }
        }
    }

    fn resolve_table_index(
        &mut self,
        table: impl Into<LazyLValue>,
        index: Expression<'gc>,
    ) -> Result<LazyLValue, CodegenError> {
        let index = self.evaluate_expr(index)?;
        let table = self.force_lvalue(table)?;
        let indexed = match table {
            LValue::Register(table) => match index {
                LazyRValue::Constant(Value::Integer(i)) if 0 <= i && i <= u8::MAX as Integer => {
                    LazyLValue::TableIntegerKey {
                        table,
                        key: i as u8,
                    }
                }
                string @ LazyRValue::Constant(Value::String(_)) => {
                    match self.discharge_to_rk(string)? {
                        RkIndex::Register(key) => LazyLValue::TableGenericKey { table, key },
                        RkIndex::Constant(field) => LazyLValue::TableField { table, field },
                    }
                }
                key => {
                    let key = self.discharge_to_any_register(key)?;
                    LazyLValue::TableGenericKey { table, key }
                }
            },
            LValue::Upvalue(upvalue) => match self.discharge_to_rk(index)? {
                RkIndex::Register(key) => {
                    let table = self.allocate_register()?;
                    self.emit(IrInstruction::GetUpvalue {
                        dest: table,
                        upvalue,
                    });
                    LazyLValue::TableGenericKey { table, key }
                }
                RkIndex::Constant(field) => LazyLValue::UpvalueField {
                    table: upvalue,
                    field,
                },
            },
        };
        Ok(indexed)
    }

    fn resolve_table_field(
        &mut self,
        table: impl Into<LazyLValue>,
        field: LuaString<'gc>,
    ) -> Result<LazyLValue, CodegenError> {
        let field = LazyRValue::Constant(field.into());
        let table = self.force_lvalue(table)?;
        let result = match table {
            LValue::Register(table) => match self.discharge_to_rk(field)? {
                RkIndex::Register(key) => LazyLValue::TableGenericKey { table, key },
                RkIndex::Constant(field) => LazyLValue::TableField { table, field },
            },
            LValue::Upvalue(upvalue) => match self.discharge_to_rk(field)? {
                RkIndex::Register(key) => {
                    let table = self.allocate_register()?;
                    self.emit(IrInstruction::GetUpvalue {
                        dest: table,
                        upvalue,
                    });
                    LazyLValue::TableGenericKey { table, key }
                }
                RkIndex::Constant(field) => LazyLValue::UpvalueField {
                    table: upvalue,
                    field,
                },
            },
        };
        Ok(result)
    }
}
