use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{Action, Continuation, ErrorKind, Vm},
    types::{LuaThread, NativeClosure, Table, ThreadStatus, Value},
};
use bstr::B;

pub fn load<'gc>(gc: &'gc GcContext, _: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    let mut table = Table::new();
    set_functions_to_table(
        gc,
        &mut table,
        &[
            (B("close"), coroutine_close),
            (B("create"), coroutine_create),
            (B("isyieldable"), coroutine_isyieldable),
            (B("resume"), coroutine_resume),
            (B("running"), coroutine_running),
            (B("status"), coroutine_status),
            (B("wrap"), coroutine_wrap),
            (B("yield"), coroutine_yield),
        ],
    );
    gc.allocate_cell(table)
}

fn coroutine_close<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let co = args.nth(1).as_thread()?;
    let status = get_coroutine_status(vm.current_thread(), co);
    if !matches!(status, CoroutineStatus::Dead | CoroutineStatus::Suspended) {
        return Err(ErrorKind::Other(format!(
            "cannot close a {} coroutine",
            status
        )));
    }

    let mut co = co.borrow_mut(gc);
    Ok(Action::Return(match &co.status {
        ThreadStatus::Resumable | ThreadStatus::Unresumable => {
            co.close(gc);
            vec![true.into()]
        }
        ThreadStatus::Error(err) => {
            let msg = gc.allocate_string(err.to_string().into_bytes()).into();
            co.close(gc);
            vec![false.into(), msg]
        }
    }))
}

fn coroutine_create<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let f = args.nth(1).ensure_function()?;
    let co = create_coroutine(vm, f)?;

    Ok(Action::Return(vec![gc.allocate_cell(co).into()]))
}

fn coroutine_isyieldable<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let co = args.nth(1);
    let co = if co.is_present() {
        co.as_thread()?
    } else {
        vm.current_thread()
    };
    let is_main_thread = GcCell::ptr_eq(&co, &vm.main_thread());

    Ok(Action::Return(vec![(!is_main_thread).into()]))
}

fn coroutine_resume<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let coroutine = args.nth(1).as_thread()?;
    let args = args.without_callee()[1..].to_vec();

    Ok(Action::Resume {
        coroutine,
        args,
        continuation: Continuation::new(|gc, _, result: Result<Vec<Value>, ErrorKind>| {
            Ok(Action::Return(match result {
                Ok(mut results) => {
                    results.insert(0, true.into());
                    results
                }
                Err(err) => vec![
                    false.into(),
                    gc.allocate_string(err.to_string().into_bytes()).into(),
                ],
            }))
        }),
    })
}

fn coroutine_running<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    _: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = vm.current_thread();
    Ok(Action::Return(vec![
        thread.into(),
        GcCell::ptr_eq(&thread, &vm.main_thread()).into(),
    ]))
}

fn coroutine_status<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let co = args.nth(1).as_thread()?;
    let status = get_coroutine_status(vm.current_thread(), co);
    Ok(Action::Return(vec![gc
        .allocate_string(status.name().as_bytes())
        .into()]))
}

fn coroutine_wrap<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    let f = args.nth(1).ensure_function()?;
    let co = create_coroutine(vm, f)?;
    let coroutine = gc.allocate_cell(co);

    let wrapper = NativeClosure::with_upvalue(coroutine, |_, _, &coroutine, args| {
        Ok(Action::Resume {
            coroutine,
            args: args.without_callee().to_vec(),
            continuation: Continuation::with_context(coroutine, |gc, _, coroutine, result| {
                match result {
                    Ok(results) => Ok(Action::Return(results)),
                    Err(err) => {
                        coroutine.borrow_mut(gc).close(gc);
                        Err(err)
                    }
                }
            }),
        })
    });

    Ok(Action::Return(vec![gc.allocate(wrapper).into()]))
}

fn coroutine_yield<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    args: Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind> {
    Ok(Action::Yield(args.without_callee().to_vec()))
}

fn create_coroutine<'gc>(vm: &mut Vm<'gc>, body: Value<'gc>) -> Result<LuaThread<'gc>, ErrorKind> {
    let mut co = LuaThread::new();
    co.stack.push(body);
    vm.push_frame(&mut co, 0)?;
    Ok(co)
}

enum CoroutineStatus {
    Running,
    Suspended,
    Normal,
    Dead,
}

impl std::fmt::Display for CoroutineStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl CoroutineStatus {
    fn name(&self) -> &'static str {
        match self {
            Self::Running => "running",
            Self::Suspended => "suspended",
            Self::Normal => "normal",
            Self::Dead => "dead",
        }
    }
}

fn get_coroutine_status<'gc>(
    thread: GcCell<'gc, LuaThread<'gc>>,
    coroutine: GcCell<'gc, LuaThread<'gc>>,
) -> CoroutineStatus {
    if GcCell::ptr_eq(&coroutine, &thread) {
        return CoroutineStatus::Running;
    }
    let coroutine_ref = coroutine.borrow();
    match coroutine_ref.status {
        ThreadStatus::Resumable => CoroutineStatus::Suspended,
        ThreadStatus::Unresumable => {
            if coroutine_ref.frames.is_empty() {
                CoroutineStatus::Dead
            } else {
                CoroutineStatus::Normal
            }
        }
        ThreadStatus::Error(_) => CoroutineStatus::Dead,
    }
}
