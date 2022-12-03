use super::helpers::{set_functions_to_table, ArgumentsExt};
use crate::{
    gc::{GcCell, GcContext, RootSet},
    runtime::{ErrorKind, Vm},
    types::{LuaThread, NativeClosure, Table, ThreadStatus, Value},
};
use bstr::B;

pub fn load<'gc, 'a>(
    gc: &'a GcContext<'gc>,
    _: &mut Vm<'gc, 'a>,
) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
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

fn coroutine_close<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let co = args.nth(1).as_thread()?;
    let status = get_coroutine_status(gc, vm.borrow(gc).current_thread(), co);
    if !matches!(status, CoroutineStatus::Dead | CoroutineStatus::Suspended) {
        return Err(ErrorKind::Other(format!(
            "cannot close a {} coroutine",
            status
        )));
    }

    let mut co = co.borrow_mut(gc);
    Ok(match &co.status {
        ThreadStatus::Resumable | ThreadStatus::Unresumable => {
            co.close(gc);
            vec![true.into()]
        }
        ThreadStatus::Error(err) => {
            let msg = gc.allocate_string(err.to_string().into_bytes()).into();
            co.close(gc);
            vec![false.into(), msg]
        }
    })
}

fn coroutine_create<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let f = args.nth(1).ensure_function()?;
    let co = create_coroutine(gc, vm, f)?;

    Ok(vec![gc.allocate_cell(co).into()])
}

fn coroutine_isyieldable<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let co = args.nth(1);
    let co = if co.is_none() {
        vm.borrow(gc).current_thread()
    } else {
        co.as_thread()?
    };
    let is_main_thread = GcCell::ptr_eq(co, vm.borrow(gc).main_thread());

    Ok(vec![(!is_main_thread).into()])
}

fn coroutine_resume<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let coroutine = args.nth(1).as_thread()?;
    let args = args.without_callee()[1..].to_vec();

    /*Ok(Action::Resume {
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
    })*/
    todo!()
}

fn coroutine_running<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    _: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let thread = vm.borrow(gc).current_thread();
    Ok(vec![
        thread.into(),
        GcCell::ptr_eq(thread, vm.borrow(gc).main_thread()).into(),
    ])
}

fn coroutine_status<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, 'a>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let co = args.nth(1).as_thread()?;
    let status = get_coroutine_status(gc, vm.borrow(gc).current_thread(), co);
    Ok(vec![gc.allocate_string(status.name().as_bytes()).into()])
}

fn coroutine_wrap<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let f = args.nth(1).ensure_function()?;
    let co = create_coroutine(gc, vm, f)?;
    let coroutine = gc.allocate_cell(co);

    /*let wrapper = NativeClosure::with_upvalue(coroutine, |_, _, &coroutine, args| {
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

    Ok(vec![gc.allocate(wrapper).into()])*/
    todo!()
}

fn coroutine_yield<'gc, 'a>(
    _: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    //Ok(Action::Yield(args.without_callee().to_vec()))
    todo!()
}

fn create_coroutine<'gc, 'a>(
    gc: &GcContext<'gc>,
    vm: GcCell<Vm>,
    body: Value<'gc, '_>,
) -> Result<LuaThread<'gc, 'a>, ErrorKind> {
    let mut co = LuaThread::new();
    co.stack.push(body);
    //vm.push_frame(gc, &mut co, 0)?;
    todo!();
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

fn get_coroutine_status<'gc, 'a>(
    gc: &GcContext<'gc>,
    thread: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
    coroutine: GcCell<'gc, 'a, LuaThread<'gc, 'a>>,
) -> CoroutineStatus {
    if GcCell::ptr_eq(coroutine, thread) {
        return CoroutineStatus::Running;
    }
    let coroutine_ref = coroutine.borrow(gc);
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
