use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{CoroutineResult, ErrorKind, Vm},
    types::{Action, LuaThread, NativeClosure, NativeClosureFn, StackWindow, Table, ThreadStatus},
};
use bstr::B;
use std::ops::Deref;

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
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let co = thread.borrow().stack(&window).arg(1).as_thread()?;
    let status = get_coroutine_status(thread, co);
    if !matches!(status, CoroutineStatus::Dead | CoroutineStatus::Suspended) {
        return Err(ErrorKind::ExplicitError(format!(
            "cannot close a {} coroutine",
            status
        )));
    }

    let mut co = co.borrow_mut(gc);
    Ok(Action::Return(match &co.status {
        ThreadStatus::Resumable | ThreadStatus::Unresumable => {
            co.close();
            vec![true.into()]
        }
        ThreadStatus::Error(err) => {
            let msg = gc.allocate_string(err.to_string().into_bytes()).into();
            co.close();
            vec![false.into(), msg]
        }
    }))
}

fn coroutine_create<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let f = thread.borrow().stack(&window).arg(1).ensure_function()?;
    let co = LuaThread::with_body(f);

    Ok(Action::Return(vec![gc.allocate_cell(co).into()]))
}

fn coroutine_isyieldable<'gc>(
    _: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let co = thread.borrow().stack(&window).arg(1);
    let co = if co.get().is_some() {
        co.as_thread()?
    } else {
        thread
    };
    let is_main_thread = GcCell::ptr_eq(&co, &vm.main_thread());

    Ok(Action::Return(vec![(!is_main_thread).into()]))
}

fn coroutine_resume<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack(&window);
    let coroutine = stack.arg(1).as_thread()?;
    let args = stack.args()[1..].to_vec();
    thread.resize_stack(&mut window, 0);

    Ok(Action::Resume {
        coroutine,
        args,
        continuation: Box::new(|gc, _, thread, window| {
            let thread = thread.borrow();
            let stack = thread.stack(&window);
            let result = stack.arg(0);
            let result = result.borrow_as_userdata::<CoroutineResult>()?;
            Ok(Action::Return(match result.deref() {
                Ok(()) => {
                    let mut results = vec![true.into()];
                    results.extend_from_slice(&stack[1..]);
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
    thread: GcCell<'gc, LuaThread<'gc>>,
    _: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    Ok(Action::Return(vec![
        thread.into(),
        GcCell::ptr_eq(&thread, &vm.main_thread()).into(),
    ]))
}

fn coroutine_status<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let co = thread.borrow().stack(&window).arg(1).as_thread()?;
    let status = get_coroutine_status(thread, co);
    Ok(Action::Return(vec![gc
        .allocate_string(status.name().as_bytes())
        .into()]))
}

fn coroutine_wrap<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let f = thread.borrow().stack(&window).arg(1).ensure_function()?;
    let co = LuaThread::with_body(f);

    let wrapper = NativeClosure::with_upvalues(
        |_, _, thread, window| {
            let thread = thread.borrow();
            let stack = thread.stack(&window);

            let closure = stack.arg(0);
            let closure = closure.as_native_closure()?;
            let coroutine = closure.upvalues().first().unwrap().as_thread().unwrap();

            let continuation: Box<NativeClosureFn> = Box::new(|gc, _, thread, window| {
                let thread = thread.borrow();
                let stack = thread.stack(&window);

                let result = stack.arg(1);
                let result = result.borrow_as_userdata::<CoroutineResult>()?;
                match result.deref() {
                    Ok(()) => Ok(Action::Return(stack.args()[1..].to_vec())),
                    Err(err) => {
                        stack
                            .arg(0)
                            .as_native_closure()?
                            .upvalues()
                            .first()
                            .unwrap()
                            .borrow_as_thread_mut(gc)
                            .unwrap()
                            .close();
                        Err(err.clone())
                    }
                }
            });
            Ok(Action::Resume {
                coroutine,
                args: stack.args().to_vec(),
                continuation,
            })
        },
        vec![gc.allocate_cell(co).into()],
    );

    Ok(Action::Return(vec![gc.allocate(wrapper).into()]))
}

fn coroutine_yield<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    Ok(Action::Yield(
        thread.borrow().stack(&window).args().to_vec(),
    ))
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
