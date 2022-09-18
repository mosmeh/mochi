use super::helpers::{set_functions_to_table, StackExt};
use crate::{
    gc::{GcCell, GcContext},
    runtime::{CoroutineResult, ErrorKind, Vm},
    types::{Action, LuaThread, NativeClosure, NativeClosureFn, StackWindow, Table, ThreadStatus},
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
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(&window);

    let co = stack.arg(0);
    let co = co.as_thread()?;
    let status = get_coroutine_status(thread, co);
    if !matches!(status, CoroutineStatus::Dead | CoroutineStatus::Suspended) {
        return Err(ErrorKind::ExplicitError(format!(
            "cannot close a {} coroutine",
            status
        )));
    }

    let mut coroutine_ref = co.borrow_mut(gc);
    match &coroutine_ref.status {
        ThreadStatus::Resumable | ThreadStatus::Unresumable => {
            stack[0] = true.into();
            coroutine_ref.close();
            Ok(Action::Return { num_results: 1 })
        }
        ThreadStatus::Error(err) => {
            stack[0] = false.into();
            stack[1] = gc.allocate_string(err.to_string().into_bytes()).into();
            coroutine_ref.close();
            Ok(Action::Return { num_results: 2 })
        }
    }
}

fn coroutine_create<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let f = stack.arg(0).ensure_function()?;
    let co = LuaThread::with_body(f);

    stack[0] = gc.allocate_cell(co).into();
    Ok(Action::Return { num_results: 1 })
}

fn coroutine_isyieldable<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(&window);

    let co = stack.arg(0);
    let co = if co.get().is_some() {
        co.as_thread()?
    } else {
        thread
    };
    let is_main_thread = GcCell::ptr_eq(&co, &vm.main_thread());

    stack[0] = (!is_main_thread).into();
    Ok(Action::Return { num_results: 1 })
}

fn coroutine_resume<'gc>(
    _: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);
    stack.arg(0).as_thread()?;

    Ok(Action::Resume {
        coroutine_bottom: 1,
        num_values: stack.args().len() - 1,
        continuation: Box::new(|gc, _, thread, mut window| {
            let mut thread = thread.borrow_mut(gc);
            let result = thread
                .stack(&window)
                .arg(0)
                .as_userdata::<CoroutineResult>()?;
            let result = result.borrow();
            match result.get::<CoroutineResult>().unwrap() {
                Ok(()) => {
                    let stack = thread.stack_mut(&window);
                    stack[0] = true.into();
                    stack.copy_within(2.., 1);
                    Ok(Action::Return {
                        num_results: stack.args().len(),
                    })
                }
                Err(err) => {
                    thread.ensure_stack(&mut window, 2);
                    let stack = thread.stack_mut(&window);
                    stack[0] = false.into();
                    stack[1] = gc.allocate_string(err.to_string().into_bytes()).into();
                    Ok(Action::Return { num_results: 2 })
                }
            }
        }),
    })
}

fn coroutine_running<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    thread_ref.ensure_stack(&mut window, 2);
    let stack = thread_ref.stack_mut(&window);
    stack[0] = thread.into();
    stack[1] = GcCell::ptr_eq(&thread, &vm.main_thread()).into();
    Ok(Action::Return { num_results: 2 })
}

fn coroutine_status<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let co = thread.borrow().stack(&window).arg(0);
    let co = co.as_thread()?;
    let status = get_coroutine_status(thread, co);
    thread.borrow_mut(gc).stack_mut(&window)[0] =
        gc.allocate_string(status.name().as_bytes()).into();
    Ok(Action::Return { num_results: 1 })
}

fn coroutine_wrap<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let f = stack.arg(0).ensure_function()?;
    let co = LuaThread::with_body(f);

    let wrapper = NativeClosure::with_upvalues(
        |gc, _, thread, mut window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack_mut(&window);

            let num_args = stack.args().len();
            let closure = stack.callee();
            let closure = closure.as_native_closure().unwrap();
            let coroutine = closure.upvalues().first().unwrap();

            thread.ensure_stack(&mut window, num_args + 2);
            let stack = thread.stack_mut(&window);
            stack.copy_within(1..1 + num_args, 2);

            stack[1] = *coroutine;
            let continuation: Box<NativeClosureFn> = Box::new(|gc, _, thread, window| {
                let mut thread = thread.borrow_mut(gc);
                let stack = thread.stack(&window);

                let result = stack.arg(0).as_userdata::<CoroutineResult>()?;
                let result = result.borrow();

                match result.get::<CoroutineResult>().unwrap() {
                    Ok(()) => {
                        let stack = thread.stack_mut(&window);
                        stack.copy_within(2.., 0);
                        Ok(Action::Return {
                            num_results: stack.len() - 2,
                        })
                    }
                    Err(err) => {
                        let closure = stack.callee();
                        let closure = closure.as_native_closure().unwrap();
                        closure
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
                coroutine_bottom: 1,
                num_values: num_args,
                continuation,
            })
        },
        vec![gc.allocate_cell(co).into()],
    );

    stack[0] = gc.allocate(wrapper).into();
    Ok(Action::Return { num_results: 1 })
}

fn coroutine_yield<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);
    stack.copy_within(1.., 0);
    Ok(Action::Yield {
        num_values: stack.args().len(),
    })
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
