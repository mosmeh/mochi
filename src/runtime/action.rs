use super::{frame::ContinuationFrame, ErrorKind, Frame, RuntimeAction, Vm};
use crate::{
    gc::{GarbageCollect, GcCell, GcContext, GcHeap, Tracer},
    types::{LuaThread, ThreadStatus, Value},
};

pub enum Action<'gc> {
    Call {
        callee: Value<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Continuation<'gc, Vec<Value<'gc>>>,
    },
    ProtectedCall {
        callee: Value<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Continuation<'gc, Result<Vec<Value<'gc>>, ErrorKind>>,
    },
    TailCall {
        callee: Value<'gc>,
        args: Vec<Value<'gc>>,
    },
    Return(Vec<Value<'gc>>),
    ReturnArguments,
    Resume {
        coroutine: GcCell<'gc, LuaThread<'gc>>,
        args: Vec<Value<'gc>>,
        continuation: Continuation<'gc, Result<Vec<Value<'gc>>, ErrorKind>>,
    },
    Yield(Vec<Value<'gc>>),
    MutateGc {
        mutator: Box<dyn Fn(&mut GcHeap)>,
        continuation: Continuation<'gc, ()>,
    },
}

trait ContinuationFn<'gc, T>: GarbageCollect {
    fn call(&mut self, gc: &'gc GcContext, vm: &mut Vm<'gc>) -> Result<Action<'gc>, ErrorKind>;
    fn args(&self) -> Option<&T>;
    fn set_args(&mut self, args: T);
}

pub struct Continuation<'gc, T>(Box<dyn ContinuationFn<'gc, T> + 'gc>);

unsafe impl<R> GarbageCollect for Continuation<'_, R> {
    fn trace(&self, tracer: &mut Tracer) {
        self.0.trace(tracer);
    }
}

impl<'gc, T: 'gc> Continuation<'gc, T> {
    pub fn new<F>(f: F) -> Self
    where
        F: 'static + Fn(&'gc GcContext, &mut Vm<'gc>, T) -> Result<Action<'gc>, ErrorKind>,
    {
        struct SimpleContinuation<R, F> {
            args: Option<R>,
            f: F,
        }

        impl<'gc, T, F> ContinuationFn<'gc, T> for SimpleContinuation<T, F>
        where
            F: Fn(&'gc GcContext, &mut Vm<'gc>, T) -> Result<Action<'gc>, ErrorKind>,
        {
            fn call(
                &mut self,
                gc: &'gc GcContext,
                vm: &mut Vm<'gc>,
            ) -> Result<Action<'gc>, ErrorKind> {
                (self.f)(gc, vm, self.args.take().unwrap())
            }

            fn args(&self) -> Option<&T> {
                self.args.as_ref()
            }

            fn set_args(&mut self, args: T) {
                self.args = Some(args);
            }
        }

        unsafe impl<T, F> GarbageCollect for SimpleContinuation<T, F> {
            fn needs_trace() -> bool {
                false
            }
        }

        Self(Box::new(SimpleContinuation {
            f: Box::new(f),
            args: None,
        }))
    }

    pub fn with_context<C, F>(context: C, f: F) -> Self
    where
        C: 'gc + GarbageCollect,
        F: 'static + Fn(&'gc GcContext, &mut Vm<'gc>, C, T) -> Result<Action<'gc>, ErrorKind>,
    {
        struct ContextContinuation<C, R, F> {
            context: Option<C>,
            args: Option<R>,
            f: F,
        }

        impl<'gc, C, T, F> ContinuationFn<'gc, T> for ContextContinuation<C, T, F>
        where
            C: 'gc + GarbageCollect,
            F: Fn(&'gc GcContext, &mut Vm<'gc>, C, T) -> Result<Action<'gc>, ErrorKind>,
        {
            fn call(
                &mut self,
                gc: &'gc GcContext,
                vm: &mut Vm<'gc>,
            ) -> Result<Action<'gc>, ErrorKind> {
                (self.f)(
                    gc,
                    vm,
                    self.context.take().unwrap(),
                    self.args.take().unwrap(),
                )
            }

            fn args(&self) -> Option<&T> {
                self.args.as_ref()
            }

            fn set_args(&mut self, result: T) {
                self.args = Some(result);
            }
        }

        unsafe impl<C, T, F> GarbageCollect for ContextContinuation<C, T, F>
        where
            C: GarbageCollect,
        {
            fn needs_trace() -> bool {
                C::needs_trace()
            }

            fn trace(&self, tracer: &mut Tracer) {
                self.context.trace(tracer);
            }
        }

        Self(Box::new(ContextContinuation {
            context: Some(context),
            args: None,
            f,
        }))
    }

    pub(crate) fn call(
        mut self,
        gc: &'gc GcContext,
        vm: &mut Vm<'gc>,
    ) -> Result<Action<'gc>, ErrorKind> {
        self.0.call(gc, vm)
    }

    pub(crate) fn args(&self) -> Option<&T> {
        self.0.args()
    }

    pub(crate) fn set_args(&mut self, args: T) {
        self.0.set_args(args);
    }
}

impl<'gc> Vm<'gc> {
    pub(super) fn handle_action(
        &mut self,
        gc: &'gc GcContext,
        action: Action<'gc>,
        bottom: usize,
    ) -> Result<Option<RuntimeAction>, ErrorKind> {
        let thread = self.current_thread();
        let mut thread_ref = thread.borrow_mut(gc);

        match action {
            Action::Call {
                callee,
                mut args,
                continuation,
            } => {
                thread_ref.stack.truncate(bottom);
                *thread_ref.frames.last_mut().unwrap() = Frame::CallContinuation {
                    inner: ContinuationFrame {
                        bottom,
                        continuation: Some(continuation),
                    },
                    callee_bottom: bottom,
                };
                thread_ref.stack.push(callee);
                thread_ref.stack.append(&mut args);
                thread_ref.push_frame(bottom)?;
            }
            Action::ProtectedCall {
                callee,
                mut args,
                continuation,
            } => {
                thread_ref.stack.truncate(bottom);
                *thread_ref.frames.last_mut().unwrap() = Frame::ProtectedCallContinuation {
                    inner: ContinuationFrame {
                        bottom,
                        continuation: Some(continuation),
                    },
                    callee_bottom: bottom,
                };
                thread_ref.stack.push(callee);
                thread_ref.stack.append(&mut args);
                thread_ref.push_frame(bottom)?;
            }
            Action::TailCall { callee, mut args } => {
                thread_ref.frames.pop().unwrap();
                thread_ref.stack.truncate(bottom);
                thread_ref.stack.push(callee);
                thread_ref.stack.append(&mut args);
                thread_ref.push_frame(bottom)?;
            }
            Action::Return(mut results) => {
                thread_ref.frames.pop().unwrap();
                thread_ref.stack.truncate(bottom);
                thread_ref.stack.append(&mut results);
            }
            Action::ReturnArguments => {
                thread_ref.frames.pop().unwrap();
            }
            Action::Resume {
                coroutine,
                mut args,
                continuation,
            } => {
                thread_ref.stack.truncate(bottom);
                *thread_ref.frames.last_mut().unwrap() =
                    Frame::ResumeContinuation(ContinuationFrame {
                        bottom,
                        continuation: Some(continuation),
                    });
                drop(thread_ref);

                let mut coroutine_ref = coroutine.borrow_mut(gc);
                let resume_result = match coroutine_ref.status {
                    ThreadStatus::Error(_) => Err(ErrorKind::other("cannot resume dead coroutine")),
                    ThreadStatus::Unresumable if coroutine_ref.frames.is_empty() => {
                        Err(ErrorKind::other("cannot resume dead coroutine"))
                    }
                    ThreadStatus::Unresumable => {
                        Err(ErrorKind::other("cannot resume non-suspended coroutine"))
                    }
                    ThreadStatus::Resumable => Ok(()),
                };
                match resume_result {
                    Ok(()) => (),
                    Err(err) => {
                        drop(coroutine_ref);
                        if let Frame::ResumeContinuation(frame) =
                            thread.borrow_mut(gc).frames.last_mut().unwrap()
                        {
                            frame.continuation.as_mut().unwrap().set_args(Err(err));
                        } else {
                            unreachable!()
                        }
                        return Ok(None);
                    }
                }

                self.thread_stack.push(coroutine);
                coroutine_ref.status = ThreadStatus::Unresumable;
                coroutine_ref.stack.append(&mut args);
            }
            Action::Yield(values) => {
                match self.thread_stack.len() {
                    0 => unreachable!(),
                    1 => {
                        return Err(ErrorKind::other(
                            "attempt to yield from outside a coroutine",
                        ))
                    }
                    _ => (),
                }

                let resumer = self.thread_stack.pop().unwrap();
                debug_assert!(GcCell::ptr_eq(&resumer, &thread));

                thread_ref.stack.truncate(bottom);
                thread_ref.frames.pop().unwrap();
                thread_ref.status = ThreadStatus::Resumable;

                let mut resumer_ref = self.thread_stack.last().unwrap().borrow_mut(gc);
                if let Frame::ResumeContinuation(frame) = resumer_ref.frames.last_mut().unwrap() {
                    frame.continuation.as_mut().unwrap().set_args(Ok(values));
                } else {
                    unreachable!()
                }
            }
            Action::MutateGc {
                mutator,
                mut continuation,
            } => {
                thread_ref.stack.truncate(bottom);
                continuation.set_args(());
                *thread_ref.frames.last_mut().unwrap() =
                    Frame::MutateGcContinuation(ContinuationFrame {
                        bottom,
                        continuation: Some(continuation),
                    });
                return Ok(Some(RuntimeAction::MutateGc(mutator)));
            }
        }

        Ok(None)
    }
}
