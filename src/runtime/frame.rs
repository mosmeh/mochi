use super::{Continuation, ErrorKind, Operation, RuntimeAction, Vm};
use crate::{
    gc::{GarbageCollect, GcCell, GcContext, Tracer},
    types::Value,
};

#[derive(Debug)]
pub(crate) enum Frame<'gc> {
    Lua(LuaFrame),
    Native {
        bottom: usize,
    },
    CallContinuation {
        inner: ContinuationFrame<'gc, Vec<Value<'gc>>>,
        callee_bottom: usize,
    },
    ProtectedCallContinuation {
        inner: ContinuationFrame<'gc, Result<Vec<Value<'gc>>, ErrorKind>>,
        callee_bottom: usize,
    },
    ResumeContinuation(ContinuationFrame<'gc, Result<Vec<Value<'gc>>, ErrorKind>>),
    MutateGcContinuation(ContinuationFrame<'gc, ()>),
}

unsafe impl GarbageCollect for Frame<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Self::Lua(_) | Self::Native { .. } => (),
            Self::CallContinuation { inner, .. } => inner.trace(tracer),
            Self::ProtectedCallContinuation { inner, .. } | Self::ResumeContinuation(inner) => {
                inner.trace(tracer)
            }
            Self::MutateGcContinuation(inner) => inner.trace(tracer),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LuaFrame {
    pub bottom: usize,
    pub base: usize,
    pub pc: usize,
    pub num_extra_args: usize,
}

impl LuaFrame {
    pub fn new(bottom: usize) -> Self {
        Self {
            bottom,
            base: bottom + 1,
            pc: 0,
            num_extra_args: 0,
        }
    }
}

pub(crate) struct ContinuationFrame<'gc, R> {
    pub bottom: usize,
    pub continuation: Option<Continuation<'gc, R>>,
}

impl<R> std::fmt::Debug for ContinuationFrame<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContinuationFrame")
            .field("bottom", &self.bottom)
            .finish()
    }
}

unsafe impl<R> GarbageCollect for ContinuationFrame<'_, R> {
    fn trace(&self, tracer: &mut Tracer) {
        self.continuation.trace(tracer);
    }
}

impl<'gc> Vm<'gc> {
    pub(super) fn execute_next_frame(
        &mut self,
        gc: &'gc GcContext,
    ) -> Result<Option<RuntimeAction>, ErrorKind> {
        let thread = self.current_thread();
        let mut thread_ref = thread.borrow_mut(gc);

        if let Some(Frame::Lua(_)) = thread_ref.frames.last() {
            drop(thread_ref);
            while self.execute_lua_frame(gc)?.is_continue() {}
            return Ok(None);
        }

        let mut current_frame = thread_ref.frames.pop();
        let (bottom, result) = match &mut current_frame {
            Some(Frame::Lua(_)) => unreachable!(),
            Some(Frame::Native { bottom, .. }) => {
                let bottom = *bottom;
                let callee = thread_ref.stack[bottom];
                let args = thread_ref.stack.split_off(bottom);
                drop(thread_ref);
                let result = match &callee {
                    Value::NativeFunction(func) => (func.0)(gc, self, args),
                    Value::NativeClosure(closure) => closure.call(gc, self, args),
                    Value::LuaClosure(_) => unreachable!(),
                    value => {
                        return Err(ErrorKind::TypeError {
                            operation: Operation::Call,
                            ty: value.ty(),
                        })
                    }
                };
                (bottom, result)
            }
            Some(Frame::CallContinuation {
                inner:
                    ContinuationFrame {
                        bottom,
                        continuation,
                    },
                callee_bottom,
            }) => {
                let mut continuation = continuation.take().unwrap();
                continuation.set_args(thread_ref.stack.split_off(*callee_bottom));
                drop(thread_ref);
                (*bottom, continuation.call(gc, self))
            }
            Some(Frame::ProtectedCallContinuation {
                inner:
                    ContinuationFrame {
                        bottom,
                        continuation,
                    },
                callee_bottom,
            }) => {
                let mut continuation = continuation.take().unwrap();
                match continuation.args() {
                    Some(Ok(_)) => unreachable!(),
                    Some(Err(_)) => (),
                    None => continuation.set_args(Ok(thread_ref.stack.split_off(*callee_bottom))),
                }
                drop(thread_ref);
                (*bottom, continuation.call(gc, self))
            }
            Some(Frame::ResumeContinuation(ContinuationFrame {
                bottom,
                continuation,
            })) => {
                drop(thread_ref);
                (*bottom, continuation.take().unwrap().call(gc, self))
            }
            Some(Frame::MutateGcContinuation(ContinuationFrame {
                bottom,
                continuation,
            })) => {
                drop(thread_ref);
                (*bottom, continuation.take().unwrap().call(gc, self))
            }
            None => {
                let coroutine = self.thread_stack.pop().unwrap();
                debug_assert!(GcCell::ptr_eq(&coroutine, &thread));

                let values = std::mem::take(&mut thread_ref.stack);
                if let Some(coroutine) = self.thread_stack.last() {
                    if let Frame::ResumeContinuation(frame) =
                        coroutine.borrow_mut(gc).frames.last_mut().unwrap()
                    {
                        frame.continuation.as_mut().unwrap().set_args(Ok(values));
                    } else {
                        unreachable!()
                    }
                }
                return Ok(None);
            }
        };

        thread.borrow_mut(gc).frames.push(current_frame.unwrap());

        match result {
            Ok(action) => self.handle_action(gc, action, bottom),
            Err(kind) => Err(kind),
        }
    }
}
