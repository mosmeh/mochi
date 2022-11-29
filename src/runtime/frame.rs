#[derive(Debug)]
pub(crate) enum Frame {
    Lua(LuaFrame),
    Native,
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

/*let mut current_frame = thread_ref.frames.pop();
let (bottom, result) = match &mut current_frame {
    Some(Frame::Lua(_)) => unreachable!(),
    Some(Frame::Native { bottom, .. }) => {
        let bottom = *bottom;
        let callee = thread_ref.stack[bottom];
        let args = thread_ref.stack.split_off(bottom);
        drop(thread_ref);
        let result = match &callee {
            Value::NativeFunction(func) => (func.0)(gc, vm, args),
            Value::NativeClosure(closure) => closure.get(gc).call(gc, vm, args),
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
        (*bottom, continuation.call(gc, vm))
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
        (*bottom, continuation.call(gc, vm))
    }
    Some(Frame::ResumeContinuation(ContinuationFrame {
        bottom,
        continuation,
    })) => {
        drop(thread_ref);
        (*bottom, continuation.take().unwrap().call(gc, vm))
    }
    Some(Frame::MutateGcContinuation(ContinuationFrame {
        bottom,
        continuation,
    })) => {
        drop(thread_ref);
        (*bottom, continuation.take().unwrap().call(gc, vm))
    }
    None => {
        let coroutine = vm.borrow_mut(gc).thread_stack.pop().unwrap();
        debug_assert!(GcCell::ptr_eq(&coroutine, &thread));

        let values = std::mem::take(&mut thread_ref.stack);
        if let Some(coroutine) = vm.borrow(gc).thread_stack.last() {
            match coroutine.borrow_mut(gc).frames.as_mut_slice() {
                [.., Frame::ResumeContinuation(frame)] => {
                    frame.continuation.as_mut().unwrap().set_args(Ok(values))
                }
                _ => unreachable!(),
            }
        }
        return Ok(None);
    }
};

thread.borrow_mut(gc).frames.push(current_frame.unwrap());

match result {
    Ok(action) => vm.borrow_mut(gc).handle_action(gc, action, bottom),
    Err(kind) => Err(kind),
}*/
