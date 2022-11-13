/*impl<'gc> Vm<'gc> {
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
                self.call_value(gc, &mut thread_ref, bottom)?;
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
                self.call_value(gc, &mut thread_ref, bottom)?;
            }
            Action::TailCall { callee, mut args } => {
                thread_ref.frames.pop().unwrap();
                thread_ref.stack.truncate(bottom);
                thread_ref.stack.push(callee);
                thread_ref.stack.append(&mut args);
                self.call_value(gc, &mut thread_ref, bottom)?;
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
                        match thread.borrow_mut(gc).frames.as_mut_slice() {
                            [.., Frame::ResumeContinuation(frame)] => {
                                frame.continuation.as_mut().unwrap().set_args(Err(err))
                            }
                            _ => unreachable!(),
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
                match resumer_ref.frames.as_mut_slice() {
                    [.., Frame::ResumeContinuation(frame)] => {
                        frame.continuation.as_mut().unwrap().set_args(Ok(values))
                    }
                    _ => unreachable!(),
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
*/
