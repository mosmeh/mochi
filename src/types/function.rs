use super::LuaThread;
use crate::{
    gc::{GarbageCollect, Gc, GcCell, GcContext, GcHeap, Tracer},
    runtime::{ErrorKind, Instruction, Vm},
    types::{LuaString, Value},
};
use std::{fmt::Debug, hash::Hash, ops::RangeInclusive};

pub enum Action<'gc> {
    Call {
        callee: Value<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Continuation<'gc, Vec<Value<'gc>>>,
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

pub type NativeFunctionPtr =
    for<'gc> fn(&'gc GcContext, &mut Vm<'gc>, Vec<Value<'gc>>) -> Result<Action<'gc>, ErrorKind>;

#[derive(Clone, Copy)]
pub struct NativeFunction(pub NativeFunctionPtr);

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NativeFunction")
            .field(&self.as_ptr())
            .finish()
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl Hash for NativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}

impl NativeFunction {
    pub fn new(x: NativeFunctionPtr) -> Self {
        Self(x)
    }

    pub fn as_ptr(&self) -> *const () {
        self.0 as *const ()
    }
}

#[derive(Debug, Clone)]
pub struct LuaClosureProto<'gc> {
    pub max_stack_size: u8,
    pub lines_defined: LineRange,
    pub constants: Box<[Value<'gc>]>,
    pub code: Box<[Instruction]>,
    pub protos: Box<[Gc<'gc, LuaClosureProto<'gc>>]>,
    pub upvalues: Box<[UpvalueDescription]>,
    pub source: LuaString<'gc>,
}

unsafe impl GarbageCollect for LuaClosureProto<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.constants.trace(tracer);
        self.protos.trace(tracer);
        self.source.trace(tracer);
    }
}

#[derive(Debug, Clone)]
pub enum LineRange {
    File,
    Lines(RangeInclusive<u32>),
}

#[derive(Debug, Clone)]
pub struct LuaClosure<'gc> {
    pub proto: Gc<'gc, LuaClosureProto<'gc>>,
    pub upvalues: Vec<GcCell<'gc, Upvalue<'gc>>>,
}

unsafe impl GarbageCollect for LuaClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.proto.trace(tracer);
        self.upvalues.trace(tracer);
    }
}

impl<'gc> From<Gc<'gc, LuaClosureProto<'gc>>> for LuaClosure<'gc> {
    fn from(proto: Gc<'gc, LuaClosureProto<'gc>>) -> Self {
        Self {
            proto,
            upvalues: Default::default(),
        }
    }
}

pub type NativeClosureFn = dyn for<'gc> Fn(
    &'gc GcContext,
    &mut Vm<'gc>,
    Vec<Value<'gc>>,
) -> Result<Action<'gc>, ErrorKind>;

pub struct NativeClosure<'gc> {
    func: Box<NativeClosureFn>,
    upvalues: Box<[Value<'gc>]>,
}

impl std::fmt::Debug for NativeClosure<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeClosure")
            .field("upvalues", &self.upvalues)
            .finish()
    }
}

unsafe impl GarbageCollect for NativeClosure<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.upvalues.trace(tracer);
    }
}

impl<'gc> NativeClosure<'gc> {
    pub fn new<T>(func: T) -> Self
    where
        T: 'static
            + for<'a> Fn(&'a GcContext, &mut Vm<'a>, Vec<Value<'a>>) -> Result<Action<'a>, ErrorKind>,
    {
        Self {
            func: Box::new(func),
            upvalues: Default::default(),
        }
    }

    pub fn with_upvalues<T, U>(func: T, upvalues: U) -> Self
    where
        T: 'static
            + for<'a> Fn(&'a GcContext, &mut Vm<'a>, Vec<Value<'a>>) -> Result<Action<'a>, ErrorKind>,
        U: Into<Box<[Value<'gc>]>>,
    {
        Self {
            func: Box::new(func),
            upvalues: upvalues.into(),
        }
    }

    pub fn function(&self) -> &NativeClosureFn {
        &self.func
    }

    pub fn upvalues(&self) -> &[Value<'gc>] {
        &self.upvalues
    }
}

#[derive(Debug, Clone)]
pub enum Upvalue<'gc> {
    Open(usize),
    Closed(Value<'gc>),
}

impl<'gc> From<Value<'gc>> for Upvalue<'gc> {
    fn from(x: Value<'gc>) -> Self {
        Self::Closed(x)
    }
}

unsafe impl GarbageCollect for Upvalue<'_> {
    fn trace(&self, tracer: &mut Tracer) {
        if let Self::Closed(x) = self {
            x.trace(tracer);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpvalueDescription {
    Register(RegisterIndex),
    Upvalue(UpvalueIndex),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RegisterIndex(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueIndex(pub u8);

trait ContinuationFn<'gc, T>: GarbageCollect {
    fn call(&mut self, gc: &'gc GcContext, vm: &mut Vm<'gc>) -> Result<Action<'gc>, ErrorKind>;
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

    pub(crate) fn set_args(&mut self, args: T) {
        self.0.set_args(args);
    }
}
