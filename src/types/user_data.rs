use super::Table;
use crate::gc::{GarbageCollect, GcCell, GcLifetime, Tracer};
use std::any::Any;

#[derive(Debug)]
pub struct UserData<'gc, 'a> {
    data: Box<dyn Any>,
    metatable: Option<GcCell<'gc, 'a, Table<'gc, 'a>>>,
}

unsafe impl GarbageCollect for UserData<'_, '_> {
    fn trace(&self, tracer: &mut Tracer) {
        self.metatable.trace(tracer);
    }
}

unsafe impl<'a, 'gc: 'a> GcLifetime<'gc, 'a> for UserData<'gc, '_> {
    type Aged = UserData<'gc, 'a>;
}

impl<'gc, 'a> UserData<'gc, 'a> {
    pub fn new<T: Any>(data: T) -> Self {
        Self {
            data: Box::new(data),
            metatable: None,
        }
    }

    pub fn is<T: Any>(&self) -> bool {
        self.data.is::<T>()
    }

    pub fn get<T: Any>(&self) -> Option<&T> {
        self.data.downcast_ref()
    }

    pub fn get_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.data.downcast_mut()
    }

    pub fn metatable(&self) -> Option<GcCell<'gc, 'a, Table<'gc, 'a>>> {
        self.metatable
    }

    pub fn set_metatable<T>(&mut self, metatable: T)
    where
        T: Into<Option<GcCell<'gc, 'a, Table<'gc, 'a>>>>,
    {
        self.metatable = metatable.into();
    }
}
