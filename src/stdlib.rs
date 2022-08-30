mod base;
mod helpers;
mod io;
mod math;
mod os;
mod package;
mod string;
mod table;

use crate::{gc::GcContext, runtime::Vm};

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) {
    base::load(gc, vm);
    package::load(gc, vm);
    string::load(gc, vm);
    table::load(gc, vm);
    math::load(gc, vm);
    io::load(gc, vm);
    os::load(gc, vm);
}
