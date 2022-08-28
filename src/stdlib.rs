mod base;
mod helpers;
mod io;
mod math;
mod os;
mod package;
mod string;
mod table;

use crate::{
    gc::{GcCell, GcContext},
    types::Table,
};

pub fn load<'gc>(gc: &'gc GcContext, globals: GcCell<'gc, Table<'gc>>) {
    base::load(gc, globals);
    package::load(gc, globals);
    string::load(gc, globals);
    table::load(gc, globals);
    math::load(gc, globals);
    io::load(gc, globals);
    os::load(gc, globals);
}
