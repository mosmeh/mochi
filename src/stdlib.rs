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

pub fn load<'gc>(gc: &'gc GcContext, global_table: GcCell<'gc, Table<'gc>>) {
    base::load(gc, global_table);
    package::load(gc, global_table);
    string::load(gc, global_table);
    table::load(gc, global_table);
    math::load(gc, global_table);
    io::load(gc, global_table);
    os::load(gc, global_table);
}
