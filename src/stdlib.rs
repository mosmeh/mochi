mod base;
mod coroutine;
mod file;
mod helpers;
mod io;
mod math;
mod os;
mod package;
mod process;
mod string;
mod table;
mod utf8;

use crate::{
    gc::{GcCell, GcContext},
    runtime::Vm,
    types::Table,
};
use bstr::B;

const LUA_LOADED_TABLE: &[u8] = b"_LOADED";
const LUA_PRELOAD_TABLE: &[u8] = b"_PRELOAD";

pub fn load<'gc, 'a>(gc: &'a GcContext<'gc>, vm: &mut Vm<'gc, 'a>) {
    let loaded = gc.allocate_cell(Table::new());
    vm.registry()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(LUA_LOADED_TABLE), loaded);

    type LoadFn =
        for<'gc, 'a> fn(&'a GcContext<'gc>, &mut Vm<'gc, 'a>) -> GcCell<'gc, 'a, Table<'gc, 'a>>;

    let libs: &[(_, LoadFn)] = &[
        (B("_G"), base::load),
        (B("coroutine"), coroutine::load),
        (B("package"), package::load),
        (B("string"), string::load),
        (B("utf8"), utf8::load),
        (B("table"), table::load),
        (B("math"), math::load),
        (B("io"), io::load),
        (B("os"), os::load),
    ];

    for (name, load_lib) in libs {
        let table = load_lib(gc, vm);
        let name = gc.allocate_string(*name);
        loaded.borrow_mut(gc).set_field(name, table);
        vm.globals().borrow_mut(gc).set_field(name, table);
    }
}
