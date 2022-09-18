mod base;
mod coroutine;
mod helpers;
mod io;
mod math;
mod os;
mod package;
mod string;
mod table;

use crate::{
    gc::{GcCell, GcContext},
    runtime::Vm,
    types::Table,
};
use bstr::B;

const LUA_LOADED_TABLE: &[u8] = b"_LOADED";
const LUA_PRELOAD_TABLE: &[u8] = b"_PRELOAD";

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) {
    let loaded = gc.allocate_cell(Table::new());
    vm.registry()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(LUA_LOADED_TABLE), loaded);

    type LoadFn = for<'a> fn(&'a GcContext, &mut Vm<'a>) -> GcCell<'a, Table<'a>>;

    let libs: &[(_, LoadFn)] = &[
        (B("_G"), base::load),
        (B("coroutine"), coroutine::load),
        (B("package"), package::load),
        (B("string"), string::load),
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
