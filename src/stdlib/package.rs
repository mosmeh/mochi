use super::helpers::StackExt;
use crate::{
    gc::{root_gc, root_gc_cell, GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{LuaClosure, LuaThread, NativeFunction, StackWindow, Table, Value},
};
use bstr::{ByteSlice, B};

const LUA_LOADED_TABLE: &[u8] = b"_LOADED";

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) {
    let globals = vm.globals();
    let mut globals = globals.borrow_mut(gc);
    globals.set_field(
        gc.allocate_string(B("require")),
        NativeFunction::new(require),
    );

    let loaded = gc.allocate_cell(Table::new());
    vm.registry()
        .borrow_mut(gc)
        .set_field(gc.allocate_string(LUA_LOADED_TABLE), loaded);

    let mut package = Table::new();
    package.set_field(gc.allocate_string(B("loaded")), loaded);
    globals.set_field(gc.allocate_string(B("package")), gc.allocate_cell(package));
}

fn require<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(&window);

    let module_name = stack.arg(0);
    let module_name = module_name.to_string()?;
    let module_name = gc.allocate_string(module_name);

    let loaded = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(LUA_LOADED_TABLE))
        .as_table()
        .unwrap();
    let value = loaded.borrow().get_field(module_name);

    if !value.is_nil() {
        stack[0] = value;
        return Ok(1);
    }
    drop(thread_ref);

    let filename = format!("./{}.lua", module_name.as_bstr());
    let lua_filename = gc.allocate_string(filename.clone().into_bytes());

    let proto = match crate::load_file(gc, &filename) {
        Ok(proto) => proto,
        Err(err) => return Err(ErrorKind::ExplicitError(err.to_string())),
    };
    let mut closure = LuaClosure::from(gc.allocate(proto));
    closure
        .upvalues
        .push(gc.allocate_cell(Value::Table(vm.globals()).into()));

    root_gc_cell!(gc, loaded);
    root_gc!(gc, module_name.0);
    root_gc!(gc, lua_filename.0);

    let value = unsafe {
        vm.execute_value(
            gc,
            thread,
            gc.allocate(closure).into(),
            &[module_name.into(), lua_filename.into()],
        )?
    };
    let value = if value.is_nil() {
        Value::Boolean(true)
    } else {
        value
    };
    loaded.borrow_mut(gc).set_field(module_name, value);

    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(&window);
    stack[0] = value;
    stack[1] = lua_filename.into();
    Ok(2)
}
