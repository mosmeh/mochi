use super::helpers::StackExt;
use crate::{
    gc::{root_gc, GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{LuaClosure, LuaThread, NativeFunction, StackWindow, Table, Value},
};
use bstr::{ByteSlice, B};

pub fn load<'gc>(gc: &'gc GcContext, global_table: GcCell<'gc, Table<'gc>>) {
    let mut table = global_table.borrow_mut(gc);
    table.set_field(
        gc.allocate_string(B("require")),
        NativeFunction::new(require),
    );

    let mut package = Table::new();
    package.set_field(
        gc.allocate_string(B("loaded")),
        gc.allocate_cell(Table::new()),
    );
    table.set_field(gc.allocate_string(B("package")), gc.allocate_cell(package));
}

fn require<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<usize, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(window.clone());

    let module_name = stack.arg(0);
    let module_name = module_name.to_string()?;
    let module_name = gc.allocate_string(module_name);

    let package_key = gc.allocate_string(B("package"));
    let package_table = vm.global_table().borrow().get_field(package_key);
    let package_table = package_table.as_table().unwrap();

    let loaded_key = gc.allocate_string(B("loaded"));
    let loaded_table = package_table.get_field(loaded_key);
    let maybe_loaded_value = loaded_table.as_table().unwrap().get_field(module_name);

    if !maybe_loaded_value.is_nil() {
        stack[0] = maybe_loaded_value;
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
        .push(gc.allocate_cell(Value::Table(vm.global_table()).into()));

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

    loaded_table
        .as_table_mut(gc)
        .unwrap()
        .set_field(module_name, value);

    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(window);
    stack[0] = value;
    stack[1] = lua_filename.into();
    Ok(2)
}
