use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{Action, LuaThread, NativeFunction, StackWindow, Table, Value},
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
    mut window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let module_name = gc.allocate_string(stack.arg(0).to_string()?);

    let loaded = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(LUA_LOADED_TABLE))
        .as_table()
        .unwrap();
    let value = loaded.borrow().get_field(module_name);

    if !value.is_nil() {
        stack[0] = value;
        return Ok(Action::Return { num_results: 1 });
    }

    let filename = format!("./{}.lua", module_name.as_bstr());
    let lua_filename = gc.allocate_string(filename.clone().into_bytes());

    let closure = match vm.load_file(gc, &filename) {
        Ok(closure) => closure,
        Err(err) => return Err(ErrorKind::ExplicitError(err.to_string())),
    };

    thread.ensure_stack(&mut window, 7);
    let stack = thread.stack_mut(&window);

    stack[1] = lua_filename.into();
    stack[2] = loaded.into();
    stack[3] = module_name.into();

    stack[4] = gc.allocate(closure).into();
    stack[5] = module_name.into();
    stack[6] = lua_filename.into();

    Ok(Action::Call {
        callee: 4,
        continuation: Box::new(|gc, _, thread, window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack_mut(&window);

            let value = match stack.arg(3).get() {
                Some(Value::Nil) | None => Value::Boolean(true),
                Some(value) => value,
            };
            let module_name = stack.arg(2).to_value()?;
            stack
                .arg(1)
                .borrow_as_table_mut(gc)?
                .set(module_name, value)?;

            stack[0] = value;
            Ok(Action::Return { num_results: 2 })
        }),
    })
}
