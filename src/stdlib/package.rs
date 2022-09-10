use super::helpers::StackExt;
use crate::{
    gc::{GcCell, GcContext},
    runtime::{ErrorKind, Vm},
    types::{
        Action, LuaThread, NativeClosure, NativeClosureFn, NativeFunction, StackWindow, Table,
        Value,
    },
    LUA_VERSION,
};
use bstr::{ByteSlice, ByteVec, B};
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

const LUA_PATH_SEP: &[u8] = b";";
const LUA_PATH_MARK: &[u8] = b"?";

const LUA_DIRSEP: &[u8] = {
    #[cfg(windows)]
    {
        b"\\"
    }

    #[cfg(not(windows))]
    {
        b"/"
    }
};
const LUA_LSUBSEP: &[u8] = LUA_DIRSEP;

pub fn load<'gc>(gc: &'gc GcContext, vm: &Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    const LUA_EXEC_DIR: &[u8] = b"!";
    const LUA_IGMARK: &[u8] = b"-";

    let lua_vdir = format!("{}.{}", LUA_VERSION.0, LUA_VERSION.1);
    let lua_vdir = lua_vdir.as_bytes();

    let path = {
        #[cfg(windows)]
        {
            const LUA_LDIR: &[u8] = b"!\\lua\\";
            const LUA_CDIR: &[u8] = b"!\\";
            let lua_shrdir = &bstr::concat([b"!\\..\\share\\lua\\", lua_vdir, b"\\"])[..];
            let lua_path_default = bstr::concat([
                LUA_LDIR,
                b"?.lua;",
                LUA_LDIR,
                b"?\\init.lua;",
                LUA_CDIR,
                b"?.lua;",
                LUA_CDIR,
                b"?\\init.lua;",
                lua_shrdir,
                b"?.lua;",
                lua_shrdir,
                b"?\\init.lua;.\\?.lua;.\\?\\init.lua",
            ]);

            // TODO: handle error
            std::env::current_exe()
                .ok()
                .and_then(|path| path.parent().map(|path| path.to_path_buf()))
                .and_then(|path| Vec::from_path_buf(path).ok())
                .map(|path| lua_path_default.replace(LUA_EXEC_DIR, path))
                .unwrap_or(lua_path_default)
        }

        #[cfg(not(windows))]
        {
            const LUA_ROOT: &[u8] = b"/usr/local/";
            let lua_ldir = &bstr::concat([LUA_ROOT, b"share/lua/", lua_vdir, b"/"])[..];
            let lua_cdir = &bstr::concat([LUA_ROOT, b"lib/lua/", lua_vdir, b"/"])[..];
            bstr::concat([
                lua_ldir,
                b"?.lua;",
                lua_ldir,
                b"?/init.lua;",
                lua_cdir,
                b"?.lua;",
                lua_cdir,
                b"?/init.lua;./?.lua;./?/init.lua",
            ])
        }
    };

    let package = gc.allocate_cell(Table::new());

    let globals = vm.globals();
    let mut globals = globals.borrow_mut(gc);
    globals.set_field(
        gc.allocate_string(B("require")),
        gc.allocate(NativeClosure::with_upvalues(require, vec![package.into()])),
    );

    let registry = vm.registry();
    let mut registry = registry.borrow_mut(gc);

    let loaded = registry.get_field(gc.allocate_string(super::LUA_LOADED_TABLE));
    assert!(!loaded.is_nil());

    let preload = gc.allocate_cell(Table::new());
    registry.set_field(gc.allocate_string(super::LUA_PRELOAD_TABLE), preload);

    let mut table = package.borrow_mut(gc);
    table.set_field(
        gc.allocate_string(B("config")),
        gc.allocate_string(bstr::join(
            b"\n",
            [
                LUA_DIRSEP,
                LUA_PATH_SEP,
                LUA_PATH_MARK,
                LUA_EXEC_DIR,
                LUA_IGMARK,
                b"",
            ],
        )),
    );
    table.set_field(gc.allocate_string(B("loaded")), loaded);
    table.set_field(gc.allocate_string(B("path")), gc.allocate_string(path));
    table.set_field(gc.allocate_string(B("preload")), preload);
    let searchers = vec![
        NativeFunction::new(searcher_preload).into(),
        gc.allocate(NativeClosure::with_upvalues(
            searcher_lua,
            vec![package.into()],
        ))
        .into(),
    ];
    table.set_field(
        gc.allocate_string(B("searchers")),
        gc.allocate_cell(Table::from(searchers)),
    );
    table.set_field(
        gc.allocate_string(B("searchpath")),
        NativeFunction::new(package_searchpath),
    );

    package
}

fn require<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let name = gc.allocate_string(stack.arg(0).to_string()?);

    let loaded = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(super::LUA_LOADED_TABLE))
        .as_table()
        .unwrap();

    let value = loaded.borrow().get_field(name);
    if !value.is_nil() {
        stack[0] = value;
        return Ok(Action::Return { num_results: 1 });
    }

    let closure = stack.callee();
    let closure = closure.as_native_closure().unwrap();
    let package = closure
        .upvalues()
        .first()
        .unwrap()
        .borrow_as_table()
        .unwrap();

    let searchers = package.get_field(gc.allocate_string(B("searchers")));
    searchers.borrow_as_table().ok_or_else(|| {
        ErrorKind::ExplicitError("'package.searchers' must be a table".to_owned())
    })?;

    let i = Cell::new(0);
    let msg = Rc::new(RefCell::new(Vec::new()));
    let continuation = NativeClosure::with_upvalues(
        move |gc, _, thread, mut window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack(&window);

            let closure = stack.callee();
            let closure = closure.as_native_closure().unwrap();
            let name = closure.upvalues()[0];
            let searchers = closure.upvalues()[1].borrow_as_table().unwrap();

            let next_i = i.get() + 1;
            i.set(next_i);

            let searcher = searchers.get(next_i);
            if searcher.is_nil() {
                return Err(ErrorKind::ExplicitError(format!(
                    "module '{}' not found:{}",
                    name.to_string().unwrap().as_bstr(),
                    msg.borrow().as_bstr()
                )));
            }

            thread.ensure_stack(&mut window, 3);
            let stack = thread.stack_mut(&window);
            stack[1] = searcher;
            stack[2] = name;

            let msg = msg.clone();
            let continuation: Box<NativeClosureFn> = Box::new(move |gc, _, thread, mut window| {
                let mut thread = thread.borrow_mut(gc);
                let stack = thread.stack(&window);

                match stack.get(1) {
                    Some(
                        Value::NativeFunction(_) | Value::LuaClosure(_) | Value::NativeClosure(_),
                    ) => (),
                    Some(value) => {
                        if let Some(s) = value.to_string() {
                            let mut msg = msg.borrow_mut();
                            msg.push_str(b"\n\t");
                            msg.extend_from_slice(&s);
                        }
                        return Ok(Action::TailCall { num_args: 0 });
                    }
                    _ => return Ok(Action::TailCall { num_args: 0 }),
                }

                let closure = stack.callee();
                let closure = closure.as_native_closure().unwrap();
                let name = closure.upvalues()[0];

                thread.ensure_stack(&mut window, 5);
                let stack = thread.stack_mut(&window);
                let loader = stack[1];
                let loader_data = stack[2];
                stack[1] = loader_data;
                stack[2] = loader;
                stack[3] = name;
                stack[4] = loader_data;

                let continuation: Box<NativeClosureFn> = Box::new(|gc, _, thread, window| {
                    let mut thread = thread.borrow_mut(gc);
                    let stack = thread.stack_mut(&window);

                    let value = match stack.get(2) {
                        Some(Value::Nil) | None => Value::Boolean(true),
                        Some(value) => *value,
                    };

                    let closure = stack.callee();
                    let closure = closure.as_native_closure().unwrap();
                    let name = closure.upvalues()[0];
                    let loaded = closure.upvalues()[2];
                    loaded.borrow_as_table_mut(gc).unwrap().set(name, value)?;

                    let loader_data = stack[1];
                    stack[0] = value;
                    stack[1] = loader_data;
                    Ok(Action::Return { num_results: 2 })
                });

                Ok(Action::Call {
                    callee_bottom: 2,
                    continuation,
                })
            });

            Ok(Action::Call {
                callee_bottom: 1,
                continuation,
            })
        },
        vec![name.into(), searchers, loaded.into()],
    );

    stack[0] = gc.allocate(continuation).into();
    Ok(Action::TailCall { num_args: 0 })
}

fn package_searchpath<'gc>(
    gc: &'gc GcContext,
    _: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let name = stack.arg(0);
    let name = name.to_string()?;

    let path = stack.arg(1);
    let path = path.to_string()?;

    let sep = stack.arg(2);
    let sep = sep.to_string_or(B("."))?;

    let rep = stack.arg(3);
    let rep = rep.to_string_or(LUA_DIRSEP)?;

    match search_path(name, path, sep, rep) {
        Ok(filename) => {
            stack[0] = gc.allocate_string(filename).into();
            Ok(Action::Return { num_results: 1 })
        }
        Err(msg) => {
            stack[0] = Value::Nil;
            stack[1] = gc.allocate_string(msg).into();
            Ok(Action::Return { num_results: 2 })
        }
    }
}

fn search_path<N, P, S, D>(name: N, path: P, sep: S, dirsep: D) -> Result<Vec<u8>, Vec<u8>>
where
    N: AsRef<[u8]>,
    P: AsRef<[u8]>,
    S: AsRef<[u8]>,
    D: AsRef<[u8]>,
{
    let name = name.as_ref().replace(sep, dirsep);
    let pathname = path.as_ref().replace(LUA_PATH_MARK, name);
    for filename in pathname.split_str(LUA_PATH_SEP) {
        if let Ok(p) = filename.to_path() {
            if std::fs::File::open(p).is_ok() {
                return Ok(filename.to_vec());
            }
        }
    }

    let mut msg = b"no file '".to_vec();
    msg.append(&mut pathname.replace(LUA_PATH_SEP, b"'\n\tno file '"));
    msg.push(b'\'');
    Err(msg)
}

fn searcher_preload<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread = thread.borrow_mut(gc);
    let stack = thread.stack_mut(&window);

    let name = stack.arg(0);
    let name = name.to_string()?;

    let preload = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(super::LUA_PRELOAD_TABLE));
    let preload = preload.borrow_as_table().unwrap();

    let value = preload.get_field(gc.allocate_string(name.clone()));
    if value.is_nil() {
        stack[0] = gc
            .allocate_string(bstr::concat([
                b"no field package.preload[\'",
                name.as_ref(),
                b"\']",
            ]))
            .into();
        Ok(Action::Return { num_results: 1 })
    } else {
        stack[0] = value;
        stack[1] = gc.allocate_string(B(":preload:")).into();
        Ok(Action::Return { num_results: 2 })
    }
}

fn searcher_lua<'gc>(
    gc: &'gc GcContext,
    vm: &Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action, ErrorKind> {
    let mut thread_ref = thread.borrow_mut(gc);
    let stack = thread_ref.stack_mut(&window);

    let name = stack.arg(0);
    let name = name.to_string()?;

    let closure = stack.callee();
    let closure = closure.as_native_closure().unwrap();
    let package = closure
        .upvalues()
        .first()
        .unwrap()
        .borrow_as_table()
        .unwrap();

    let path = package.get_field(gc.allocate_string(B("path")));
    let path = path
        .to_string()
        .ok_or_else(|| ErrorKind::ExplicitError("'package.path' must be a string".to_owned()))?;

    let filename = match search_path(&name, path, b".", LUA_LSUBSEP) {
        Ok(filename) => filename,
        Err(msg) => {
            stack[0] = gc.allocate_string(msg).into();
            return Ok(Action::Return { num_results: 1 });
        }
    };

    let closure = filename
        .to_path()
        .map_err(|e| e.to_string())
        .and_then(|path| vm.load_file(gc, path).map_err(|e| e.to_string()));
    let closure = match closure {
        Ok(closure) => closure,
        Err(err) => {
            return Err(ErrorKind::ExplicitError(format!(
                "error loading module '{}' from file '{}':\n\t{}",
                name.as_bstr(),
                filename.as_bstr(),
                err
            )))
        }
    };

    stack[0] = gc.allocate(closure).into();
    stack[1] = gc.allocate_string(filename).into();
    Ok(Action::Return { num_results: 2 })
}
