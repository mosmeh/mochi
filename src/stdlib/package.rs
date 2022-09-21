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

pub fn load<'gc>(gc: &'gc GcContext, vm: &mut Vm<'gc>) -> GcCell<'gc, Table<'gc>> {
    const LUA_EXEC_DIR: &[u8] = b"!";
    const LUA_IGMARK: &[u8] = b"-";

    let lua_vdir = format!("{}.{}", LUA_VERSION.0, LUA_VERSION.1);
    let lua_vdir = lua_vdir.as_bytes();

    let package_path = {
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
        gc.allocate(NativeClosure::with_upvalues(
            package_require,
            vec![package.into()],
        )),
    );

    let registry = vm.registry();
    let mut registry = registry.borrow_mut(gc);

    let package_loaded = registry.get_field(gc.allocate_string(super::LUA_LOADED_TABLE));
    assert!(!package_loaded.is_nil());

    let package_preload = gc.allocate_cell(Table::new());
    registry.set_field(
        gc.allocate_string(super::LUA_PRELOAD_TABLE),
        package_preload,
    );

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
    table.set_field(gc.allocate_string(B("loaded")), package_loaded);
    table.set_field(
        gc.allocate_string(B("path")),
        gc.allocate_string(package_path),
    );
    table.set_field(gc.allocate_string(B("preload")), package_preload);
    let package_searchers = vec![
        NativeFunction::new(searcher_preload).into(),
        gc.allocate(NativeClosure::with_upvalues(
            searcher_lua,
            vec![package.into()],
        ))
        .into(),
    ];
    table.set_field(
        gc.allocate_string(B("searchers")),
        gc.allocate_cell(Table::from(package_searchers)),
    );
    table.set_field(
        gc.allocate_string(B("searchpath")),
        NativeFunction::new(package_searchpath),
    );

    package
}

fn package_require<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let name = gc.allocate_string(stack.arg(1).to_string()?);

    let loaded = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(super::LUA_LOADED_TABLE))
        .as_table()
        .unwrap();

    let value = loaded.borrow().get_field(name);
    if !value.is_nil() {
        return Ok(Action::Return(vec![value]));
    }

    let closure = stack.arg(0);
    let closure = closure.as_native_closure()?;
    let package = closure
        .upvalues()
        .first()
        .unwrap()
        .borrow_as_table()
        .unwrap();

    let searchers = package.get_field(gc.allocate_string(B("searchers")));
    searchers
        .borrow_as_table()
        .ok_or_else(|| ErrorKind::other("'package.searchers' must be a table"))?;

    let i = Cell::new(0);
    let msg = Rc::new(RefCell::new(Vec::new()));
    let continuation = NativeClosure::with_upvalues(
        move |gc, _, thread, mut window| {
            let mut thread = thread.borrow_mut(gc);
            let stack = thread.stack(&window);

            let closure = stack.arg(0);
            let closure = closure.as_native_closure()?;
            thread.resize_stack(&mut window, 1);

            let name = closure.upvalues()[0];
            let searchers = closure.upvalues()[1].borrow_as_table().unwrap();

            let next_i = i.get() + 1;
            i.set(next_i);

            let searcher = searchers.get(next_i);
            if searcher.is_nil() {
                return Err(ErrorKind::Other(format!(
                    "module '{}' not found:{}",
                    name.to_string().unwrap().as_bstr(),
                    msg.borrow().as_bstr()
                )));
            }

            let msg = msg.clone();
            let continuation: Box<NativeClosureFn> = Box::new(move |gc, _, thread, mut window| {
                let mut thread = thread.borrow_mut(gc);
                let stack = thread.stack(&window);

                let loader = match stack.get(1) {
                    Some(
                        value @ Value::NativeFunction(_)
                        | value @ Value::LuaClosure(_)
                        | value @ Value::NativeClosure(_),
                    ) => *value,
                    Some(value) => {
                        if let Some(s) = value.to_string() {
                            let mut msg = msg.borrow_mut();
                            msg.push_str(b"\n\t");
                            msg.extend_from_slice(&s);
                        }
                        return Ok(Action::TailCall {
                            callee: stack[0],
                            args: vec![],
                        });
                    }
                    _ => {
                        return Ok(Action::TailCall {
                            callee: stack[0],
                            args: vec![],
                        })
                    }
                };
                let loader_data = stack.get(2).copied().unwrap_or_default();

                let closure = stack.arg(0);
                let closure = closure.as_native_closure()?;
                let name = closure.upvalues()[0];

                thread.resize_stack(&mut window, 2);
                let stack = thread.stack_mut(&window);
                stack[1] = loader_data;

                let continuation: Box<NativeClosureFn> = Box::new(|gc, _, thread, window| {
                    let thread = thread.borrow();
                    let stack = thread.stack(&window);

                    let loader_data = stack[1];
                    let value = match stack.get(2) {
                        Some(Value::Nil) | None => Value::Boolean(true),
                        Some(value) => *value,
                    };

                    let closure = stack.arg(0);
                    let closure = closure.as_native_closure()?;
                    let name = closure.upvalues()[0];
                    let loaded = closure.upvalues()[2];
                    loaded.borrow_as_table_mut(gc).unwrap().set(name, value)?;

                    Ok(Action::Return(vec![value, loader_data]))
                });

                Ok(Action::Call {
                    callee: loader,
                    args: vec![name, loader_data],
                    continuation,
                })
            });

            Ok(Action::Call {
                callee: searcher,
                args: vec![name],
                continuation,
            })
        },
        vec![name.into(), searchers, loaded.into()],
    );

    Ok(Action::TailCall {
        callee: gc.allocate(continuation).into(),
        args: Vec::new(),
    })
}

fn package_searchpath<'gc>(
    gc: &'gc GcContext,
    _: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let name = stack.arg(1);
    let name = name.to_string()?;

    let path = stack.arg(2);
    let path = path.to_string()?;

    let sep = stack.arg(3);
    let sep = sep.to_string_or(B("."))?;

    let rep = stack.arg(4);
    let rep = rep.to_string_or(LUA_DIRSEP)?;

    Ok(Action::Return(match search_path(name, path, sep, rep) {
        Ok(filename) => vec![gc.allocate_string(filename).into()],
        Err(msg) => vec![Value::Nil, gc.allocate_string(msg).into()],
    }))
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
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let name = thread.borrow().stack(&window).arg(1);
    let name = name.to_string()?;

    let preload = vm
        .registry()
        .borrow()
        .get_field(gc.allocate_string(super::LUA_PRELOAD_TABLE));
    let preload = preload.borrow_as_table().unwrap();

    let value = preload.get_field(gc.allocate_string(name.clone()));
    Ok(Action::Return(if value.is_nil() {
        let msg = bstr::concat([b"no field package.preload[\'", name.as_ref(), b"\']"]);
        vec![gc.allocate_string(msg).into()]
    } else {
        vec![value, gc.allocate_string(B(":preload:")).into()]
    }))
}

fn searcher_lua<'gc>(
    gc: &'gc GcContext,
    vm: &mut Vm<'gc>,
    thread: GcCell<'gc, LuaThread<'gc>>,
    window: StackWindow,
) -> Result<Action<'gc>, ErrorKind> {
    let thread = thread.borrow();
    let stack = thread.stack(&window);

    let name = stack.arg(1);
    let name = name.to_string()?;

    let closure = stack.arg(0);
    let closure = closure.as_native_closure()?;
    let package = closure
        .upvalues()
        .first()
        .unwrap()
        .borrow_as_table()
        .unwrap();

    let path = package.get_field(gc.allocate_string(B("path")));
    let path = path
        .to_string()
        .ok_or_else(|| ErrorKind::other("'package.path' must be a string"))?;

    let filename = match search_path(&name, path, b".", LUA_LSUBSEP) {
        Ok(filename) => filename,
        Err(msg) => return Ok(Action::Return(vec![gc.allocate_string(msg).into()])),
    };

    let closure = filename
        .to_path()
        .map_err(|e| e.to_string())
        .and_then(|path| vm.load_file(gc, path).map_err(|e| e.to_string()));
    let closure = match closure {
        Ok(closure) => closure,
        Err(err) => {
            return Err(ErrorKind::Other(format!(
                "error loading module '{}' from file '{}':\n\t{}",
                name.as_bstr(),
                filename.as_bstr(),
                err
            )))
        }
    };

    Ok(Action::Return(vec![
        gc.allocate(closure).into(),
        gc.allocate_string(filename).into(),
    ]))
}
