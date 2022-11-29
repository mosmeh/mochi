use super::helpers::ArgumentsExt;
use crate::{
    gc::{GcCell, GcContext, RootSet},
    rebind,
    runtime::{call, ErrorKind, Vm},
    to_rooted,
    types::{NativeClosure, NativeFunction, Table, Value},
    LUA_VERSION,
};
use bstr::{ByteSlice, ByteVec, B};

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

pub fn load<'gc, 'a>(
    gc: &'a GcContext<'gc>,
    vm: &mut Vm<'gc, 'a>,
) -> GcCell<'gc, 'a, Table<'gc, 'a>> {
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
        gc.allocate(NativeClosure::with_upvalue(package, package_require)),
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
        gc.allocate(NativeClosure::with_upvalue(package, searcher_lua))
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

fn package_require<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    roots: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    package: &GcCell<'gc, '_, Table<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let name = gc.allocate_string(args.nth(1).to_string()?);

    let loaded = vm
        .borrow(gc)
        .registry()
        .borrow(gc)
        .get_field(gc.allocate_string(super::LUA_LOADED_TABLE))
        .as_table()
        .unwrap();

    let value = loaded.borrow(gc).get_field(name);
    if value.to_boolean() {
        return Ok(vec![rebind!(gc, value)]);
    }

    let searchers = package
        .borrow(gc)
        .get_field(gc.allocate_string(B("searchers")));
    let searchers = searchers
        .as_table()
        .ok_or_else(|| ErrorKind::other("'package.searchers' must be a table"))?;

    let thread = vm.borrow(gc).current_thread();

    to_rooted!(roots, searchers, loaded, name, thread);

    let mut msg = Vec::new();
    for i in 1.. {
        let searcher = searchers.borrow(gc).get_integer_key(i);
        if searcher.is_nil() {
            return Err(ErrorKind::Other(format!(
                "module '{}' not found:{}",
                name.as_bstr(),
                msg.as_bstr()
            )));
        }

        to_rooted!(roots, searcher);
        let results = call(gc, roots, vm, *thread, *searcher, &[(*name).into()])?;
        let loader = match results.first() {
            Some(
                value @ Value::NativeFunction(_)
                | value @ Value::LuaClosure(_)
                | value @ Value::NativeClosure(_),
            ) => *value,
            Some(value) => {
                if let Some(s) = value.to_string() {
                    msg.push_str(b"\n\t");
                    msg.extend_from_slice(&s);
                }
                continue;
            }
            _ => continue,
        };
        let loader_data = results.get(1).copied().unwrap_or_default();

        to_rooted!(roots, loader, loader_data);
        let results = call(
            gc,
            roots,
            vm,
            *thread,
            *loader,
            &[(*name).into(), *loader_data],
        )?;
        let results = rebind!(gc, results);
        let mut loaded = loaded.borrow_mut(gc);
        let value = match results.first() {
            Some(Value::Nil) | None => {
                let value = loaded.get_field(*name);
                if value.is_nil() {
                    Value::Boolean(true)
                } else {
                    value
                }
            }
            Some(value) => *value,
        };
        loaded.set_field(*name, value);
        return Ok(vec![rebind!(gc, value), rebind!(gc, *loader_data)]);
    }
    unreachable!()
}

fn package_searchpath<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    _: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let name = args.nth(1);
    let name = name.to_string()?;

    let path = args.nth(2);
    let path = path.to_string()?;

    let sep = args.nth(3);
    let sep = sep.to_string_or(B("."))?;

    let rep = args.nth(4);
    let rep = rep.to_string_or(LUA_DIRSEP)?;

    Ok(match search_path(name, path, sep, rep) {
        Ok(filename) => vec![gc.allocate_string(filename).into()],
        Err(msg) => vec![Value::Nil, gc.allocate_string(msg).into()],
    })
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
        match filename.to_path() {
            Ok(p) if std::fs::File::open(p).is_ok() => {
                return Ok(filename.to_vec());
            }
            _ => (),
        }
    }

    let mut msg = b"no file '".to_vec();
    msg.append(&mut pathname.replace(LUA_PATH_SEP, b"'\n\tno file '"));
    msg.push(b'\'');
    Err(msg)
}

fn searcher_preload<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let name = args.nth(1);
    let name = name.to_string()?;

    let preload = vm
        .borrow(gc)
        .registry()
        .borrow(gc)
        .get_field(gc.allocate_string(super::LUA_PRELOAD_TABLE));
    let preload = preload.borrow_as_table(gc).unwrap();

    let value = preload.get_field(gc.allocate_string(name.clone()));
    Ok(if value.is_nil() {
        let msg = bstr::concat([b"no field package.preload[\'", name.as_ref(), b"\']"]);
        vec![gc.allocate_string(msg).into()]
    } else {
        vec![value, gc.allocate_string(B(":preload:")).into()]
    })
}

fn searcher_lua<'gc, 'a>(
    gc: &'a mut GcContext<'gc>,
    _: &RootSet<'gc>,
    vm: GcCell<'gc, '_, Vm<'gc, '_>>,
    package: &GcCell<'gc, '_, Table<'gc, '_>>,
    args: &[Value<'gc, '_>],
) -> Result<Vec<Value<'gc, 'a>>, ErrorKind> {
    let name = args.nth(1);
    let name = name.to_string()?;

    let path = package.borrow(gc).get_field(gc.allocate_string(B("path")));
    let path = path
        .to_string()
        .ok_or_else(|| ErrorKind::other("'package.path' must be a string"))?;

    let filename = match search_path(&name, path, b".", LUA_LSUBSEP) {
        Ok(filename) => filename,
        Err(msg) => return Ok(vec![gc.allocate_string(msg).into()]),
    };

    let closure = filename
        .to_path()
        .map_err(|e| e.to_string())
        .and_then(|path| vm.borrow(gc).load_file(gc, path).map_err(|e| e.to_string()));
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

    Ok(vec![
        gc.allocate(closure).into(),
        gc.allocate_string(filename).into(),
    ])
}
