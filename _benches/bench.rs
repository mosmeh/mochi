use bstr::B;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use mochi_lua::{
    runtime::Runtime,
    types::{Integer, Table},
};
use std::path::Path;

fn run_impl<P: AsRef<Path>>(script: P, args: &[&[u8]]) {
    let mut runtime = Runtime::new();
    runtime.heap().with(|gc, vm| {
        let mut vm = vm.borrow_mut(gc);
        vm.load_stdlib(gc);

        let mut arg = Table::new();
        for (i, x) in args.iter().enumerate() {
            let key = (i + 1) as Integer;
            let value = gc.allocate_string(*x);
            arg.set_integer_key(key, value);
        }
        vm.globals()
            .borrow_mut(gc)
            .set_field(gc.allocate_string(B("arg")), gc.allocate_cell(arg));
    });
    runtime
        .execute(|gc, vm| {
            let closure = vm.borrow().load_file(gc, script)?;
            Ok(gc.allocate(closure).into())
        })
        .unwrap();
}

fn run<P: AsRef<Path>>(c: &mut Criterion, name: &str, script: P, args: &[&[u8]]) {
    c.bench_function(name, |b| {
        b.iter_with_large_drop(|| run_impl(black_box(&script), black_box(args)))
    });
}

fn criterion_benchmark(c: &mut Criterion) {
    run(c, "life", "/home/mosm/Lua-Benchmarks/life.lua", &[]);
    run(c, "ack", "/home/mosm/Lua-Benchmarks/ack.lua", &[b"3", b"9"]);
    run(
        c,
        "fixpoint-fact",
        "/home/mosm/Lua-Benchmarks/fixpoint-fact.lua",
        &[b"3000"],
    );
    run(
        c,
        "heapsort",
        "/home/mosm/Lua-Benchmarks/heapsort.lua",
        &[b"10", b"250000"],
    );
    run(c, "juliaset", "/home/mosm/Lua-Benchmarks/qt.lua", &[]);
    run(c, "queen", "/home/mosm/Lua-Benchmarks/queen.lua", &[b"12"]);
    run(
        c,
        "sieve",
        "/home/mosm/Lua-Benchmarks/sieve.lua",
        &[b"5000"],
    );
    run(
        c,
        "binary",
        "/home/mosm/Lua-Benchmarks/binary-trees.lua",
        &[b"15"],
    );
    run(
        c,
        "n-body",
        "/home/mosm/Lua-Benchmarks/n-body.lua",
        &[b"1000000"],
    );
    run(
        c,
        "fannkuch",
        "/home/mosm/Lua-Benchmarks/fannkuch-redux.lua",
        &[b"10"],
    );
    run(
        c,
        "spectral-norm",
        "/home/mosm/Lua-Benchmarks/spectral-norm.lua",
        &[b"1000"],
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
