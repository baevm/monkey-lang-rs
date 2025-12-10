use std::hint::black_box;

use compiler::{compiler::Compiler, vm::Vm};
use criterion::{Criterion, criterion_group, criterion_main};

use monke_core::{lexer::Lexer, parser::Parser};

fn compile_and_run(input: &str) -> i64 {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let mut compiler = Compiler::new();
    compiler.compile(program).unwrap();

    let bytecode = compiler.bytecode();
    let mut vm = Vm::new(&bytecode);

    vm.run().unwrap();

    if let Some(obj) = vm.get_run_result() {
        if let monke_core::object::Object::Integer(val) = obj {
            return val.value;
        }
    }

    0
}

fn benchmark_arithmetic(c: &mut Criterion) {
    c.bench_function("simple addition", |b| {
        b.iter(|| black_box(compile_and_run("1 + 2")))
    });

    c.bench_function("complex arithmetic", |b| {
        b.iter(|| black_box(compile_and_run("(5 + 10 * 2 + 15 / 3) * 2 + 10")))
    });

    c.bench_function("nested arithmetic", |b| {
        b.iter(|| black_box(compile_and_run("((10 + 5) * 3 - 8) / 2 + (20 - 15) * 4")))
    });
}

fn benchmark_compilation_only(c: &mut Criterion) {
    c.bench_function("compile simple expression", |b| {
        b.iter(|| {
            let lexer = Lexer::new(black_box("1 + 2 * 3"));
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            let mut compiler = Compiler::new();
            compiler.compile(program).unwrap();
        })
    });

    c.bench_function("compile complex expression", |b| {
        b.iter(|| {
            let input = black_box("(5 + 10 * 2 + 15 / 3) * 2 + 10");
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            let mut compiler = Compiler::new();
            compiler.compile(program).unwrap();
        })
    });
}

fn benchmark_vm_execution(c: &mut Criterion) {
    // Pre-compile the bytecode
    let lexer = Lexer::new("(5 + 10 * 2 + 15 / 3) * 2 + 10");
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let mut compiler = Compiler::new();
    compiler.compile(program).unwrap();
    let bytecode = compiler.bytecode();

    c.bench_function("vm execute precompiled", |b| {
        b.iter(|| {
            let mut vm = Vm::new(&bytecode);
            black_box(vm.run().unwrap());
        })
    });
}

fn benchmark_many_operations(c: &mut Criterion) {
    c.bench_function("100 additions", |b| {
        let input = (0..100)
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(" + ");
        b.iter(|| black_box(compile_and_run(&input)))
    });

    c.bench_function("50 multiplications", |b| {
        let input = (1..=50)
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(" * ");
        b.iter(|| black_box(compile_and_run(&input)))
    });
}

fn benchmark_fibonacci_of_20(c: &mut Criterion) {
    c.bench_function("compiler fibonacci of 20", |b| {
        let input = r#"
            let fibonacci = function(x) {
                if (x == 0) {
                    return 0;
                } else {
                    if (x == 1) {
                        return 1;
                    } else {
                        return fibonacci(x - 1) + fibonacci(x - 2);
                    }
                }
            };
            fibonacci(20);
        "#;
        b.iter(|| black_box(compile_and_run(input)))
    });
}

criterion_group!(
    benches,
    benchmark_arithmetic,
    benchmark_compilation_only,
    benchmark_vm_execution,
    benchmark_many_operations,
    benchmark_fibonacci_of_20
);
criterion_main!(benches);
