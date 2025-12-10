use criterion::{Criterion, criterion_group, criterion_main};
use monke_core::{lexer::Lexer, token::Kind};
use std::hint::black_box;

const SMALL_PROGRAM: &str = r#"
let five = 5;
let ten = 10;

let add = function(x, y) {
    x + y;
};

add(five, ten);
"#;

const COLLECTION_HEAVY_PROGRAM: &str = r#"
let catalog = [
    {"name": "alpha", "values": [1, 2, 3, 4]},
    {"name": "beta", "values": [5, 6, 7, 8]},
    {"name": "gamma", "values": [9, 10, 11, 12]}
];

let grandTotal = 0;

for (let i = 0; i < len(catalog); i += 1) {
    let entry = catalog[i];
    let values = entry["values"];
    for (let j = 0; j < len(values); j += 1) {
        grandTotal = grandTotal + values[j];
    }
};

grandTotal;
"#;

fn run_lexer(src: &str) {
    let mut lexer = Lexer::new(src);
    loop {
        if lexer.next_token().kind == Kind::Eof {
            break;
        }
    }
}

fn bench_small_program(c: &mut Criterion) {
    c.bench_function("lexer small program", |b| {
        b.iter(|| run_lexer(black_box(SMALL_PROGRAM)));
    });
}

fn bench_collection_heavy_program(c: &mut Criterion) {
    c.bench_function("lexer collection heavy program", |b| {
        b.iter(|| run_lexer(black_box(COLLECTION_HEAVY_PROGRAM)));
    });
}

criterion_group!(benches, bench_small_program, bench_collection_heavy_program,);
criterion_main!(benches);
