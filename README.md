# monke-lang-rs
Monke language interpreter and compiler based on books *"Writing an Interpreter in Go"* and *"Writing A Compiler In Go"*

🐵 Monke language in your browser (running in WASM + worker): [https://monkey-lang-rs.vercel.app/](https://monkey-lang-rs.vercel.app/)

---
Features:
- [x] Variables, variables reassignment
- [x] Types: integers, booleans, strings, null
- [x] Arithmetic operations and compound operators: +, -, *, /, +=, -=, *=, /=
- [x] Comparison operations: ==, !=, <, <=, >, >=
- [x] If statements
- [x] C-like for loops (for (init; condition; post) { ... })
- [x] Functions
- [x] Closures
- [x] Built-in functions
- [x] Arrays
- [x] Objects
- [x] Web playground with compiler/interpreter compiled to WASM running in web worker

---
To run REPL:
```
    cargo build --release

    ./target/release/cli --repl // runs interpreter mode
    ./target/release/cli --repl --compile // runs compiler mode 
```

Example:
```
Welcome to Monke programming language!
Running in compiler mode.

>> let addFunction = function(x, y) { return x + y; };

>> addFunction(6,2);
8
```

## CLI usage

- `--repl` / `-r`: start an interactive REPL session (interpreter by default, compiler if combined with `--compile`).
- `--compile` / `-c`: enable compiler mode (affects both REPL and file execution).
- `<path>`: optional `.monke` file to execute. When omitted, the tool waits for `--repl` to launch an interactive session.

## Benchmark

Not optimized:
- Compiler mode, fibonacci of 20: 
    - time: ~13.357 ms
- Interpreter mode, fibonacci of 20: 
    - time: ~48.13 ms

Optimized:
- Compiler mode, fibonacci of 20: 
    - time: TODO...
- Interpreter mode, fibonacci of 20: 
    - time: TODO...