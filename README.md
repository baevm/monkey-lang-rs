# monkey-lang-rs
Monkey language interpreter based on book "Writing an Interpreter in Go"

---
Supported features:
- [x] Variables
- [x] Types: integers, booleans, strings, null
- [x] Arithmetic operations and compound operators: +, -, *, /, +=, -=, *=, /=
- [x] Comparison operations: ==, !=, <, <=, >, >=
- [x] If statements
- [x] Functions
- [x] Closures
- [x] Built-in functions
- [x] Arrays
- [x] Objects

---
To run REPL:
```
    cargo run . 
```

Example:
```
Welcome to Monkey programming language!

>> let addFunction = function(x, y) { return x + y; };
"null"
>> addFunction(6,2);
"8"
```