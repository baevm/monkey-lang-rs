use ::compiler::compiler::Compiler;
use compiler::vm::Vm;
use interpreter::interpreter::Evaluator;
use monke_core::{
    lexer::Lexer,
    object::Environment,
    output::{BufferHandler, get_buffer, set_output_handler},
    parser::Parser,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile_code(input: &str) -> String {
    // captures all stdout
    set_output_handler(Box::new(BufferHandler::new()));

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let mut compiler = Compiler::new();
    if let Err(err) = compiler.compile(program) {
        return format!("Compiler error: {}", err);
    };

    let bytecode = compiler.bytecode();
    let mut vm = Vm::new(&bytecode);

    if let Err(err) = vm.run() {
        return format!("VM error: {}", err);
    }

    let mut result = String::new();

    // get captured output from print() calls
    let output = get_buffer();

    if !output.is_empty() {
        result.push_str(&output);
    }

    result
}

#[wasm_bindgen]
pub fn interpret_code(input: &str) -> String {
    // captures all stdout
    set_output_handler(Box::new(BufferHandler::new()));

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let env = Environment::new();
    let mut interpreter = Evaluator::new(env);
    let _result = interpreter.eval(&program);

    let mut result = String::new();

    // get captured output from print() calls
    let output = get_buffer();

    if !output.is_empty() {
        result.push_str(&output);
    }

    result
}
