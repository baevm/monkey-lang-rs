use ::compiler::compiler::Compiler;
use compiler::vm::Vm;
use interpreter::interpreter::Evaluator;
use monke_core::{
    lexer::Lexer,
    object::{Environment, Inspect, Object},
    output::{BufferHandler, get_buffer, set_output_handler},
    parser::Parser,
    parser_error::ParseError,
};
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct RunResult {
    stdout: String,
    value: Option<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Serialize)]
struct Diagnostic {
    severity: String,
    phase: String,
    message: String,
    line: Option<usize>,
    column: Option<usize>,
}

type RunCodeResult = Result<JsValue, JsValue>;

fn serialize_result(result: &RunResult) -> RunCodeResult {
    serde_wasm_bindgen::to_value(result).map_err(|error| JsValue::from_str(&error.to_string()))
}

#[wasm_bindgen]
pub fn compile_code(input: &str) -> RunCodeResult {
    // captures all stdout
    set_output_handler(Box::new(BufferHandler::new()));

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let parser_errors = parser.errors();

    if !parser_errors.is_empty() {
        let diagnostics = parser_errors
            .iter()
            .map(|err| {
                let (column, line) = match err {
                    ParseError::UnexpectedToken { info, .. } => {
                        (Some(info.column), Some(info.line))
                    }
                    _ => (None, None),
                };

                return Diagnostic {
                    column,
                    line,
                    message: err.to_string(),
                    phase: "parser".to_string(),
                    severity: "error".to_string(),
                };
            })
            .collect();

        return serialize_result(&RunResult {
            stdout: get_buffer(),
            value: None,
            diagnostics,
        });
    }

    let mut compiler = Compiler::new();
    if let Err(err) = compiler.compile(program) {
        return serialize_result(&RunResult {
            stdout: get_buffer(),
            value: None,
            diagnostics: vec![Diagnostic {
                severity: "error".to_string(),
                phase: "compiler".to_string(),
                message: err.to_string(),
                line: None,
                column: None,
            }],
        });
    };

    let bytecode = compiler.bytecode();
    let mut vm = Vm::new(&bytecode);

    if let Err(err) = vm.run() {
        return serialize_result(&RunResult {
            stdout: get_buffer(),
            value: None,
            diagnostics: vec![Diagnostic {
                severity: "error".to_string(),
                phase: "runtime".to_string(),
                message: err.to_string(),
                line: None,
                column: None,
            }],
        });
    }

    let value = vm.get_run_result().and_then(|object| match object {
        Object::Null(_) => None,
        value => Some(value.inspect()),
    });

    serialize_result(&RunResult {
        stdout: get_buffer(),
        value,
        diagnostics: vec![],
    })
}

#[wasm_bindgen]
pub fn interpret_code(input: &str) -> RunCodeResult {
    // captures all stdout
    set_output_handler(Box::new(BufferHandler::new()));

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let parser_errors = parser.errors();

    if !parser_errors.is_empty() {
        let diagnostics = parser_errors
            .iter()
            .map(|err| {
                let (column, line) = match err {
                    ParseError::UnexpectedToken { info, .. } => {
                        (Some(info.column), Some(info.line))
                    }
                    _ => (None, None),
                };

                return Diagnostic {
                    column,
                    line,
                    message: err.to_string(),
                    phase: "parser".to_string(),
                    severity: "error".to_string(),
                };
            })
            .collect();

        return serialize_result(&RunResult {
            stdout: get_buffer(),
            value: None,
            diagnostics,
        });
    }

    let env = Environment::new();
    let mut interpreter = Evaluator::new(env);
    let result = interpreter.eval(&program);

    if let Object::InternalError(err) = &result {
        let diagnostics = vec![Diagnostic {
            column: None,
            line: None,
            message: err.message.to_string(),
            phase: "runtime".to_string(),
            severity: "error".to_string(),
        }];

        return serialize_result(&RunResult {
            stdout: get_buffer(),
            value: None,
            diagnostics,
        });
    }

    let value = match result {
        Object::Null(_) => None,
        value => Some(value.inspect()),
    };

    serialize_result(&RunResult {
        stdout: get_buffer(),
        value,
        diagnostics: vec![],
    })
}
