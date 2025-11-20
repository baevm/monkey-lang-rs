use std::{
    fs,
    io::{self, Error, ErrorKind, Write},
    path::Path,
};

use ::compiler::symbol_table::SymbolTable;

use compiler::{
    compiler::Compiler,
    vm::{self, Vm},
};
use monke_core::{
    evaluator::Evaluator,
    lexer::Lexer,
    object::{Environment, Null, Object, ObjectTrait},
    parser::{ParseErr, Parser},
};

pub struct Repl {
    is_compiler: bool,
}

impl Repl {
    pub fn new(is_compiler: bool) -> Self {
        Self { is_compiler }
    }

    pub fn start(&self) {
        println!("Welcome to Monke programming language!");

        if self.is_compiler {
            self.start_compiler();
        } else {
            self.start_interpreter();
        }
    }

    fn start_compiler(&self) {
        println!("Running in compiler mode.");

        let mut compiler = Compiler::new();
        let mut globals = vec![Object::Null(Box::new(Null {})); vm::GLOBALS_SIZE];

        loop {
            let buf = match self.read_line() {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("IO error: {e}");
                    continue;
                }
            };

            match compile_line(&buf, &mut compiler, &mut globals) {
                Ok(Some(obj)) => println!("{}", obj.inspect()),
                Ok(None) => {} // e.g. pure let statement
                Err(parse_errs) => {
                    for e in parse_errs {
                        eprintln!("{e}");
                    }
                }
            }
        }
    }

    fn start_interpreter(&self) {
        println!("Running in interpreter mode.");

        loop {
            let buf = self.read_line();

            if let Err(err) = buf {
                panic!("{}", err);
            }

            let evaluated = interpret(buf.unwrap());

            match evaluated {
                Ok(obj) => println!("{:?}", obj.inspect()),
                Err(errors) => errors.iter().for_each(|err| println!("{}", err)),
            }
        }
    }

    fn read_line(&self) -> Result<String, std::io::Error> {
        let mut stdout = std::io::stdout().lock();

        write!(stdout, ">> ")?;

        stdout.flush()?;

        let stdin = io::stdin();
        let mut buf = String::new();

        let line = stdin.read_line(&mut buf)?;

        Ok(buf)
    }
}

fn compile_line(
    buf: &str,
    compiler: &mut Compiler,
    globals: &mut [Object],
) -> Result<Option<Object>, Vec<ParseErr>> {
    let lexer = Lexer::new(buf.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.is_err() {
        return Err(parser.errors());
    }

    if let Err(_e) = compiler.compile(program) {
        return Err(vec![]);
    }

    let bytecode = compiler.bytecode();

    let mut vm = Vm::new_with_state(bytecode, globals.to_vec());

    if let Err(_e) = vm.run() {
        return Err(vec![]);
    }

    for (dst, src) in globals.iter_mut().zip(vm.globals.iter()) {
        *dst = src.clone();
    }

    Ok(vm.last_popped_stack_element())
}

fn interpret(buf: String) -> Result<Object, Vec<ParseErr>> {
    let lexer = Lexer::new(buf);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    if parser.is_err() {
        return Err(parser.errors());
    }

    let environment = Environment::new();

    let mut evaluator = Evaluator::new(environment);

    let evaluated = evaluator.eval(&program);

    Ok(evaluated)
}

const FILE_EXT: &str = "monke";

pub fn run_file(path: String) -> Result<Object, std::io::Error> {
    let file_path = Path::new(&path);

    match file_path.extension() {
        Some(ext) => {
            if ext != FILE_EXT {
                return Err(Error::new(
                    ErrorKind::Other,
                    format!("Wrong file extension. Use .{}", FILE_EXT),
                ));
            }
        }
        None => {
            return Err(Error::new(
                ErrorKind::Other,
                format!("File extension not found. Use .{}", FILE_EXT),
            ));
        }
    }

    let code = fs::read_to_string(path)?;

    let evaluated = interpret(code);

    match evaluated {
        Ok(obj) => Ok(obj),
        Err(errors) => {
            let error_messages: String = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n");
            Err(Error::new(ErrorKind::Other, error_messages))
        }
    }
}
