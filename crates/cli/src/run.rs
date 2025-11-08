use std::{
    fs,
    io::{self, Error, ErrorKind, Write},
    path::Path,
};

use compiler::{compiler::Compiler, vm::Vm};
use monke_core::{
    evaluator::Evaluator,
    lexer::Lexer,
    object::{Environment, Object, ObjectTrait},
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

        loop {
            let buf = self.read_line();

            if let Err(err) = buf {
                panic!("{}", err);
            }

            let compiled = compile(buf.unwrap());

            match compiled {
                Ok(obj) => println!("{:?}", obj.inspect()),
                Err(errors) => errors.iter().for_each(|err| println!("{}", err)),
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

pub fn compile(buf: String) -> Result<Object, Vec<ParseErr>> {
    let lexer = Lexer::new(buf);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    if parser.is_err() {
        return Err(parser.errors());
    }

    let environment = Environment::new();

    let mut compiler = Compiler::new();

    if let Err(err) = compiler.compile(program) {
        return Err(vec![]); // TODO error handling
    }

    let mut vm = Vm::new(compiler.bytecode());

    if let Err(err) = vm.run() {
        return Err(vec![]); // TODO error handling
    };

    let stack_top = vm.last_popped_stack_element();

    if let Some(stack_top) = stack_top {
        return Ok(stack_top);
    }

    Err(vec![])
}

pub fn interpret(buf: String) -> Result<Object, Vec<ParseErr>> {
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
