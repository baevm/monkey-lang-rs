use std::{
    fs,
    io::{self, Error, ErrorKind, Write},
    path::Path,
};

use compiler::{
    compiler::Compiler,
    vm::{self, Vm},
};
use interpreter::interpreter::Evaluator;
use monke_core::{
    ast::Program,
    lexer::Lexer,
    object::{Environment, Inspect, Null, Object},
    parser::Parser,
    parser_error::ParseError,
};

pub struct MonkeCli {
    is_compiler: bool,
}

const FILE_EXT: &str = "monke";

impl MonkeCli {
    /// Creates instance of monke CLI
    pub fn new(is_compiler: bool) -> Self {
        Self { is_compiler }
    }

    /// Starts REPL
    pub fn start(&self) {
        println!("Welcome to Monke programming language!");

        if self.is_compiler {
            println!("Running in compiler mode.");

            self.start_compiler();
        } else {
            println!("Running in interpreter mode.");

            self.start_interpreter();
        }
    }

    /// Runs compiler for REPL mode
    /// Reuses defined variables between user inputs
    fn start_compiler(&self) {
        let mut compiler = Compiler::new();

        // to reuse variables between runs
        let mut globals = vec![Object::Null(Box::new(Null {})); vm::GLOBALS_SIZE];

        loop {
            let buf = match self.read_line() {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("IO error: {e}");
                    continue;
                }
            };

            match self.repl_compile_line(&buf, &mut compiler, &mut globals) {
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
        loop {
            let buf = self.read_line();

            if let Err(err) = buf {
                panic!("{}", err);
            }

            let evaluated = self.interpret(buf.unwrap());

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

        stdin.read_line(&mut buf)?;

        Ok(buf)
    }

    /// Runs compiler/interpreter on file
    pub fn execute_file(&self, path: String) -> Result<Object, std::io::Error> {
        let file_path = Path::new(&path);

        if let Err(ext_error) = self.is_valid_file_extension(file_path) {
            return Err(ext_error);
        }

        let code = fs::read_to_string(&file_path)?;

        if self.is_compiler {
            println!("Compiling file: {path}...");
            self.execute_compiler_on_file(code)
        } else {
            println!("Interpreting file: {path}...");
            self.execute_interpreter_on_file(code)
        }
    }

    fn is_valid_file_extension(&self, file_path: &Path) -> Result<(), std::io::Error> {
        match file_path.extension() {
            Some(ext) => {
                if ext != FILE_EXT {
                    return Err(Error::new(
                        ErrorKind::Other,
                        format!("Wrong file extension. Use .{}", FILE_EXT),
                    ));
                }
                Ok(())
            }
            None => {
                return Err(Error::new(
                    ErrorKind::Other,
                    format!("File extension not found. Use .{}", FILE_EXT),
                ));
            }
        }
    }

    fn execute_compiler_on_file(&self, code: String) -> Result<Object, std::io::Error> {
        let program = self.parse_program(code);

        if let Err(errors) = program {
            let error_messages: String = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("\n");
            return Err(Error::new(ErrorKind::Other, error_messages));
        }

        let mut compiler = Compiler::new();

        if let Err(compiler_err) = compiler.compile(program.unwrap()) {
            return Err(Error::new(ErrorKind::Other, compiler_err.to_string()));
        };

        let bytecode = compiler.bytecode();
        let mut vm = Vm::new(&bytecode);

        if let Err(vm_error) = vm.run() {
            return Err(Error::new(ErrorKind::Other, vm_error.to_string()));
        };

        let result = vm.get_run_result();

        match result {
            Some(result_obj) => Ok(result_obj),
            None => Err(Error::new(ErrorKind::Other, "failed to run VM")),
        }
    }

    fn execute_interpreter_on_file(&self, code: String) -> Result<Object, std::io::Error> {
        let evaluated = self.interpret(code);

        match evaluated {
            Ok(obj) => {
                if !matches!(obj, Object::Null(_)) {
                    println!("{}", obj.inspect());
                }
                Ok(obj)
            }
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

    fn repl_compile_line(
        &self,
        buf: &str,
        compiler: &mut Compiler,
        globals: &mut [Object],
    ) -> Result<Option<Object>, Vec<ParseError>> {
        let program = self.parse_program(buf.to_string())?;

        if let Err(_e) = compiler.compile(program) {
            return Err(vec![]);
        }

        let bytecode = compiler.bytecode();

        let mut vm = Vm::new_with_state(&bytecode, globals.to_vec());

        if let Err(_e) = vm.run() {
            return Err(vec![]);
        }

        for (dst, src) in globals.iter_mut().zip(vm.globals().iter()) {
            *dst = src.clone();
        }

        Ok(vm.get_run_result())
    }

    fn interpret(&self, buf: String) -> Result<Object, Vec<ParseError>> {
        let program = self.parse_program(buf)?;

        let environment = Environment::new();

        let mut evaluator = Evaluator::new(environment);

        let evaluated = evaluator.eval(&program);

        Ok(evaluated)
    }

    fn parse_program(&self, buf: String) -> Result<Program, Vec<ParseError>> {
        let lexer = Lexer::new(&buf);
        let mut parser = Parser::new(lexer);

        if parser.is_err() {
            return Err(parser.errors());
        }

        let program = parser.parse_program();

        Ok(program)
    }
}
