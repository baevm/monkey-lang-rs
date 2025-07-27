use std::{
    fs,
    io::{self, Error, ErrorKind, Write},
    path::Path,
};

use crate::{
    evaluator::Evaluator,
    lexer::Lexer,
    object::{Environment, Object, ObjectTrait},
    parser::Parser,
};

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to Monkey programming language!");

        loop {
            let mut stdout = std::io::stdout().lock();

            if let Err(err) = write!(stdout, ">> ") {
                println!("{err}");
                break;
            }

            if let Err(err) = stdout.flush() {
                println!("{err}");
                break;
            }

            let stdin = io::stdin();
            let mut buf = String::new();

            let line = stdin.read_line(&mut buf);

            if line.is_err() {
                panic!("{:?}", line.err());
            }

            let evaluated = interpret(buf);

            println!("{:?}", evaluated.inspect());
        }
    }
}

pub fn interpret(buf: String) -> Object {
    let lexer = Lexer::new(buf);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    let environment = Environment::new();

    let mut evaluator = Evaluator::new(environment);

    let evaluated = evaluator.eval(&program);

    evaluated
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

    Ok(interpret(code))
}
