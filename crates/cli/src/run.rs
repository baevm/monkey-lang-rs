use std::{
    fs,
    io::{self, Error, ErrorKind, Write},
    path::Path,
};

use monke_core::{
    evaluator::Evaluator,
    lexer::Lexer,
    object::{Environment, Object, ObjectTrait},
    parser::{ParseErr, Parser},
};

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to Monke programming language!");

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

            match evaluated {
                Ok(obj) => println!("{:?}", obj.inspect()),
                Err(errors) => errors.iter().for_each(|err| println!("{}", err)),
            }
        }
    }
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
