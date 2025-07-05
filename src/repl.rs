use std::io::{self, Write};

use crate::{
    evaluator::Evaluator,
    lexer::Lexer,
    object::{Environment, ObjectTrait},
    parser::Parser,
};

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to Monkey programming language!");

        // reuse defined user variables between inputs
        let mut environment = Environment::new();

        loop {
            let mut stdout = std::io::stdout().lock();

            if let Err(err) = write!(stdout, ">> ") {
                println!("{}", err);
                break;
            }

            if let Err(err) = stdout.flush() {
                println!("{}", err);
                break;
            }

            let stdin = io::stdin();
            let mut buf = String::new();

            let line = stdin.read_line(&mut buf);

            if line.is_err() {
                panic!("{:?}", line.err());
            }

            let lexer = Lexer::new(buf);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let evaluator = Evaluator::new();

            let evaluated = evaluator.eval(&program, &mut environment);

            println!("{:?}", evaluated.inspect());
        }
    }
}
