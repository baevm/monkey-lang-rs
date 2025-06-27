use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to Monkey programming language!");

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

            if parser.errors.len() != 0 {
                println!("parser errors:");
                parser.errors.iter().for_each(|err| println!("\t{}", err));
                continue;
            }

            println!("{}", program.to_string())
        }
    }
}
