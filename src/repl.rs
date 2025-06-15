use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token::TokenType};

pub struct Repl {}

impl Repl {
    pub fn start() {
        println!("Welcome to Monkey programming language!");

        loop {
            let mut stdout = std::io::stdout().lock();
            write!(stdout, ">> ");
            stdout.flush();

            let stdin = io::stdin();
            let mut buf = String::new();

            let line = stdin.read_line(&mut buf);

            if line.is_err() {
                panic!("{:?}", line.err());
            }

            let mut lexer = Lexer::new(buf);

            loop {
                let token = lexer.next_token();

                if token.token_type == TokenType::Eof {
                    break;
                }

                println!("{token}");
            }
        }
    }
}
