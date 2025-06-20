use crate::repl::Repl;

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    Repl::start();
}
