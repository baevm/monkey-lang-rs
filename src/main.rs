use crate::repl::Repl;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    Repl::start();
}
