use crate::repl::Repl;

mod lexer;
mod repl;
mod token;

fn main() {
    Repl::start();
}
