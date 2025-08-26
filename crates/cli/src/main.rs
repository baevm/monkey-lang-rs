use clap::Parser;

use crate::run::{Repl, run_file};
mod run;

#[derive(Parser)]
struct Cli {
    path: Option<String>,

    #[arg(short, long)]
    repl: bool,
}

fn main() {
    let cli = Cli::parse();

    if let Some(path) = cli.path {
        let result = run_file(path);

        if result.is_err() {
            println!("{:?}", result.err())
        }

        return;
    }

    if cli.repl {
        Repl::start();
    }
}
