use clap::Parser;

use crate::repl::MonkeCli;
mod repl;

#[derive(Parser)]
struct Cli {
    path: Option<String>,

    #[arg(short, long)]
    repl: bool,

    #[arg(short, long)]
    compile: bool,
}

fn main() {
    let cli = Cli::parse();

    let monke_cli = MonkeCli::new(cli.compile);

    if let Some(path) = cli.path {
        let result = monke_cli.execute_file(path);

        if result.is_err() {
            println!("{:?}", result.err())
        }

        return;
    }

    if cli.repl {
        monke_cli.start();
    }
}
