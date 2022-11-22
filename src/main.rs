use clap::Parser;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[clap(version, author, about)]
struct Opts {
    // The input file. The default is stdin.
    file: PathBuf,
}

fn main() {
    let opts: Opts = Opts::parse();
    let input: String = fs::read_to_string(opts.file)
        .expect("Error reading input");

    println!("{}", input);
}
