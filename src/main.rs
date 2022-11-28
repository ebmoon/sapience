use clap::Parser;
use sapience::{
    sexp::Program,
    ast_node::{combine_exprs, Expr, Pretty},
    lang::SimpleOp,
};
use std::{
    convert::TryFrom,
    fs,
    path::PathBuf,
};

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

    // Parse a list of exprs
    let prog: Vec<Expr<SimpleOp>> = Program::parse(&input)
        .expect("Failed to parse program")
        .0.into_iter()
        .map(|x| {
            x.try_into().expect("Input is not a valid list of expressions")
        }).collect();

    println!("{:#?}", prog);
}
