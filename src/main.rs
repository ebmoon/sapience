use clap::Parser;
use egg::{AstSize, CostFunction, RecExpr};
use sapience::{
    sexp::Program,
    ast_node::{combine_exprs, Expr, Pretty},
    lang::SimpleOp,
    learner::Learner,
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

    #[clap(long, default_value_t = 10)]
    beams: usize,

    #[clap(long, default_value_t = 1)]
    lps: usize,
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

    
    let initial_expr: RecExpr<_> = combine_exprs(prog.clone());
    let initial_cost = AstSize.cost_rec(&initial_expr);

    println!("{:?}", initial_expr.clone());
    println!("Initial expression (cost {}):", initial_cost);
    println!();
    
    let dsrs = vec![];

    let learner = Learner::gen(prog, dsrs);

    learner.learn("harness/data_gen/res.csv");
}
