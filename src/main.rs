use clap::Parser;
use egg::{AstSize, CostFunction, RecExpr};
use sapience::{
    sexp::Program,
    ast_node::{combine_exprs, Expr, Pretty, AstNode},
    lang::SimpleOp,
    learner::Learner,
};
use std::{
    convert::TryFrom,
    fs,
    path::PathBuf,
};
use ruler::{
    SynthParams,
    SynthLanguage,
    synth::synth,
    equality::{Equality, SerializedEq},
    enumo::{ruleset::Ruleset, workload::Workload},
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
    env_logger::init();

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

    println!("{}", initial_expr.clone());
    println!("Initial expression (cost {}):", initial_cost);
    println!();
    
    let dsrs = vec![
        egg::rewrite!("plus commute"; "(+ ?x ?y)" => "(+ ?y ?x)"),
        egg::rewrite!("plus zero"; "(+ ?x 0)" => "?x"),
    ];
    

    let workload = Workload::iter_lang(5, &["0", "1"], &["a", "b", "c"], &["--"], &["+", "-", "*", "/"]);
    let synthParam = SynthParams {
        prior_rules: Ruleset::<AstNode<SimpleOp>>::default(),
        workload: workload,
        node_limit: 300000,
        iter_limit: 3,
        time_limit: 30,
    };
    
    let rules = synth(synthParam);

    for eq in rules.0.iter() {
        println!("{:?}", <Equality<AstNode<SimpleOp>> as Into<SerializedEq>>::into(eq.clone()));
    }

    let learner = Learner::gen(prog, dsrs, opts.beams, opts.lps);

    learner.learn("harness/data_gen/res.csv");
}
