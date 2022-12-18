use clap::Parser;
use egg::{AstSize, CostFunction, RecExpr, Rewrite, Pattern};
use sapience::{
    beam::PartialLibCost,
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
    Limits,
    SynthLanguage,
    equality::Equality,
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

    // Basic rewrite rules of lambda calculus
    let lam_rules = [
        "(@ (lam $0) ?x) ==> ?x"
    ];

    let mut prior_rules = Ruleset::<AstNode<SimpleOp>>::default();
    prior_rules.extend(Ruleset::from_str_vec(&lam_rules));

    // Constants and symbols of LIA
    let workload = Workload::iter_lang(
        3, 
        &["-1", "0", "1"], 
        &["a", "b", "c"], 
        &["neg", "!"], 
        &["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!=", "&&", "||"]
    );

    // Learn rules of LIA
    let rules = <AstNode<SimpleOp> as SynthLanguage>::run_workload (
        workload,
        prior_rules,
        Limits::default(),
    );
    
    let dsrs: Vec<Rewrite<AstNode<SimpleOp>, PartialLibCost>> = rules
        .0.values().enumerate()
        .map(|(i, rw)| {
            let searcher: Pattern<_> = rw.lhs.to_owned();
            let applier: Pattern<_> = rw.rhs.to_owned();
            let name = format!("ruler {}", i);
            
            Rewrite::new(name, searcher, applier).unwrap_or_else(|_| unreachable!())
        })
        .collect();

    let learner = Learner::gen(prog, dsrs, opts.beams, opts.lps);

    let lifted = learner.learn();
    let final_cost = AstSize.cost_rec(&lifted);

    println!("{}", lifted.clone());
    println!("final cost: {}", final_cost);

}
