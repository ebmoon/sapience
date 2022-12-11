use crate::{
    beam::PartialLibCost,
    co_occurrence::COBuilder,
    sexp::Program,
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    lang::SimpleOp,
    teachable::Teachable,
    learn::LearnedLibrary,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    io,
    marker::PhantomData,
    time::{Duration, Instant},
};
use log::{debug, info};
use egg::{EGraph, Id, RecExpr, Rewrite, Runner};

// A set of expressions to extract a library
pub struct Learner<Op> {
    exprs: Vec<Expr<Op>>,
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
    beams: usize,
    lps: usize,
}

impl<Op: Debug> Debug for Learner<Op> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Learner")
            .field("exprs", &self.exprs)
            .finish()
    }
}

pub type CsvWriter = csv::Writer<Box<dyn io::Write>>;

impl<Op> Learner<Op>
where
    Op: Teachable
        + Printable
        + Arity
        + Clone
        + Send
        + Sync
        + Debug
        + Display
        + Hash
        + Ord
        + 'static,
{
    /// Creates a new empty learner
    pub fn new() -> Self {
        Self {
            dsrs: Vec::new(),
            exprs: Vec::new(),
            beams: 0,
            lps: 1,
        }
    }

    /// Adds all the experiments from another experiment set into this one
    pub fn add(&mut self, other: Self) {
        self.dsrs.extend(other.dsrs);
        self.exprs.extend(other.exprs);
    }


    /// Generate a new learner 
    pub fn gen(
        exprs: Vec<Expr<Op>>, 
        dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
        beams: usize, lps: usize
    ) -> Self
    {
        Self {dsrs, exprs, beams, lps}
    }

    /// Runs learning
    pub fn learn(self, csv_path: &str) {
        // let file = std::fs::File::create(csv_path).unwrap();
        // let mut writer: CsvWriter = csv::Writer::from_writer(Box::new(file));
        
        let mut egraph: EGraph<AstNode<Op>, _> = 
            EGraph::new(PartialLibCost::new(self.beams, self.lps));

        let recexprs: Vec<RecExpr<AstNode<Op>>> = 
            self.exprs.clone().into_iter().map(|x| x.into()).collect();
        let roots = recexprs.iter()
            .map(|x| egraph.add_expr(x))
            .collect::<Vec<_>>();
        egraph.rebuild();
    
        self.run_egraph(&roots, egraph);

        // To-Do: Do library learning
        ()
    }

    fn run_egraph(
        &self, roots: &[Id], egraph: EGraph<AstNode<Op>, PartialLibCost>
    ) -> Expr<Op> {
        let timeout = Duration::from_secs(600);

        let runner = Runner::<_, _, ()>::new(PartialLibCost::empty())
            .with_egraph(egraph)
            .with_iter_limit(3) 
            .with_time_limit(timeout)
            .run(&self.dsrs);

        let aeg = runner.egraph;

        /// Compute co-occurence between e-graph nodes
        let co_occurs = COBuilder::new(&aeg, roots).run();

        /// Run anti-unification
        let mut learned_lib = LearnedLibrary::new(&aeg, co_occurs);
        learned_lib.deduplicate(&aeg);

        let lib_rewrites: Vec<_> = learned_lib.rewrites().collect();
        
        let runner = Runner::<_, _, ()>::new(PartialLibCost::new(
            self.beams,
            self.lps,
        )).with_egraph(aeg.clone())
            .with_iter_limit(1)
            .with_time_limit(timeout)
            .with_node_limit(1000000)
            .run(lib_rewrites.iter());
        
        let mut egraph = runner.egraph;
        let root = egraph.add(AstNode::new(Op::list(), roots.iter().copied()));
        let mut cs = egraph[egraph.find(root)].data.clone();
        cs.set.sort_unstable_by_key(|elem| elem.full_cost);

        println!("{:?}", cs);


        let all_libs: Vec<_> = learned_lib.libs().collect();
        let mut chosen_rewrites = Vec::new();
        for lib in &cs.set[0].libs {
            debug!("{}: {}", lib.0, &all_libs[lib.0 .0]);
            chosen_rewrites.push(lib_rewrites[lib.0 .0].clone());
        }

        println!("{:?}", chosen_rewrites);

        unimplemented!()
    }
}