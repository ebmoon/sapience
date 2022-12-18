use crate::{
    beam::{PartialLibCost, LibExtractor},
    co_occurrence::COBuilder,
    ast_node::{Arity, AstNode, Expr, Printable},
    teachable::{Teachable, BindingExpr},
    learn::{LearnedLibrary, LibId},
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    io,
    time::{Duration},
};
use log::{debug, info};
use egg::{
    Analysis, 
    EGraph, 
    Id, 
    RecExpr, 
    Rewrite, 
    Runner, 
    Language,
    Pattern,
    Searcher,
};

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
    pub fn learn(self) -> RecExpr<AstNode<Op>> {
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
    
        self.run_egraph(&roots, egraph)
    }

    fn rewrite_score(
        egraph: &EGraph<AstNode<Op>, PartialLibCost>,
        rw: &Rewrite<AstNode<Op>, PartialLibCost>
    ) -> usize {
        let match_searcher = rw.searcher.n_matches(&egraph);
        let applier_pattern = rw.applier.get_pattern_ast().unwrap().clone();
        let match_applier = Pattern::new(applier_pattern).n_matches(&egraph);

        match_searcher * match_applier
    }

    fn run_egraph(
        &self, roots: &[Id], egraph: EGraph<AstNode<Op>, PartialLibCost>
    ) -> RecExpr<AstNode<Op>> {
        let timeout = Duration::from_secs(600);

        info!("Number of rewrites: {}", self.dsrs.len());

        let mut useful_dsrs = self.dsrs.clone().into_iter()
            .filter(|rw| (Self::rewrite_score(&egraph, rw) > 0))
            .collect::<Vec<Rewrite<AstNode<Op>, PartialLibCost>>>();
        useful_dsrs.sort_unstable_by_key(|rw| Self::rewrite_score(&egraph, rw));

        info!("Choose {} rewrites that may be useful", useful_dsrs.len());

        let runner = Runner::<_, _, ()>::new(PartialLibCost::empty())
            .with_egraph(egraph)
            .with_iter_limit(3) 
            .with_time_limit(timeout)
            .run(&useful_dsrs);

        let aeg = runner.egraph;

        // Compute co-occurence between e-graph nodes
        let co_occurs = COBuilder::new(&aeg, roots).run();

        // Run anti-unification
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

        debug!("cs: {:?}", cs);

        let all_libs: Vec<_> = learned_lib.libs().collect();
        let mut chosen_rewrites = Vec::new();
        for lib in &cs.set[0].libs {
            debug!("{}: {}", lib.0, &all_libs[lib.0 .0]);
            chosen_rewrites.push(lib_rewrites[lib.0 .0].clone());
        }

        debug!("chosen rewrites: {:?}", chosen_rewrites);

        let lifted = Learner::apply_libs(aeg.clone(), &roots, &chosen_rewrites);

        lifted
    }

    fn apply_libs<A>(
        egraph: EGraph<AstNode<Op>, A>,
        roots: &[Id],
        rewrites: &[Rewrite<AstNode<Op>, A>],
    ) -> RecExpr<AstNode<Op>>
    where
        Op: Clone
            + Teachable
            + Ord
            + Debug
            + Display
            + Hash
            + Arity
            + Send
            + Sync,
        A: Analysis<AstNode<Op>> + Default + Clone,
    {
        let mut fin = Runner::<_, _, ()>::new(Default::default())
            .with_egraph(egraph)
            .run(rewrites.iter())
            .egraph;
        let root = fin.add(AstNode::new(Op::list(), roots.iter().copied()));

        let mut extractor = LibExtractor::new(&fin);
        let best = extractor.best(root);
        Learner::lift_libs(best)
    }

    /// Lifts libs
    fn lift_libs(expr: RecExpr<AstNode<Op>>) -> RecExpr<AstNode<Op>>
    where
        Op: Clone + Teachable + Ord + Debug + Hash,
    {
        let orig: Vec<AstNode<Op>> = expr.as_ref().to_vec();
        let mut seen = HashMap::new();

        fn build<Op: Clone + Teachable + Debug>(
            orig: &[AstNode<Op>],
            cur: Id,
            mut seen: impl FnMut(LibId, Id),
        ) -> AstNode<Op> {
            match orig[Into::<usize>::into(cur)].as_binding_expr() {
                Some(BindingExpr::Lib(id, lam, c)) => {
                    seen(id, *lam);
                    build(orig, *c, seen)
                }
                _ => orig[Into::<usize>::into(cur)].clone(),
            }
        }

        let rest = orig[orig.len() - 1].build_recexpr(|id| {
            build(&orig, id, |k, v| {
                seen.insert(k, v);
            })
        });
        let mut res = rest.as_ref().to_vec();

        // Work queue for functions we still have to do
        let mut q: Vec<(LibId, Id)> = seen.iter().map(|(k, v)| (*k, *v)).collect();

        // TODO: order based on libs dependency w each other?
        while let Some((lib, expr)) = q.pop() {
            let body = res.len() - 1;
            let value: Vec<_> = orig[Into::<usize>::into(expr)]
                .build_recexpr(|id| {
                    build(&orig, id, |k, v| {
                        if let None = seen.insert(k, v) {
                            q.push((k, v));
                        }
                    })
                })
                .as_ref()
                .iter()
                .cloned()
                .map(|x| x.map_children(|x| (usize::from(x) + res.len()).into()))
                .collect();
            res.extend(value);
            res.push(Teachable::lib(lib, Id::from(res.len() - 1), Id::from(body)));
        }

        res.into()
    }
}