use crate::{
    beam::PartialLibCost,
    sexp::Program,
    ast_node::{Arity, AstNode, Expr, Pretty, Printable},
    lang::SimpleOp,
    teachable::Teachable,
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
use egg::{EGraph, Id, RecExpr, Rewrite, Runner};

// A set of expressions to extract a library
pub struct Learner<Op> {
    exprs: Vec<Expr<Op>>,
    dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>,
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
        }
    }

    /// Adds all the experiments from another experiment set into this one
    pub fn add(&mut self, other: Self) {
        self.dsrs.extend(other.dsrs);
        self.exprs.extend(other.exprs);
    }


    /// Generate a new learner 
    pub fn gen(exprs: Vec<Expr<Op>>, dsrs: Vec<Rewrite<AstNode<Op>, PartialLibCost>>) -> Self
    {
        Self {dsrs, exprs}
    }

    /// Runs learning
    pub fn learn(self, csv_path: &str) {
        let file = std::fs::File::create(csv_path).unwrap();
        let mut writer: CsvWriter = csv::Writer::from_writer(Box::new(file));

        // To-Do: Do library learning
        ()
    }
}