mod nfa;
mod tests;

pub use nfa::*;

use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    Universal,
    Literal(L),
    Not(Box<Classifier<L>>),
    Any(BTreeSet<Classifier<L>>),
    And(BTreeSet<Classifier<L>>),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Relation {
    Disjoint,
    Intersection,
    Subset,
    Superset,
    Equality,
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    #[tracing::instrument(skip_all)]
    fn relation(&self, other: &Self) -> Relation {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        // graphviz will help w/ this
        todo!()
    }

    #[tracing::instrument(skip_all)]
    fn not(c: Classifier<L>) -> Self {
        Classifier::Not(Box::new(c))
    }
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug + std::hash::Hash + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    #[tracing::instrument(skip_all)]
    fn compile<E>(&self) -> Nfa<NfaNode<()>, NfaEdge<E>>
    where
        E: std::fmt::Debug + Clone + Universal + BranchProduct<E> + Eq,
        L: Into<Nfa<NfaNode<()>, NfaEdge<E>>>,
    {
        match self {
            Classifier::Universal => Nfa::universal(),
            Classifier::Literal(l) => l.clone().into(),
            Classifier::Not(c) => c.compile().negate(),
            Classifier::Any(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(acc.compile(), |acc, cur| acc.union(&cur.compile()))
                } else {
                    Nfa::universal().negate()
                }
            }
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(acc.compile(), |acc, cur| acc.intersection(&cur.compile()))
                } else {
                    Nfa::universal().negate()
                }
            }
        }
    }
}
