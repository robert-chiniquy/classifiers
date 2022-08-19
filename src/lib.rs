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
    fn relation<E, M, C>(&self, other: &Self) -> Relation
    where
        M: std::fmt::Debug + Clone + PartialOrd + Ord + Default,
        C: Into<E>,
        L: Iterator<Item = C> + NfaBuilder<L, M, E>,
        E: std::fmt::Debug + Clone + PartialEq + Eq + Universal + BranchProduct<E>,
    {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        // graphviz will help w/ this
        let s = Classifier::compile(&self, Default::default());
        let o = Classifier::compile(&other, Default::default());
        let i = s.intersection(&o);
        // if the number of Accept states in the intersection is < s, s has Accept states not in o
        // and the inverse for i < o
        // ... probably only works for a minimal NFA
        // if i has no Accept states, then s and o are disjoint
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
    fn compile<E, M, C>(c: &Self, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>>
    where
        E: std::fmt::Debug + Clone + Universal + BranchProduct<E> + Eq,
        C: Into<E>,
        L: Iterator<Item = C> + NfaBuilder<L, M, E>,
        M: std::fmt::Debug + Clone + PartialOrd + Ord + Default,
    {
        match c {
            Classifier::Universal => Nfa::universal(m),
            Classifier::Literal(l) => {
                let f = l.clone();
                let a: Nfa<NfaNode<M>, NfaEdge<E>> = Nfa::build_nfa(f, m);
                a
            }
            Classifier::Not(c) => Classifier::compile(c, m).negate(),
            Classifier::Any(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(Classifier::compile(acc, m.clone()), |acc, cur| {
                        acc.union(&Classifier::compile(cur, m.clone()))
                    })
                } else {
                    Nfa::universal(m).negate()
                }
            }
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    let acc = Classifier::compile(acc, m.clone());
                    items.fold(acc, |acc, cur| {
                        acc.intersection(&Classifier::compile(cur, m.clone()))
                    })
                } else {
                    Nfa::universal(m).negate()
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LRSemantics {
    L,
    R,
    LR,
    None,
}

impl Default for LRSemantics {
    fn default() -> Self {
        LRSemantics::None
    }
}

impl LRSemantics {
    fn sum(&self, other: &LRSemantics) -> LRSemantics {
        match (self, other) {
            (x, y) if x == y => x.clone(),
            (x, LRSemantics::None) | (LRSemantics::None, x) => x.clone(),
            (x, LRSemantics::LR) | (LRSemantics::LR, x) => LRSemantics::LR,
            (LRSemantics::R, LRSemantics::L) | (LRSemantics::L, LRSemantics::R) => LRSemantics::LR,
            (_, _) => unreachable!(),
        }
    }
}
