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
    fn relation<M, E>(&self, other: &Self) -> Relation
    where
        M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
        L: Into<Nfa<NfaNode<M>, NfaEdge<E>>>,
        E: std::fmt::Debug + Clone + PartialEq + Eq + Universal + BranchProduct<E>,
    {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        // graphviz will help w/ this
        let s: Nfa<NfaNode<M>, NfaEdge<E>> = self.compile();
        let o = other.compile();
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
    fn compile<M, E>(&self, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>>
    where
        E: std::fmt::Debug + Clone + Universal + BranchProduct<E> + Eq,
        L: Into<Nfa<NfaNode<M>, NfaEdge<E>>>,
        M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
    {
        match self {
            Classifier::Universal => Nfa::universal(),
            Classifier::Literal(l) => l.clone().into(),
            Classifier::Not(c) => c.compile(m).negate(),
            Classifier::Any(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(acc.compile(m), |acc, cur| acc.union(&cur.compile(m)))
                } else {
                    Nfa::universal().negate()
                }
            }
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(acc.compile(m), |acc, cur| acc.intersection(&cur.compile(m)))
                } else {
                    Nfa::universal().negate()
                }
            }
        }
    }
}

// For use in functions which need to sort M
pub enum LRSemantics<M> {
    L(Box<M>),
    R(Box<M>),
    LR(Box<M>),
    None(Box<M>),
}

impl<M> LRSemantics<M> {
    pub fn m(&self) -> M {
        match self {
            LRSemantics::L(m) | LRSemantics::R(m) | LRSemantics::LR(m) | LRSemantics::None(m) => {
                **m
            }
        }
    }
}

impl<M> From<LRSemantics<M>> for M {
    fn from(s: LRSemantics<M>) -> Self {
        s.m()
    }
}
