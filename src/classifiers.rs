use super::*;

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
    pub fn relation<E, C>(&self, other: &Self) -> Relation
    where
        C: Into<E> + std::fmt::Debug + Eq + std::hash::Hash + Default,
        L: IntoIterator<Item = C>,
        // <L as std::iter::IntoIterator>::IntoIter: nfa::Language,
        E: std::fmt::Debug
            + Clone
            + PartialEq
            + Eq
            + Universal
            + BranchProduct<E>
            + std::hash::Hash
            + std::default::Default,
    {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        // graphviz will help w/ this
        let s: Nfa<NfaNode<()>, NfaEdge<_>> = Classifier::compile(self, ());
        let o = Classifier::compile(other, ());
        let i = s.intersection(&o);
        // if the number of Accept states in the intersection is < s, s has Accept states not in o
        // and the inverse for i < o
        // ... probably only works for a minimal NFA
        // if i has no Accept states, then s and o are disjoint
        todo!()
    }

    #[tracing::instrument(skip_all)]
    pub fn not(c: Classifier<L>) -> Self {
        Classifier::Not(Box::new(c))
    }
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug + std::hash::Hash + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    #[tracing::instrument(skip_all)]
    pub fn compile<E, M, C>(c: &Self, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>>
    where
        E: std::fmt::Debug
            + Clone
            + Universal
            + BranchProduct<E>
            + Eq
            + std::hash::Hash
            + std::default::Default,
        C: Into<E> + std::fmt::Debug + Eq + std::hash::Hash + Default,
        Nfa<NfaNode<M>, NfaEdge<E>>: NfaBuilder<E, M, C>,
        L: IntoIterator<Item = C>,
        // <L as std::iter::IntoIterator>::IntoIter: nfa::Language,
        M: std::fmt::Debug + Clone + PartialOrd + Ord + Default,
    {
        match c {
            Classifier::Universal => Nfa::universal(m),
            Classifier::Literal(l) => {
                let arg = l.clone().into_iter().collect();
                Nfa::build_nfa(arg, m)
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
    pub(crate) fn sum(&self, other: &LRSemantics) -> LRSemantics {
        match (self, other) {
            (x, y) if x == y => x.clone(),
            (x, LRSemantics::None) | (LRSemantics::None, x) => x.clone(),
            (x, LRSemantics::LR) | (LRSemantics::LR, x) => LRSemantics::LR,
            (LRSemantics::R, LRSemantics::L) | (LRSemantics::L, LRSemantics::R) => LRSemantics::LR,
            (_, _) => unreachable!(),
        }
    }
}
