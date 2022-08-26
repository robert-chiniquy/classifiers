use super::*;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Relation {
    Disjoint,
    Intersection,
    Subset,
    Superset,
    Equality,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    Universal,
    Literal(L),
    Not(Box<Classifier<L>>),
    /// Union
    Any(BTreeSet<Classifier<L>>),
    /// Intersection
    And(BTreeSet<Classifier<L>>),
}

// TODO: Check membership
impl<L> Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    #[tracing::instrument(skip_all)]
    pub fn relation<E, C>(&self, other: &Self) -> Relation
    where
        C: Into<E> + std::fmt::Debug + Eq + std::hash::Hash + Default,
        L: IntoIterator<Item = C>,
        Vec<E>: Invertible,
        E: Accepts<E>,
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
        let mut s: Nfa<NfaNode<()>, NfaEdge<_>> = Classifier::compile(self, ());
        let mut o = Classifier::compile(other, ());

        s.set_chirality(LRSemantics::L);
        o.set_chirality(LRSemantics::R);
        let union = s.union(&o);
        let paths = union.accepting_paths();
        match (
            !paths.l.is_empty(),
            !paths.lr.is_empty(),
            !paths.r.is_empty(),
        ) {
            (true, true, true) => Relation::Intersection,
            (true, true, false) => Relation::Superset,
            (true, false, true) => Relation::Disjoint,
            (true, false, false) => Relation::Disjoint,
            (false, true, true) => Relation::Subset,
            (false, true, false) => Relation::Equality,
            (false, false, true) => Relation::Disjoint,
            (false, false, false) => Relation::Disjoint,
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn not(c: Classifier<L>) -> Self {
        Classifier::Not(Box::new(c))
    }

    #[tracing::instrument]
    pub fn and(items: &[Classifier<L>]) -> Self {
        Classifier::And(BTreeSet::from_iter(items.iter().cloned()))
    }
}

impl Classifier<Vec<char>> {
    pub fn literal(s: &str) -> Self {
        Classifier::Literal(str_to_chars(s))
    }
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug + std::hash::Hash + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    // TODO: make Element default? or more inferable
    #[tracing::instrument(skip_all)]
    pub fn compile<E, M, C>(&self, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>>
    where
        E: std::fmt::Debug
            + Clone
            + Universal
            + BranchProduct<E>
            + Eq
            + std::hash::Hash
            + std::default::Default,
        E: Accepts<E>,
        C: Into<E> + std::fmt::Debug + Eq + std::hash::Hash + Default,
        Nfa<NfaNode<M>, NfaEdge<E>>: NfaBuilder<E, M, C>,
        L: IntoIterator<Item = C>,
        M: std::fmt::Debug + Clone + PartialOrd + Ord + Default,
        Vec<E>: Invertible,
    {
        match self {
            Classifier::Universal => Nfa::universal(m),
            Classifier::Literal(l) => {
                let arg = l.clone().into_iter().collect();
                Nfa::build_nfa(arg, m)
            }
            // NotToken?
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
            // how does this treat heterogenous states?
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    let acc = Classifier::compile(acc, m.clone());
                    items.fold(acc, |acc, cur| {
                        // An intersection() method on Classifiers first which may delegate to Nfa::intersection() ? for heterogenous structures
                        acc.intersection(&Classifier::compile(cur, m.clone()))
                        // if we needed a conjunction state on all terminal states in the intersection,
                        // we could mutate and add it here, or does it happen in intersection?
                    })
                } else {
                    Nfa::universal(m).negate()
                }
            }
        }
    }
}
