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
pub enum Classifier<R = Dfa>
where
    R: Relatable,
{
    /// Includes all things in the domain
    Universal,
    /// Includes a single thing in the domain
    Literal(R::Language, Option<R::Metadata>),
    /// Explicitly excludes anything included by a classifier
    // Exclude(Box<Classifier<R>>),
    /// Includes every item not included by a classifier
    Not(Box<Classifier<R>>),
    /// Union
    Or(BTreeSet<Classifier<R>>),
    /// Intersection
    And(BTreeSet<Classifier<R>>),
}

impl<R> Classifier<R>
where
    R: Relatable,
{
    #[tracing::instrument(skip(self))]
    pub fn compile(&self, m: &Option<R::Metadata>) -> R {
        match self {
            Classifier::Universal => R::universal(m),
            Classifier::Literal(l, m) => R::from_language(l, m),
            // TODO: prove out negation for exclusion rather than just as complementation
            Classifier::Not(c) => Classifier::compile(c, m).complement(m),
            Classifier::Or(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(Classifier::compile(acc, m), |acc, cur| {
                        acc.union(&Classifier::compile(cur, m))
                    })
                } else {
                    R::none(m)
                }
            }
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    let acc = Classifier::compile(acc, m);
                    items.fold(acc, |acc, cur| {
                        acc.intersection(&Classifier::compile(cur, m))
                    })
                } else {
                    R::none(m)
                }
            }
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn relation(&self, other: &Self) -> Relation {
        let s: R = Classifier::compile(self, &None);
        let o = Classifier::compile(other, &None);

        s.relation(&o)
    }

    // TODO: simplify()
    // pub fn simplify(&mut self) {}

    #[tracing::instrument(skip_all)]
    pub fn not(c: Self) -> Self {
        Classifier::Not(Box::new(c))
    }

    #[tracing::instrument(skip_all)]
    pub fn and(items: &[Self]) -> Self {
        Classifier::And(BTreeSet::from_iter(items.iter().cloned()))
    }

    #[tracing::instrument(skip_all)]
    pub fn or(items: &[Self]) -> Self {
        Classifier::Or(BTreeSet::from_iter(items.iter().cloned()))
    }
}

impl<M> Classifier<Dfa<M>>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    pub fn literal(s: &str) -> Self {
        Classifier::Literal(s.to_string(), None)
    }
}

// hack
impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}
