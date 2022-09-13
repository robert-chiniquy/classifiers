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
pub enum Classifier<R>
where
    R: Relatable,
{
    Universal,
    Literal(R::Language, Option<R::Metadata>),
    Not(Box<Classifier<R>>),
    /// Union
    Any(BTreeSet<Classifier<R>>),
    /// Intersection
    And(BTreeSet<Classifier<R>>),
}

/*
Policy Statement 1
Classifier C1

PS2
C2

What is the relation between C1 and C2 ? 
If one is allow, another is deny, that is a concern at the call site

Classifier::Literal("s3:*")
Classifier::Literal("s3:Get*")



*/

impl<R> Classifier<R>
where
    R: Relatable,
{
    #[tracing::instrument(skip_all)]
    pub fn relation(&self, other: &Self) -> Relation
    {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        let mut s: R = Classifier::compile(self, &None);
        let mut o = Classifier::compile(other, &None);

        let (relation, _work) = s.relation(&o);
        relation
    }

    #[tracing::instrument(skip_all)]
    pub fn not(c: Self) -> Self {
        Classifier::Not(Box::new(c))
    }

    #[tracing::instrument]
    pub fn and(items: &[Self]) -> Self {
        Classifier::And(BTreeSet::from_iter(items.iter().cloned()))
    }
}

// impl Classifier<Vec<char>> {
//     pub fn literal(s: &str) -> Self {
//         Classifier::Literal(str_to_chars(s))
//     }
// }

impl<R> Classifier<R> 
where
    R: Relatable,
{
    #[tracing::instrument(skip_all)]
    pub fn compile(&self, m: &Option<R::Metadata>) -> R
    {
        match self {
            Classifier::Universal => R::universal(m),
            Classifier::Literal(l, m) => {
                R::from_language(l, m)
            }
            Classifier::Not(c) => Classifier::compile(c, m).negate(),
            Classifier::Any(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    items.fold(Classifier::compile(acc, m), |acc, cur| {
                        acc.union(&Classifier::compile(cur, m))
                    })
                } else {
                    R::none(m)
                }
            }
            // how does this treat heterogenous states?
            Classifier::And(v) => {
                let mut items = v.iter();
                if let Some(acc) = items.next() {
                    let acc = Classifier::compile(acc, m);
                    items.fold(acc, |acc, cur| {
                        // An intersection() method on Classifiers first which may delegate to R::intersection() ? for heterogenous structures
                        acc.intersection(&Classifier::compile(cur, m))
                        // if we needed a conjunction state on all terminal states in the intersection,
                        // we could mutate and add it here, or does it happen in intersection?
                    })
                } else {
                    R::none(m)
                }
            }
        }
    }
}
