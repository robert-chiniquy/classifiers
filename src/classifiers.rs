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

#[test]
fn test_basic_classifier() {
    let c1 = Classifier::<DFA<()>>::Literal("a*".to_string(), None);
    let c2 = Classifier::Literal("*a".to_string(), None);
    let c3 = Classifier::and(&[c1, c2]);
    let mut d = c3.compile(&None);
    d.simplify();
    d.graphviz_file("new-test.dot", "a* & *a");
    // assert_eq!(c1.relation(&c2), Relation::Equality);
}


impl<R> Classifier<R>
where
    R: Relatable,
{
    #[tracing::instrument(skip_all)]
    pub fn relation(&self, other: &Self) -> Relation
    {
        // 1. compile
        // 2. relate NFAs (product of NFAs, search terminal states)
        let s: R = Classifier::compile(self, &None);
        let o = Classifier::compile(other, &None);

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
            Classifier::Not(c) => Classifier::compile(c, m).negate(m),
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
