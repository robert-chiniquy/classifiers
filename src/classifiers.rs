use super::*;

#[test]
fn test_basic_classifier() {
    let c1 = Classifier::<Dfa>::Literal("a*".to_string(), None);
    let c2 = Classifier::Literal("*a".to_string(), None);
    let c3 = Classifier::and(&[c1.clone(), c2.clone()]);
    let mut d = c3.compile(&None);
    d.simplify();
    d.graphviz_file("new-test.dot", "a* & *a");
    assert_eq!(c1.relation(&c2), Relation::Intersection);
}

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
    Universal,
    Literal(R::Language, Option<R::Metadata>),
    Not(Box<Classifier<R>>),
    /// Union
    // TODO: rename to Or() ?
    Any(BTreeSet<Classifier<R>>),
    /// Intersection
    And(BTreeSet<Classifier<R>>),
}

impl<R> Classifier<R>
where
    R: Relatable,
{
    #[tracing::instrument(skip_all)]
    pub fn relation(&self, other: &Self) -> Relation {
        let s: R = Classifier::compile(self, &None);
        let o = Classifier::compile(other, &None);

        let (relation, _work, _, _) = s.relation(&o);
        relation
    }

    #[tracing::instrument(skip_all)]
    pub fn not(c: Self) -> Self {
        Classifier::Not(Box::new(c))
    }

    #[tracing::instrument(skip_all)]
    pub fn and(items: &[Self]) -> Self {
        Classifier::And(BTreeSet::from_iter(items.iter().cloned()))
    }
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
            Classifier::Not(c) => Classifier::compile(c, m).complement(m),
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
}

// hack
impl std::fmt::Display for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}
