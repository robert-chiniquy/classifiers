use super::*;

impl<M> Relatable for DfaBuilder<M> 
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    type Language = String;

    type Element = Element;

    type Metadata = M;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self {
      let dfa = DfaBuilder::from_language(l.chars().collect(), m);

        todo!()
    }

    fn relation(&self, other: &Self) -> (Relation, Self) {
        todo!()
    }

    fn universal(m: &Option<Self::Metadata>) -> Self {
        todo!()
    }

    fn none(m: &Option<Self::Metadata>) -> Self {
        todo!()
    }

    fn negate(&self) -> Self {
        todo!()
    }

    fn union(&self, other: &Self) -> Self {
        todo!()
    }

    fn intersection(&self, other: &Self) -> Self {
        todo!()
    }

}