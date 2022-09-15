use super::*;

pub trait Relatable: std::fmt::Debug + Clone + PartialOrd + Ord {
    type Language: std::fmt::Debug + Clone + PartialOrd + Ord;
    type Element: ElementalLanguage<Self::Element>;

    type Metadata: std::fmt::Debug + Clone + PartialOrd + Ord;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self;

    // TODO(soon): Determine if/how to re-use the data computed in relation()
    // FIXME: CompoundId doesn't belong in this trait
    /// Produces a Relation describing self v other
    /// as well as a re-usable Relatable capturing the relationship
    fn relation(
        &self,
        other: &Self,
    ) -> (Relation, Self, BTreeSet<CompoundId>, BTreeSet<CompoundId>);

    fn union(&self, other: &Self) -> Self;
    fn intersection(&self, other: &Self) -> Self;
    fn negate(&self, m: &Option<Self::Metadata>) -> Self;

    fn universal(m: &Option<Self::Metadata>) -> Self;
    fn none(m: &Option<Self::Metadata>) -> Self;
}

pub trait ElementalLanguage<E>:
    Clone
    + Eq
    + std::fmt::Debug
    + std::fmt::Display
    + std::hash::Hash
    + Accepts<E>
    + std::ops::Add<Output = E>
where
    E: Eq + std::hash::Hash + PartialEq,
{
    fn difference(a: &E, b: &E) -> E;
    fn universal() -> Self;
}

/// E: Accepts<L> implies a C: Into<E> and L: IntoIterator<Item = C>
pub trait Accepts<E> {
    fn accepts(&self, l: &E) -> bool;
}
