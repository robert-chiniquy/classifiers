use super::*;

pub trait Relatable: std::fmt::Debug + Clone + PartialOrd + Ord {
    type Language: std::fmt::Debug + Clone + PartialOrd + Ord;
    type Element: ElementalLanguage<Self::Element>;

    type Metadata: std::fmt::Debug + Clone + PartialOrd + Ord;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self;

    fn relation(&self, other: &Self) -> Relation;

    fn union(&self, other: &Self) -> Self;
    fn intersection(&self, other: &Self) -> Self;

    fn complement(&self, m: &Option<Self::Metadata>) -> Self;
    fn negate(&self) -> Self;

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

pub trait Accepts<E> {
    fn accepts(&self, l: &E) -> bool;
}
