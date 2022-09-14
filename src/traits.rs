use std::ops::Add;

use super::*;

pub trait Relatable: std::fmt::Debug + Clone + PartialOrd + Ord {
    type Language: std::fmt::Debug + Clone + PartialOrd + Ord;
    type Element: ElementalLanguage<Self::Element>;

    type Metadata: std::fmt::Debug + Clone + PartialOrd + Ord;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self;

    // TODO(soon): Determine how to re-use the data computed in relation()
    /// Produces a Relation describing self v other
    /// as well as a re-usable Relatable capturing the relationship
    fn relation(&self, other: &Self) -> (Relation, Self);

    fn union(&self, other: &Self) -> Self;
    fn intersection(&self, other: &Self) -> Self;
    fn negate(&self, m: &Option<Self::Metadata>) -> Self;

    fn universal(m: &Option<Self::Metadata>) -> Self;
    fn none(m: &Option<Self::Metadata>) -> Self;
}

#[derive(Debug)]
pub enum GeneralError {
    Error(String),
}

impl From<String> for GeneralError {
    fn from(s: String) -> Self {
        GeneralError::Error(s)
    }
}

impl std::fmt::Display for GeneralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}

pub trait ElementalLanguage<E>:
    Clone
    + Eq
    + std::fmt::Debug
    + std::fmt::Display
    + std::hash::Hash
    + Accepts<E>
    + Complement<E>
    + Subtraction<E>
    + Add
    + std::ops::Add<Output = E>
    + Universal
where
    E: Eq + std::hash::Hash + PartialEq,
{
}

/// E: Accepts<L> implies a C: Into<E> and L: IntoIterator<Item = C>
pub trait Accepts<E> {
    fn accepts(&self, l: &E) -> bool;
}

pub trait Complement<E>
where
    Self: Sized,
{
    fn complement(&self) -> Option<Self>;
}

pub trait Subtraction<E> {
    fn difference(a: &E, b: &E) -> E;
}

pub trait Universal {
    fn universal() -> Self;
}
