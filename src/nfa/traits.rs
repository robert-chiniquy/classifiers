use std::ops::Add;

use super::*;


// 6 Relatables
// 1,2,3,4,5,6
// 1v2, 1v3, ..
// 2,3, 2v4, ..
// 1 > 2, 1 > 3, 2v3 - the work to do 2v3 was not done in 1v2 or 1v3
// 1 ¥ 2, 1 ¥ 3, 2v3? 

pub trait Relatable : std::fmt::Debug + Clone + PartialOrd + Ord {
    type Language: std::fmt::Debug + Clone + PartialOrd + Ord;
    type Element: ElementalLanguage<Self::Element>;
    type Metadata: std::fmt::Debug + Clone + PartialOrd + Ord + Default;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self;

    // TODO(soon): Determine how to re-use the data computed in relation()
    // ^ nm, rolling with the tuple return for now, see how it works out
    // fn relation(&self, other: &Self) -> Relation;
    /// Produces a Relation describing self v other 
    /// as well as a re-usable Relatable capturing the relationship
    fn relation(&self, other: &Self) -> (Relation, Self);

    fn universal(m: &Option<Self::Metadata>) -> Self;
    fn none(m: &Option<Self::Metadata>) -> Self;

    fn negate(&self) -> Self;
    fn union(&self, other: &Self) -> Self;
    fn intersection(&self, other: &Self) -> Self;    
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
    + Default
    + Eq
    + std::fmt::Debug
    + std::fmt::Display
    + std::hash::Hash
    + Accepts<E>
    + Complement<E>
    + Subtraction<E>
    + Add
    + std::ops::Add<Output = E>
    + Product<E>
    + Universal
    + FromLanguage<E>
where
    E: Eq + std::hash::Hash + Default + PartialEq,
{
}

pub trait FromLanguage<E>
where
    E: Eq + std::hash::Hash + Default + PartialEq,
{
    type Language;
    type Metadata: std::fmt::Debug + Clone + Ord + PartialOrd + Default + PartialEq + Eq;
    fn from_language(
        l: Self::Language,
        m: Self::Metadata,
    ) -> Nfa<NfaNode<Self::Metadata>, NfaEdge<E>>;
}

pub trait Accepting {
    fn accepting(&self) -> bool;
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

pub trait Product<E> {
    fn product(a: &E, b: &E) -> Vec<NfaBranch<E>>;
}

pub trait NodeSum {
    fn sum(&self, other: &Self) -> Self;
    fn sum_mut(&mut self, other: &Self);
}

#[derive(Debug, PartialEq, Eq)]
pub enum EdgeTransition {
    Advance,
    Stay,
    Stop,
}

#[derive(Debug)]
pub struct NfaBranch<El> {
    pub(crate) kind: El,
    pub(crate) left: EdgeTransition,
    pub(crate) right: EdgeTransition,
}

impl<E: Clone> NfaBranch<E> {
    pub fn new(kind: E, left: EdgeTransition, right: EdgeTransition) -> Self {
        debug_assert!(!(left == EdgeTransition::Stop && right == EdgeTransition::Stop));
        Self { kind, left, right }
    }
}
