use std::ops::Add;

use super::*;

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
