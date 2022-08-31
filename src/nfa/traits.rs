/// E: Accepts<L> implies a C: Into<E> and L: IntoIterator<Item = C>
pub trait Accepts<L> {
    fn accepts(&self, l: L) -> Result<bool, GeneralError>;
}

pub trait Complement<L>
where
    Self: Sized,
{
    fn complement(&self) -> Option<Self>;
}

pub trait Remaindery<L> {
    fn remainder(a: &L, b: &L) -> Result<Option<L>, String>;

    fn is_valid(a: &L, b: &L) -> bool;
}

pub trait Universal {
    fn universal() -> Self;
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
    + Remaindery<E>
    + Universal
{
    fn product(a: &Self, b: &Self) -> Result<Vec<NfaBranch<E>>, GeneralError>;
}

pub trait NodeSum {
    fn sum(&self, other: &Self) -> Self;
    fn sum_mut(&mut self, other: &Self);
}

// This should be implemented on a path of E
// pub trait Invertible
// where
//     Self: Sized,
// {
//     fn inverse(&self) -> Vec<Self>;
// }

#[derive(Debug)]
pub enum GeneralError {
    Error(String),
    // UnrollLeft,d
    // UnrollRight,
    // UnrollLeftRight,
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
