/// E: Accepts<L> implies a C: Into<E> and L: IntoIterator<Item = C>
pub trait Accepts<L> {
    fn accepts(&self, l: L) -> Result<bool, MatchingError>;
}

pub trait Remaindery<L> {
    fn remainder(a: &L, b: &L) -> Result<Option<L>, String>;

    fn is_valid(a: &L, b: &L) -> bool;
}

pub trait Complement<L>
where
    Self: Sized,
{
    fn complement(&self) -> Option<Self>;
}

pub trait Universal {
    fn universal() -> Self;
}

pub trait NodeSum {
    fn sum(&self, other: &Self) -> Self;
    fn sum_mut(&mut self, other: &Self);
}

// This should be implemented on a path of E
pub trait Invertible
where
    Self: Sized,
{
    fn inverse(&self) -> Vec<Self>;
}

#[derive(Debug)]
pub enum MatchingError {
    Error(String),
    UnrollLeft,
    UnrollRight,
    UnrollLeftRight,
}

impl std::fmt::Display for MatchingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}

pub trait BranchProduct<E> {
    fn product(a: &Self, b: &Self) -> Result<Vec<NfaBranch<E>>, MatchingError>;
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
