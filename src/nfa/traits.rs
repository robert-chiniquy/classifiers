/// E: Accepts<L> implies a C: Into<E> and L: IntoIterator<Item = C>
pub trait Accepts<L> {
    fn accepts(&self, l: L) -> bool;
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

pub trait BranchProduct<E> {
    fn product(a: &Self, b: &Self) -> Vec<NfaBranch<E>>;
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
