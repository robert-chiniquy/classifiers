use super::*;

pub trait NfaBuilder<E, M, C>
where
    E: Eq + std::hash::Hash + std::default::Default,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    fn build_nfa(s: Vec<C>, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>>;
}

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

// / This is the default impl of build_nfa for any type where this works
impl<E, M, C> NfaBuilder<E, M, C> for Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: Eq + Clone + std::hash::Hash + std::default::Default + std::fmt::Debug + BranchProduct<E>,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
    C: Into<E> + std::fmt::Debug,
{
    fn build_nfa(l: Vec<C>, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>> {
        // let l: Vec<C> = l.into_iter().collect();
        Nfa::from_language(l, m)
    }
}

// impl<E, M> NfaBuilder<E, M, E> for Nfa<NfaNode<M>, NfaEdge<E>>
// where
//     E: Eq + Clone + std::hash::Hash + std::default::Default + std::fmt::Debug,
//     M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
// {
//     fn build_nfa(l: Vec<E>, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>> {
//         // let l: Vec<C> = l.into_iter().collect();
//         Nfa::from_language(l, m)
//     }
// }

// impl<M, E> NfaBuilder<E, M, char> for Nfa<NfaNode<M>, NfaEdge<E>>
// where
//     E: Eq + Clone + std::hash::Hash + std::default::Default + From<char> + std::fmt::Debug,
//     M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
// {
//     // E could be an associated type but that would require an nfa builder for every E type
//     fn build_nfa(s: Vec<char>, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>> {
//         // let s: Vec<char> = s.chars().collect();
//         Nfa::from_language(s, m)
//     }
// }
