mod classifiers;
mod element;
mod graphviz;
mod nfa;
mod tests;

pub use crate::classifiers::*;
pub use element::*;
pub use graphviz::*;
pub use nfa::*;

use std::collections::BTreeSet;

pub(crate) fn str_to_chars(s: &str) -> Vec<char> {
    s.to_string().chars().into_iter().collect()
}
