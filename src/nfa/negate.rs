use super::*;

impl<M, E> Nfa<NfaNode<M>, E>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
    E: std::fmt::Debug + Clone + Eq + std::hash::Hash + std::default::Default,
{
    #[tracing::instrument]
    pub fn negate(&self) -> Self {
        // a NotToken here would need to flip edges as well
        // TODO: negate edges too
        let mut nfa: Nfa<NfaNode<M>, E> = self.clone();
        nfa.nodes = nfa
            .nodes
            .into_iter()
            .map(|(i, n)| (i, n.negate()))
            .collect();
        nfa
    }
}
