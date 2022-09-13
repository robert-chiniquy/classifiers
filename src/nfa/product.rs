use std::collections::HashMap;

use super::*;

#[derive(Default)]
struct ProductConstruction {
    // p = a * b
    // (a node id, b node id) -> product id
    product_nodes: HashMap<(Option<NodeId>, Option<NodeId>), NodeId>,
}

impl ProductConstruction {
    pub fn product_construction<M, E>(
        &mut self,
        a: &Nfa<NfaNode<M>, NfaEdge<E>>,
        b: &Nfa<NfaNode<M>, NfaEdge<E>>,
    ) -> Self
    where
        E: ElementalLanguage<E>,
        M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
    {
        todo!()
    }
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    
    pub fn product(&self, other: &Self) -> Self {
        let mut product: Self = Default::default();
        todo!()
    }
}
