use super::*;

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    /// A union is a non-mimimal NFA with the same resulting states for every input as
    /// either of the two input NFAs.
    // Whatever invariant is enforced here, assume that the inputs have that invariant
    // Blindly copying states here allows M to vary widely
    #[tracing::instrument(skip(self, other))]
    pub fn union(&self, other: &Self) -> Self {

        let dfabuilder : DfaBuilder = todo!();
    todo!()
    }

    #[tracing::instrument(skip(self))]
    pub fn safe_add_edge(
        &mut self,
        edge: NfaEdge<E>,
        source: NodeId,
        target: NodeId,
    ) -> Vec<NodeId> {
        self.branch(&source, edge.criteria, self.node(target).clone())
    }

    #[allow(unused)]
    fn safe_add_node(&mut self, source: NodeId, kind: E, target_node: &NfaNode<M>) -> Vec<NodeId> {
        let existing_edges = self.edge_by_kind(source, &kind);
        if existing_edges.is_empty() {
            let new_node_id = self.add_node(target_node.clone());
            self.add_edge(NfaEdge { criteria: kind }, source, new_node_id);
            return vec![new_node_id];
        }
        let (existing_node, _existing_edge) = existing_edges[0];
        self.node_mut(existing_node).sum_mut(target_node);
        vec![existing_node]
    }

    /// Invariant preserving edge insert
    /// Ensures that edges from source remain consistent with a new edge to target
    /// Does not create any nodes
    // ? Should target instead be an a_id, b_id pair? to allow Ri - (Ri - L) trees to be merged ...
    pub fn branch_to_target(&mut self, source: &NodeId, kind: E, target: &NodeId) {
        /*
        1. Divide R into disjoint and overlapping sections
            - disjoint: R - L
            - overlapping: R - (R - L)
        2. Partition L into its remainder of L - R
        3. Make changes
            - Ri - L : (this is a piecemeal operation) retains the prior R subtrees (this is a move) (solo edge) downscope Ri to exclude L
            - L - R : This edge connects to target (solo edge)
            - Ri - (Ri - L): (this is a piecemeal operation) this must link to the appropriate target product node - what are the product ids or (a,b) ids here? the subtree should be the product subtree of the (.., target, ..) node along .. the Ri product node source (a,b) pair
        */
        todo!()
    }

    /// Invariant-preserving edge->node insert
    /// Ensures that the edges from source remain consistent (disjoint)
    /// Returns the ids of any added nodes
    #[tracing::instrument(skip(self), ret)]
    pub fn branch(
        &mut self,
        source: &NodeId,
        kind: E,
        // this node is instantiated, but not in the graph
        new_node: NfaNode<M>,
    ) -> Vec<NodeId> {
        println!("branching: {}", kind);
        todo!()
    }

    pub(crate) fn converging_copy(&mut self, source: &NodeId, targets: &[NodeId]) {
        // TODO: this must use branch like logic, but branch does not converge!
        let edges = self.edges_from(*source).clone();
        for (t, edge) in edges {
            // maybe use branch??
            let copy = self.add_node(self.node(t).clone());

            let c = self.edge(&edge).unwrap().clone();
            for target in targets {
                self.add_edge(c.clone(), *target, copy);
            }
            self.self_copy_subtree(&t, &copy);
        }
    }

    // there is no convergence yet but this is convergence-safe
    pub(crate) fn copy_subtree(
        &mut self,
        source: &Self,
        source_node: &NodeId,
        copy_target_node: &NodeId,
    ) {
        for (source_edge_endpoint, edge) in source.edges_from(*source_node) {
            // - create a new copy-target edge-target node matching the target of the source edge
            let new_edge_endpoint = self.add_node(source.node(*source_edge_endpoint).clone());
            // - get the weight and create a matching edge from target connecting to the new edge target node
            let _matching_edge = self.safe_add_edge(
                source.edge(edge).unwrap().clone(),
                new_edge_endpoint,
                *copy_target_node,
            );
            self.copy_subtree(source, source_edge_endpoint, &new_edge_endpoint);
        }
    }

    #[allow(dead_code)]
    #[tracing::instrument(skip(self))]
    pub(crate) fn self_copy_subtree(&mut self, source_node: &NodeId, destination_node: &NodeId) {
        for (target, edge) in self.edges_from(*source_node).clone() {
            println!("self_copy_subtree: {target} {edge}");

            let c = self.edge(&edge).unwrap().criteria.clone();

            for id in self.branch(destination_node, c, self.node(target).clone()) {
                self.self_copy_subtree(&target, &id);
            }
        }
    }
}

#[test]
fn test_union() {
    tests::setup();
    let nt = Element::not_tokens;
    let a = Nfa::from_symbols(&[nt(&['a']), nt(&['b'])], ());
    let b = Nfa::from_symbols(&[nt(&['c'])], ());
    // println!("from symbols {a:?}\n{b:?}");
    let n1 = a.union(&b);
    //n1.graphviz_file("unioned1.dot", "!a!b U !c");
    assert_eq!(n1.edges.len(), 5);
    let n2 = n1.union(&Nfa::from_symbols(&[Element::not_tokens(&['c'])], ()));
    //n2.graphviz_file("unioned2.dot", "!a U !b U !c");
    assert_eq!(n1, n2);
}
