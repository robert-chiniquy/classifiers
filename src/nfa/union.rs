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
        println!("union!");
        let mut union: Self = self.clone();
        let mut stack = vec![(union.entry, other.entry)];

        // The stack loop covers subtrees which require smashing at any given point,
        // subtrees which diverge will be copied.
        // This is order sensitive if there are multiple entry nodes which mostly does not happen
        // (and they are stored in a vec)
        // Cycle detection can occur by tracking visited combinations on the stack
        let mut visited: HashSet<_> = Default::default();
        while let Some((union_id, other_id)) = stack.pop() {
            if visited.contains(&(union_id, other_id)) {
                // should this also validate working node id?
                continue;
            }
            visited.insert((union_id, other_id));
            // smash these nodes together
            // if no edges from either, consider them smashed
            // where edges match from other to union, push the edge targets on the stack
            // where edges do not match, create a new edge of the appropriate (other) kind and copy subtree to it.
            //
            let union_edges = union.edges_from(union_id);
            let other_edges = other.edges_from(other_id);

            if other_edges.is_empty() {
                continue;
            }

            if union_edges.is_empty() {
                println!("adding in body subtree!: {:?}", &other_edges);
                union.copy_subtree(other, &other_id, &union_id);
                continue;
            }

            println!(
                "looking at other edges: {:?}\n{:?}\n{:?}",
                other_edges, union, other
            );
            for (other_edge_target, other_edge_id) in other_edges {
                println!(
                    "node we give to branch: {:?}",
                    other.node(*other_edge_target).clone()
                );
                // is there a matching edge in union?
                let next_ids = union.branch(
                    &union_id,
                    other.edge(other_edge_id).unwrap().criteria.clone(),
                    other.node(*other_edge_target).clone(),
                );

                for id in next_ids {
                    stack.push((id, *other_edge_target));
                }
            }
        }
        union
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
        /*
        # Rules for intersection play out for all relations so long as you ignore empty edge results ø

        1. Divide R into disjoint and overlapping sections
           - disjoint: R - L
           - overlapping: R - (R - L)
        2. Partition L into its remainder of L - R
        3. Make changes
          - Add (or modify if in an existing tree) edges for:
            - Ri - L : (this is a piecemeal operation) retains the prior R subtrees (this is a move)
            - L - R : proceed to copy new subtree (new subtree)
            - Ri - (Ri - L): (this is a piecemeal operation) proceed to copy new subtree and copy prior subtree (copy old and add new)

        # Implementation
        1. Add the weights of all extant edges (this is R)
        2. Loop over all extant edges and calculate Ri - L, if it exists, reduce scope of edge and just retain the subtree there
        2a. Add an edge for Ri - (Ri - L) if it exists and sum both subtrees there
        3. Add an edge for L - R if it exists and copy the new subtree there
        */

        // 1
        // let existing_edge_weights: Vec<E> = self
        //     .edges_from(*source)
        //     .iter()
        //     .map(|(_, e)| self.edge(e).unwrap().criteria.clone())
        //     .collect();

        // println!("existing_edge_weights {:?}", existing_edge_weights);
        // if existing_edge_weights.is_empty() {
        //     // just add edge and return
        //     let node = self.add_node(new_node);
        //     let _edge = self.add_edge(NfaEdge { criteria: kind }, *source, node);
        //     return vec![node];
        // }
        // let initial = existing_edge_weights[0].clone();
        // let weights_sum = existing_edge_weights[1..]
        //     .iter()
        //     .fold(initial, |acc, cur| acc + cur.clone());

        // println!("weights_sum: {:?}", weights_sum);
        // let mut node_ids = vec![];

        // let edges = self.edges_from(*source).clone();
        // for (target, e) in edges {
        //     let edge_criteria = self.edge(&e).unwrap().criteria.clone();
        //     // Ri - L
        //     let difference = E::difference(&edge_criteria, &kind);
        //     println!(
        //         "difference: {:?} = {} - {}",
        //         difference,
        //         &self.edge(&e).unwrap().criteria,
        //         &kind
        //     );
        //     match difference {
        //         EDifference::E(d) => {
        //             // 2a
        //             // Ri - (Ri - L)
        //             match E::difference(&edge_criteria, &d) {
        //                 EDifference::E(r_r_l) => {
        //                     let r_r_l_node =
        //                         self.add_node_with_edge(r_r_l, *source, new_node.clone());
        //                     node_ids.push(r_r_l_node);
        //                     // copy the edge target subtree under the new node
        //                     self.self_copy_subtree(&target, &r_r_l_node);
        //                 }
        //                 EDifference::ToStar(r_r_l) => {
        //                     let r_r_l_node =
        //                         self.add_node_with_edge(r_r_l, *source, new_node.clone());
        //                     node_ids.push(r_r_l_node);
        //                     let star_target = self.add_node_with_edge(
        //                         E::universal(),
        //                         r_r_l_node,
        //                         new_node.clone(),
        //                     );
        //                     node_ids.push(star_target);
        //                     self.converging_copy(&target, &[r_r_l_node, star_target]);
        //                     // As r_r_l_node is ToStar, self_copy_subtree must respect branch logic
        //                     // copy the edge target subtree under the new node
        //                     // self.self_copy_subtree(&target, &r_r_l_node);
        //                     // // copy also under the star target
        //                     // // FIXME: convergence
        //                     // self.self_copy_subtree(&target, &star_target);
        //                 }
        //                 // If Ri - (Ri - L) is ø, do nothing
        //                 EDifference::None => (),
        //             }
        //             // 2
        //             // reduce the scope of edge and retain the subtree there
        //             self.edge_mut(e).unwrap().criteria = d;
        //         }
        //         EDifference::ToStar(d) => {
        //             // L add: *
        //             // R have: a
        //             // want: !a -> *, a -> *, !a
        //             // L - R:  * - a = {!a, !a -> *} .. push both node IDs into node_ids to achieve this effect
        //             // Ri - L: a - * = ø
        //             // Ri - (Ri - L): a - (a - *) = a
        //             // ..
        //             // what if Ri - L is ToStar
        //             // L add: a
        //             // R have: *
        //             // want: !a -> *, a -> *, !a, a
        //             // L - R: a - * = ø
        //             // Ri - L: * - a = {!a, !a -> *} .. push both node IDs
        //             // Ri - (Ri - L): a - ({!a, !a -> *}) = {a, a -> *}, the ToStar is unrelated to an individual edge eval
        //             // Ri - (Ri - L) for ToStar:
        //             match E::difference(&edge_criteria, &d) {
        //                 EDifference::E(r_r_l) => {
        //                     let r_r_l_node =
        //                         self.add_node_with_edge(r_r_l, *source, new_node.clone());
        //                     node_ids.push(r_r_l_node);
        //                     // copy the edge target subtree under the new node
        //                     self.self_copy_subtree(&target, &r_r_l_node);
        //                 }
        //                 EDifference::ToStar(r_r_l) => {
        //                     let r_r_l_node =
        //                         self.add_node_with_edge(r_r_l, *source, new_node.clone());
        //                     node_ids.push(r_r_l_node);
        //                     let star_target = self.add_node_with_edge(
        //                         E::universal(),
        //                         r_r_l_node,
        //                         new_node.clone(),
        //                     );
        //                     node_ids.push(star_target);
        //                     self.converging_copy(&target, &[r_r_l_node, star_target]);
        //                 }
        //                 // If Ri - (Ri - L) is ø, do nothing
        //                 EDifference::None => (),
        //             }
        //             // reduce the scope of edge and retain the subtree there
        //             self.edge_mut(e).unwrap().criteria = d;
        //             self.node_mut(target).sum(&new_node);
        //             // target must have a star branch added to it, as d is ToStar
        //             // this must respect branch logic as it is an existing subtree
        //             node_ids.push(target);
        //             // TODO: this logic is incorrect....
        //             // let n = self.add_node(new_node.clone());
        //             // self.add_edge(NfaEdge { criteria: Universal::universal() }, target, n);
        //             // node_ids.push(n);
        //             node_ids.extend(self.branch(&target, Universal::universal(), new_node.clone()));
        //         }
        //         EDifference::None => {
        //             // 2a (None case) Ri - L = ø, Ri - (Ri - L) = Ri
        //             // The Ri edge already exists, so just do a sum_mut on that edge
        //             self.node_mut(target).sum_mut(&new_node.clone());
        //             node_ids.push(target);
        //         }
        //     }
        // }

        // // 3
        // match E::difference(&kind, &weights_sum) {
        //     EDifference::E(l_r) => {
        //         node_ids.push(self.add_node_with_edge(l_r, *source, new_node));
        //     }
        //     EDifference::ToStar(l_r) => {
        //         let l_r = self.add_node_with_edge(l_r, *source, new_node.clone());
        //         node_ids.push(l_r);
        //         let star_target = self.add_node_with_edge(E::universal(), l_r, new_node.clone());
        //         node_ids.push(star_target);
        //     }
        //     EDifference::None => (),
        // }

        // node_ids
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
