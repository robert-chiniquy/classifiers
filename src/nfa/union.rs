use super::*;

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: Eq
        + Clone
        + std::hash::Hash
        + Default
        + std::fmt::Debug
        + BranchProduct<E>
        + std::fmt::Display,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    /// A union is a non-mimimal NFA with the same resulting states for every input as
    /// either of the two input NFAs.
    // Whatever invariant is enforced here, assume that the inputs have that invariant
    // Blindly copying states here allows M to vary widely
    #[tracing::instrument(skip(self, other))]
    pub fn union(&self, other: &Self) -> Self {
        if self.entry.is_empty() {
            return other.clone();
        }

        if other.entry.is_empty() {
            return self.clone();
        }

        let mut union: Self = self.clone();
        // println!("I union stuff: {} {} {}", self.nodes.len(), other.nodes.len(), union.nodes.len());
        // let _entry = union.add_node(Default::default());

        // for every edge from every entry node in other
        // find a matching edge from an entry node in union
        // walk from that edge either smashing or copying (pushing new nodes on the stack as you go)
        // if no matching edge, create a new matching edge from an entry in union,
        // and copy subtree of that edge from other to union.
        // working target (union) node id, source (other) node id
        let mut stack: Vec<(_, _)> = Default::default();
        for other_id in &other.entry {
            if let Some(edges) = other.edges_from(*other_id) {
                for (other_edge_target, other_edge) in edges {
                    let other_edge_kind = other.edge(other_edge);
                    // is there a matching edge in union?
                    let mut found = false;
                    for union_id in &union.entry {
                        let matching = union
                            .edge_by_kind(*union_id, &other_edge_kind.criteria)
                            .pop();
                        match matching {
                            Some((union_edge_target, _matching_edge)) => {
                                found = true;
                                // push initial nodes on the stack to smash the targets together
                                stack.push((union_edge_target, other_edge_target));
                                break;
                            }
                            None => (),
                        }
                    }
                    if !found {
                        // copy subtree to an arbitrary union entry node
                        // need to create an edge of the appropriate type from entry to a new
                        // copy target node in union, where then the subtree can be copied?
                        let entry = union.entry.iter().next().unwrap();
                        let new_node = union.add_node(other.node(*other_edge_target).clone());
                        let _edge = union.add_edge(
                            NfaEdge {
                                criteria: other_edge_kind.criteria,
                            },
                            *entry,
                            new_node,
                        );
                        union.copy_subtree(&new_node, other, other_edge_target);
                    }
                }
            }
        }
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
            let other_edges = other.edges_from(*other_id);
            if (other_edges == None && other_edges == None)
                || (other_edges.is_some()
                    && other_edges.is_some()
                    && other_edges.as_ref().unwrap().is_empty()
                    && other_edges.as_ref().unwrap().is_empty())
            {
                continue;
            }

            if union_edges == None || union_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&union_id, other, other_id);
                continue;
            }

            for (self_target_node_id, self_edge_id) in self.edges_from(*union_id).unwrap() {
                let self_edge = self.edge(self_edge_id);

                for (other_target_node_id, other_edge_id) in other.edges_from(*other_id).unwrap() {
                    let other_edge = other.edge(other_edge_id);
                    if self_edge.criteria == other_edge.criteria {
                    } else {
                    }

                    // let next_working_node_id = union.branch(
                    //     &union_id,
                    //     kind.clone(),
                    //     new_node.clone(),
                    // );
                    // debug_assert!(working_union_node_id != next_working_node_id, "working_union_node_id == next_working_node_id");

                    // // if one side is dropped, the other side just copies in from there
                    // // either create a branch and recur to construct the union from that point
                    // // or visit a next node along a branch and recur to union with that node
                    // match (left_node_id, right_node_id) {
                    //     (None, None) => unreachable!(),
                    //     (None, Some(right_node_id)) => {
                    //         // the right hand side is other
                    //         union.copy_subtree(&next_working_node_id, other, right_node_id)
                    //     }
                    //     (Some(left_node_id), None) => {
                    //         // the left hand side is self
                    //         union.copy_subtree(&next_working_node_id, self, left_node_id)
                    //     }
                    //     (Some(left_node_id), Some(right_node_id)) => {
                    //         stack.push((left_node_id, right_node_id, next_working_node_id))
                    //     }
                    // }
                }
            }
        }
        println!("I done union stuff");
        union
    }

    // #[tracing::instrument(skip_all)]
    // fn branch(&mut self, working_node_id: &NfaIndex, kind: E, new_node: NfaNode<M>) -> NfaIndex {
    //     // rationalization: there should only be 1 branch of a given kind from a given node
    //     //
    //     // rationalize the potential branches against each other
    //     // rationalize the branches against the actual branches from self and other
    //     // rationalize the branches against the actual branches present in union already

    //     // assume only one - this is an Nfa.
    //     // Could do it to all of them to be even more Nfa-y but this is simpler.
    //     match self.edge_by_kind(*working_node_id, &kind).pop() {
    //         Some((target_node_id, _edge_id)) => {
    //             // smash the new node with the existing node
    //             self.node_mut(target_node_id).sum_mut(&new_node);
    //             target_node_id
    //         }
    //         None => {
    //             // new node
    //             // caller must pass in node of correct chirality, usually this will
    //             // be from NfaNode.sum()
    //             let new_node = self.add_node(new_node);
    //             let _edge = self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node);
    //             new_node
    //         }
    //     }
    // }

    // // there is no convergence yet but this is convergence-safe
    // #[tracing::instrument(skip_all)]
    // pub(crate) fn copy_subtree(
    //     &mut self,
    //     copy_target_node: &NfaIndex,
    //     source: &Self,
    //     source_node: &NfaIndex,
    // ) {
    //     // Step 1 may be needed later for various weird edge cases,
    //     // probably need to make a change elsewhere to have it here
    //     // 1. merge the node states into target
    //     // self.node_mut(*copy_target_node)
    //     //     .sum_mut(source.node(*source_node_id));
    //     // 2. for each edge from source,
    //     let edges: Vec<(_, _)> = source.edges_from(*source_node).unwrap_or(&vec![]).to_vec();
    //     for (source_edge_endpoint, edge) in edges {
    //         // - create a new copy-target edge-target node matching the target of the source edge
    //         let new_edge_endpoint = self.add_node(source.node(source_edge_endpoint).clone());
    //         // - get the weight and create a matching edge from target connecting to the new edge target node
    //         let _matching_edge = self.add_edge(
    //             source.edge(&edge).clone(),
    //             *copy_target_node,
    //             new_edge_endpoint,
    //         );
    //         // 3. recur on the new copy-target edge-target node copying from the
    //         //    copy-source edge-target node
    //         self.copy_subtree(&new_edge_endpoint, source, &source_edge_endpoint);
    //     }
}
