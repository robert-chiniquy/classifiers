use super::*;

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: std::fmt::Debug + Clone + Default + NodeSum,
    E: std::fmt::Debug + Clone + BranchProduct<E> + Eq + std::hash::Hash + std::default::Default,
{
    /// A union is a non-mimimal NFA with the same resulting states for every input as
    /// either of the two input NFAs.
    // Whatever invariant is enforced here, assume that the inputs have that invariant
    // Blindly copying states here allows M to vary widely
    #[tracing::instrument(skip(self, other))]
    pub fn union(&self, other: &Self) -> Self {
        if self.entry.is_empty() {
            return other.clone();
        } else if other.entry.is_empty() {
            return self.clone();
        }
        let mut union: Nfa<N, NfaEdge<E>> = Default::default();
        let _entry = union.add_node(Default::default());
        union.entry.insert(_entry);
        // for every edge on every node in self.enter,
        // for every edge on every node in other.enter,
        // now a 3-tuple, (self node id, other node id, union node id)
        let mut stack: Vec<(&NfaIndex, &NfaIndex, NfaIndex)> = Default::default();
        for self_id in &self.entry {
            for other_id in &other.entry {
                stack.push((self_id, other_id, _entry));
            }
        }
        // start
        // stack: first left, first right, target, ignore target
        // evaluate edges, each of the two nodes has 1 edge, you see like a * A product,
        // next ... for each combo of edges, you get a vector
        // for each branch in the vector, you should push onto the stack again,
        // This currently assumes an acyclic graph
        // Cycle detection can occur by tracking visited combinations on the stack
        while let Some((self_id, other_id, working_union_node_id)) = stack.pop() {
            // if *self_id == 1_usize && *other_id == 2_usize && working_union_node_id == 6_usize {
            // println!("{stack:?}");
            //     ()
            // }
            let self_edges = self.edges_from(*self_id);
            let other_edges = other.edges_from(*other_id);
            if (self_edges == None && other_edges == None)
                || (self_edges.is_some()
                    && other_edges.is_some()
                    && self_edges.as_ref().unwrap().is_empty()
                    && other_edges.as_ref().unwrap().is_empty())
            {
                continue;
            } else if self_edges == None || self_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&working_union_node_id, other, other_id);
            } else if other_edges == None || other_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&working_union_node_id, self, self_id);
            } else {
                for (self_target_node_id, self_edge_id) in self.edges_from(*self_id).unwrap() {
                    let self_edge = self.edge(self_edge_id);
                    for (other_target_node_id, other_edge_id) in
                        other.edges_from(*other_id).unwrap()
                    {
                        let other_edge = other.edge(other_edge_id);
                        let product = E::product(&self_edge.criteria, &other_edge.criteria);
                        for NfaBranch { kind, left, right } in product {
                            let left_node_id = match left {
                                EdgeTransition::Advance => Some(self_target_node_id),
                                EdgeTransition::Stay => Some(self_id),
                                EdgeTransition::Stop => None,
                            };
                            let right_node_id = match right {
                                EdgeTransition::Advance => Some(other_target_node_id),
                                EdgeTransition::Stay => Some(other_id),
                                EdgeTransition::Stop => None,
                            };
                            let new_node = match (left_node_id, right_node_id) {
                                (None, None) => unreachable!(),
                                (None, Some(right_node_id)) => {
                                    // println!("🎧🎧🎧🎧{:?}", other.node(*right_node_id));
                                    other.node(*right_node_id).clone()
                                }
                                (Some(left_node_id), None) => {
                                    // println!("✈️✈️✈️✈️✈️✈️✈️✈️✈️{:?}", self.node(*left_node_id));
                                    self.node(*left_node_id).clone()
                                }
                                (Some(left_node_id), Some(right_node_id)) => {
                                    self.node(*left_node_id).sum(other.node(*right_node_id))
                                }
                            };
                            // println!("🐥🐥🐥🐥🐥{new_node:?}");
                            let next_working_node_id = union.branch(
                                &working_union_node_id,
                                kind.clone(),
                                new_node.clone(),
                            );

                            // if left == EdgeTransition::Stay && right == EdgeTransition::Advance {
                            //     println!("🚗🚗🚗🚗 {kind:?} {left_node_id:?} {right_node_id:?} {new_node:?} {next_working_node_id}");
                            // }

                            // if one side is dropped, the other side just copies in from there
                            // either create a branch and recur to construct the union from that point
                            // or visit a next node along a branch and recur to union with that node
                            match (left_node_id, right_node_id) {
                                (None, None) => unreachable!(),
                                (None, Some(right_node_id)) => {
                                    // the right hand side is other
                                    union.copy_subtree(&next_working_node_id, other, right_node_id)
                                }
                                (Some(left_node_id), None) => {
                                    // the left hand side is self
                                    union.copy_subtree(&next_working_node_id, self, left_node_id)
                                }
                                (Some(left_node_id), Some(right_node_id)) => {
                                    // println!(
                                    //     "XOXOXOXO {:?}",
                                    //     (left_node_id, right_node_id, next_working_node_id)
                                    // );
                                    stack.push((left_node_id, right_node_id, next_working_node_id))
                                }
                            }
                        }
                    }
                }
            }
        }
        union
    }

    /// - working_node_id: union node edges are being compared from
    /// - new_node: the node to be either put at the end of a new edge, or
    ///   combined with the target node of an existing edge from the working_node_id node
    ///
    /// returns the index of the new node if an edge is created, or the pre-existing node
    /// which was rationalized to if not.
    #[tracing::instrument]
    fn branch(&mut self, working_node_id: &NfaIndex, kind: E, new_node: N) -> NfaIndex {
        // rationalization: there should only be 1 branch of a given kind from a given node
        //
        // rationalize the potential branches against each other
        // rationalize the branches against the actual branches from self and other
        // rationalize the branches against the actual branches present in union already

        // assume only one - this is an Nfa.
        // Could do it to all of them to be even more Nfa-y but this is simpler.
        match self.edge_by_kind(*working_node_id, &kind).pop() {
            Some((target_node_id, _edge_id)) => {
                // smash the new node with the existing node
                self.node_mut(target_node_id).sum_mut(&new_node);
                target_node_id
            }
            None => {
                // new node
                // caller must pass in node of correct chirality, usually this will
                // be from NfaNode.sum()
                let new_node = self.add_node(new_node);
                let _edge = self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node);
                new_node
            }
        }
    }

    // there is no convergence yet but this is convergence-safe
    #[tracing::instrument]
    pub(crate) fn copy_subtree(
        &mut self,
        copy_target_node: &NfaIndex,
        source: &Self,
        source_node: &NfaIndex,
    ) {
        // Step 1 may be needed later for various weird edge cases,
        // probably need to make a change elsewhere to have it here
        // 1. merge the node states into target
        // self.node_mut(*copy_target_node)
        //     .sum_mut(source.node(*source_node_id));
        // 2. for each edge from source,
        let edges: Vec<(_, _)> = source.edges_from(*source_node).unwrap_or(&vec![]).to_vec();
        for (source_edge_endpoint, edge) in edges {
            // - create a new copy-target edge-target node matching the target of the source edge
            let new_edge_endpoint = self.add_node(source.node(source_edge_endpoint).clone());
            // - get the weight and create a matching edge from target connecting to the new edge target node
            let _matching_edge = self.add_edge(
                source.edge(&edge).clone(),
                *copy_target_node,
                new_edge_endpoint,
            );
            // 3. recur on the new copy-target edge-target node copying from the
            //    copy-source edge-target node
            self.copy_subtree(&new_edge_endpoint, source, &source_edge_endpoint);
        }
    }
}
