use super::*;

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    pub fn product(&self, other: &Self) -> Self {
        // TODO: handle unrolling here?
        // make a copy of self to unroll and use in the remainder of product
        // alternatives
        // - for each edge in self, call unroll_edge, maybe it handles that edge kind, maybe no-op
        // - visit each edge pair which would occur in product and only unroll the needed?
        let mut union: Self = Default::default();

        union.entry = union.add_node(Default::default());
        // for every edge on every node in self.enter,
        // for every edge on every node in other.enter,
        // now a 3-tuple, (self node id, other node id, union node id)
        let mut stack: Vec<(&NodeId, &NodeId, NodeId)> = Default::default();
        stack.push((&self.entry, &other.entry, union.entry));

        // start
        // stack: first left, first right, target, ignore target
        // evaluate edges, each of the two nodes has 1 edge, you see like a * A product,
        // next ... for each combo of edges, you get a vector
        // for each branch in the vector, you should push onto the stack again,
        // This currently assumes an acyclic graph
        // Cycle detection can occur by tracking visited combinations on the stack
        let mut visited: HashSet<_> = Default::default();
        while let Some((self_id, other_id, working_union_node_id)) = stack.pop() {
            if visited.contains(&(self_id, other_id)) {
                // should this also validate working node id?
                continue;
            }
            visited.insert((self_id, other_id));
            let self_edges = self.edges_from(*self_id);
            let other_edges = other.edges_from(*other_id);
            if self_edges.is_empty() && other_edges.is_empty() {
                continue;
            }

            if self_edges.is_empty() {
                union.copy_subtree(other, other_id, &working_union_node_id);
                continue;
            }

            if other_edges.is_empty() {
                union.copy_subtree(self, self_id, &working_union_node_id);
                continue;
            }

            for (self_target_node_id, self_edge_id) in self.edges_from(*self_id) {
                let self_edge = self.edge(self_edge_id);
                for (other_target_node_id, other_edge_id) in other.edges_from(*other_id) {
                    // println!("{} {} {other_id}",self_edge_id, other_edge_id);
                    let other_edge = other.edge(other_edge_id);
                    // Robert suggests: Do not unroll here.
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
                            (None, Some(right_node_id)) => other.node(*right_node_id).clone(),
                            (Some(left_node_id), None) => self.node(*left_node_id).clone(),
                            (Some(left_node_id), Some(right_node_id)) => {
                                self.node(*left_node_id).sum(other.node(*right_node_id))
                            }
                        };
                        let next_working_ids =
                            union.branch(&working_union_node_id, kind.clone(), new_node.clone());

                        // debug_assert!(
                        //     working_union_node_id != next_working_ids,
                        //     "working_union_node_id == next_working_ids"
                        // );

                        // if one side is dropped, the other side just copies in from there
                        // either create a branch and recur to construct the union from that point
                        // or visit a next node along a branch and recur to union with that node
                        match (left_node_id, right_node_id) {
                            (None, None) => unreachable!(),
                            (None, Some(right_node_id)) => {
                                // the right hand side is other
                                next_working_ids
                                    .iter()
                                    .for_each(|id| union.copy_subtree(other, right_node_id, id))
                            }
                            (Some(left_node_id), None) => {
                                // the left hand side is self
                                next_working_ids
                                    .iter()
                                    .for_each(|id| union.copy_subtree(self, left_node_id, id))
                            }
                            (Some(left_node_id), Some(right_node_id)) => next_working_ids
                                .iter()
                                .for_each(|id| stack.push((left_node_id, right_node_id, *id))),
                        }
                    }
                }
            }
        }
        // println!("I done union stuff");
        union
    }
}
