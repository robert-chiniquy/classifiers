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
        let mut product: Nfa<NfaNode<M>, NfaEdge<E>> = Default::default();
        product.entry = product.add_node(Default::default());
        // first, construct the product nodes
        // there is a traversal ordering of input pairs established by edge relations
        // every pair of nodes in the traversal ordering must have a corresponding product node computed
        let mut stack: Vec<(Option<NodeId>, Option<NodeId>)> = vec![(Some(a.entry), Some(b.entry))];
        while let Some((a_id, b_id)) = stack.pop() {
            if self.product_nodes.contains_key(&(a_id, b_id)) {
                continue;
            }
            // FIXME: set product_node state correctly here?
            let product_node = product.add_node(Default::default());
            self.product_nodes.insert((a_id, b_id), product_node);
            // for every edge pair in (a_id, b_id), compute the edge product,
            // and push the appropriate values for the target nodes (a, b) onto the stack
            // for any solo nodes, just continue to traverse solo (the solo node occurs in the product)
            // push all edge targets onto the stack in the correct location along with None for the other
            match (&a_id, &b_id) {
                (None, None) => unreachable!(),
                (None, Some(b_id)) => {
                    for (b_target_node_id, _b_edge_id) in b.edges_from(*b_id) {
                        stack.push((None, Some(*b_target_node_id)));
                    }
                }
                (Some(a_id), None) => {
                    for (a_target_node_id, _a_edge_id) in a.edges_from(*a_id) {
                        stack.push((Some(*a_target_node_id), None));
                    }
                }
                (Some(a_id), Some(b_id)) => {
                    for (a_target_node_id, a_edge_id) in a.edges_from(*a_id) {
                        let a_edge = a.edge(a_edge_id);
                        for (b_target_node_id, b_edge_id) in b.edges_from(*b_id) {
                            let b_edge = b.edge(b_edge_id);
                            let edge_product =
                                E::product(&a_edge.unwrap().criteria, &b_edge.unwrap().criteria);
                            // FIXME: use kind here?
                            for NfaBranch {
                                kind: _kind,
                                left,
                                right,
                            } in edge_product
                            {
                                let a_next_node_id = match left {
                                    EdgeTransition::Advance => Some(a_target_node_id),
                                    EdgeTransition::Stay => Some(a_id),
                                    EdgeTransition::Stop => None,
                                };
                                let b_next_node_id = match right {
                                    EdgeTransition::Advance => Some(b_target_node_id),
                                    EdgeTransition::Stay => Some(b_id),
                                    EdgeTransition::Stop => None,
                                };
                                stack.push((a_next_node_id.cloned(), b_next_node_id.cloned()));
                            }
                        }
                    }
                }
            }
        }
        // next, construct the product edges
        // for every product node, add or match an edge from it for every edge from the (a, b) nodes
        // for single paths, no matching or branch reconciliation is needed
        for ((a_id, b_id), p_id) in &self.product_nodes {
            match (a_id, b_id) {
                (None, None) => unreachable!(),
                (None, Some(b_id)) => {
                    for (b_target_node_id, b_edge_id) in b.edges_from(*b_id) {
                        // add an edge
                        // - from p_id
                        // - of the kind of b_edge_id
                        // - to the product node for b_target_node_id
                        let target_product_node = self
                            .product_nodes
                            .get(&(None, Some(*b_target_node_id)))
                            .unwrap();
                        product.add_edge(
                            b.edge(b_edge_id).unwrap().clone(),
                            *p_id,
                            *target_product_node,
                        );
                    }
                }
                (Some(a_id), None) => {
                    for (a_target_node_id, a_edge_id) in b.edges_from(*a_id) {
                        // add an edge
                        // - from p_id
                        // - of the kind of a_edge_id
                        // - to the product node for a_target_node_id
                        let target_product_node = self
                            .product_nodes
                            .get(&(Some(*a_target_node_id), None))
                            .unwrap();
                        product.add_edge(
                            a.edge(a_edge_id).unwrap().clone(),
                            *p_id,
                            *target_product_node,
                        );
                    }
                }
                (Some(a_id), Some(b_id)) => {
                    // this must do the branch reconciliation logic,
                    // add or modify (downscope) existing edges in product
                    // but not add nodes as the existing branch method does
                }
            }
        }
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

        product.entry = product.add_node(Default::default());
        // for every edge on every node in self.enter,
        // for every edge on every node in other.enter,
        // now a 3-tuple, (self node id, other node id, union node id)
        let mut stack: Vec<(&NodeId, &NodeId, NodeId)> = Default::default();
        stack.push((&self.entry, &other.entry, product.entry));

        // start
        // stack: first left, first right, target, ignore target
        // next ... for each combo of edges, you get a vector
        // for each branch in the vector, push onto the stack again
        let mut visited: HashSet<_> = Default::default();
        while let Some((self_id, other_id, working_union_node_id)) = stack.pop() {
            if visited.contains(&(self_id, other_id)) {
                continue;
            }
            visited.insert((self_id, other_id));
            let self_edges = self.edges_from(*self_id);
            let other_edges = other.edges_from(*other_id);
            if self_edges.is_empty() && other_edges.is_empty() {
                continue;
            }

            if self_edges.is_empty() {
                product.copy_subtree(other, other_id, &working_union_node_id);
                continue;
            }

            if other_edges.is_empty() {
                product.copy_subtree(self, self_id, &working_union_node_id);
                continue;
            }

            for (self_target_node_id, self_edge_id) in self.edges_from(*self_id) {
                let self_edge = self.edge(self_edge_id);
                for (other_target_node_id, other_edge_id) in other.edges_from(*other_id) {
                    // println!("{} {} {other_id}",self_edge_id, other_edge_id);
                    let other_edge = other.edge(other_edge_id);
                    let edge_product =
                        E::product(&self_edge.unwrap().criteria, &other_edge.unwrap().criteria);

                    for NfaBranch { kind, left, right } in edge_product {
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
                            product.branch(&working_union_node_id, kind.clone(), new_node.clone());

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
                                    .for_each(|id| product.copy_subtree(other, right_node_id, id))
                            }
                            (Some(left_node_id), None) => {
                                // the left hand side is self
                                next_working_ids
                                    .iter()
                                    .for_each(|id| product.copy_subtree(self, left_node_id, id))
                            }
                            (Some(left_node_id), Some(right_node_id)) => next_working_ids
                                .iter()
                                .for_each(|id| stack.push((left_node_id, right_node_id, *id))),
                        }
                    }
                }
            }
        }
        product
    }
}
