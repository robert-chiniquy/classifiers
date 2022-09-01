use super::*;

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    #[tracing::instrument(skip(self, other))]
    pub fn product(&self, other: &Self) -> Self {
        if self.entry.is_empty() {
            return other.clone();
        } else if other.entry.is_empty() {
            return self.clone();
        }

        // TODO: handle unrolling here?
        // make a copy of self to unroll and use in the remainder of product
        // alternatives
        // - for each edge in self, call unroll_edge, maybe it handles that edge kind, maybe no-op
        // - visit each edge pair which would occur in product and only unroll the needed?
        let mut union: Self = Default::default();

        // println!(
        //     "I union stuff: {} {} {}",
        //     self.nodes.len(),
        //     other.nodes.len(),
        //     union.nodes.len()
        // );
        let _entry = union.add_node(Default::default());
        union.entry.insert(_entry);
        // for every edge on every node in self.enter,
        // for every edge on every node in other.enter,
        // now a 3-tuple, (self node id, other node id, union node id)
        let mut stack: Vec<(&NodeId, &NodeId, NodeId)> = Default::default();
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
        let mut visited: HashSet<_> = Default::default();
        while let Some((self_id, other_id, working_union_node_id)) = stack.pop() {
            if visited.contains(&(self_id, other_id)) {
                // should this also validate working node id?
                continue;
            }
            visited.insert((self_id, other_id));
            let self_edges = self.edges_from(*self_id);
            let other_edges = other.edges_from(*other_id);
            if (self_edges == None && other_edges == None)
                || (self_edges.is_some()
                    && other_edges.is_some()
                    && self_edges.as_ref().unwrap().is_empty()
                    && other_edges.as_ref().unwrap().is_empty())
            {
                continue;
            }

            if self_edges == None || self_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&working_union_node_id, other, other_id);
                continue;
            }

            if other_edges == None || other_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&working_union_node_id, self, self_id);
                continue;
            }

            for (self_target_node_id, self_edge_id) in self.edges_from(*self_id).unwrap() {
                let self_edge = self.edge(self_edge_id);
                for (other_target_node_id, other_edge_id) in other.edges_from(*other_id).unwrap() {
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
                        let  next_working_ids =
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
                                next_working_ids.iter().for_each(|id| union.copy_subtree(&id, other, right_node_id))
                                
                            }
                            (Some(left_node_id), None) => {
                                // the left hand side is self
                                next_working_ids.iter().for_each(|id| union.copy_subtree(&id, self, left_node_id))
                            }
                            (Some(left_node_id), Some(right_node_id)) => {
                                next_working_ids.iter().for_each(|id| stack.push((left_node_id, right_node_id, *id)))   
                            }
                        }
                    }
                }
            }
        }
        // println!("I done union stuff");
        union
    }

    /// - working_node_id: union node edges are being compared from
    /// - new_node: the node to be either put at the end of a new edge, or
    ///   combined with the target node of an existing edge from the working_node_id node
    ///
    /// returns the index of the new node if an edge is created, or the pre-existing node
    /// which was rationalized to if not.
    #[tracing::instrument(skip_all)]
    fn branch(&mut self, working_node_id: &NodeId, kind: E, new_node: NfaNode<M>) -> Vec<NodeId> {
        // rationalization: there should only be 1 branch of a given kind from a given node
        //
        // rationalize the potential branches against each other
        // rationalize the branches against the actual branches from self and other
        // rationalize the branches against the actual branches present in union already

        // assume only one - this is an Nfa.
        // Could do it to all of them to be even more Nfa-y but this is simpler.
        let existing_edge = self.edge_by_kind(*working_node_id, &kind).pop();
        if existing_edge.is_some() {
            let (t, _) = existing_edge.unwrap();
            self.node_mut(t).sum_mut(&new_node);
            return vec![t];
        }
        use EdgeTransition::*;
        if let Some(edges) = self.edges_from(*working_node_id) {
            for (_t, e) in edges {
                let e = self.edge(e);
                let p = E::product(&kind, &e.criteria);

                /*
                equality/superset:
                    !b vs a
                    take the edge
                    combine node states
                    return !b -> target
                
                subset:
                    a vs !b 
                    a & !a!b... 
                    a stays the same, 
                    combine node states at a-> target with our node, 
                    make new edge and add new node, 
                    return both node_ids

                intersection:
                    !a vs !b
                    b !a!b a
                    copy subtree from !a to !a!b
                    change edge !a -> b
                    copy node state into !a!b -> target
                    add our node
                    add edge to our node
                    return nodeids for targets for [!a!b, a]
                */
                let _is_intersection =  p.len() == 3;
                for branch in &p {
                    // branch.kind;
                    match (&branch.left, &branch.right) {
                        // could be a Vs a OR !a VS !a OR * VS a OR !a vs !b  -> !a!b
                        (Advance, Advance) => {
                            // we need to merge both branches and change the kind of e.criteria to this...
                            // do we make a new branch with merged states?
                            todo!()
                        },
                        (Advance, Stop) => {
                            // a,b,c VS !a,!b
                            //  we change kind to branch.kind (will often be the same thing)
                            //  we attempt to add the edge and node
                            todo!()
                        },
                        (Stop, Advance) => {
                            //  we change e.kind to branch.kind (will often be the same thing)
                            //  we do nothing else
                            todo!()
                        },

                        (Advance, Stay) => {
                            // in this case, we need to merge both branches and change the kind of e.criteria to this...
                            todo!()
                        },
                        (Stay, Advance) => todo!(),
                        (Stay, Stay) => unreachable!(), // stars only
                        (Stay, Stop) => unreachable!(),
                        (Stop, Stay) => unreachable!(),
                        (Stop, Stop) => unreachable!(),
                   }
                }

                // if let Ok(true) = e.criteria.accepts(kind.clone()) {
                //     // superset case
                //     superset_edge.push(*t);
                //     println!("🍩🍩🍩🍩 found accepting path {:?} > {:?}", e.criteria, kind);
                // }
            }
        }


        /*
    
        !a 
        !b

        *a
        a*
        
        aa
        ??*

        */

        // the superset edge case
        // When adding a new edge to a node, we need to ensure not only that there are no extant identical edges,
        // but that no edge is a superset (accepting of the new edge kind). If an edge is a superset, we
        // we need to visit it as if it were an identical edge type
        let mut superset_edge = vec![];
        if let Some(edges) = self.edges_from(*working_node_id) {
            for (t, e) in edges {
                let e = self.edge(e);
                if let Ok(true) = e.criteria.accepts(kind.clone()) {
                    // superset case
                    superset_edge.push(*t);
                    println!("🍩🍩🍩🍩 found accepting path {:?} > {:?}", e.criteria, kind);
                }
            }
        }
        if !superset_edge.is_empty() {
            superset_edge.iter().for_each(|t| self.node_mut(*t).sum_mut(&new_node));
            return superset_edge;
        }

        let mut subset = vec![];
        if let Some(edges) = self.edges_from(*working_node_id) {
            for (t, edge_id) in edges {
                if let Ok(true) = kind.clone().accepts(self.edge(edge_id).criteria.clone()) {
                    // superset case
                    subset.push((*t, *edge_id));
                    println!("🌮🌮🌮🌮 we are accepting path {:?} > {:?}", kind, self.edge(edge_id).criteria.clone());
                }
            }
        }
        if !&subset.is_empty() {
            let mut new_node = new_node.clone();
            //  ? vs a, b...
            //  add our state to a -> target, b -> target
            //  change kind to  ? - a - b, 
            //  add our edge
            //  add our node

            for (t, _) in &subset {
                // self.remove_edge(e);
                new_node.sum_mut(&self.node(*t));
            }

            let new_node_id = self.add_node(new_node);
            let _edge = self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node_id);
            let mut r = subset.into_iter().map(|(t,_)| t.clone()).collect::<Vec<_>>();
            r.push(new_node_id);
            return r;
        }


        // TODO: the intersection case

        // new node
        // caller must pass in node of correct chirality, usually this will
        // be from NfaNode.sum()
        let new_node = self.add_node(new_node.clone());
        let _edge = self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node);
        return vec![new_node];
    }

    // there is no convergence yet but this is convergence-safe
    #[tracing::instrument(skip_all)]
    pub(crate) fn copy_subtree(
        &mut self,
        copy_target_node: &NodeId,
        source: &Self,
        source_node: &NodeId,
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
