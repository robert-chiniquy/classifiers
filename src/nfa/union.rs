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

                    // 1. equality ✅
                    // 2. subset ✅
                    // 3. superset ✅
                    // 4. intersection 

                    if !found {
                        // copy subtree to an arbitrary union entry node
                        // need to create an edge of the appropriate type from entry to a new
                        // copy target node in union, where then the subtree can be copied?
                        let new_node = union.add_node(other.node(*other_edge_target).clone());
                        let _edge = union.add_edge(
                            NfaEdge {
                                criteria: other_edge_kind.criteria.clone(),
                            },
                            *union.entry.iter().next().unwrap(),
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

            if other_edges == None {
                continue;
            }

            if union_edges.is_some()
                && union_edges.as_ref().unwrap().is_empty()
                && other_edges.as_ref().unwrap().is_empty()
            {
                continue;
            }

            if union_edges == None || union_edges.as_ref().unwrap().is_empty() {
                union.copy_subtree(&union_id, other, other_id);
                continue;
            }

            for (other_edge_target, other_edge) in other_edges.unwrap() {
                let other_edge_kind = other.edge(other_edge);
                // is there a matching edge in union?
                let matching = union
                    .edge_by_kind(union_id, &other_edge_kind.criteria)
                    .pop();

                if let Some((union_edge_target, _)) = matching {
                    union.node_mut(union_id).sum_mut(other.node(*other_id));
                    stack.push((union_edge_target, other_edge_target));
                    continue;
                }

                let new_node = union.add_node(other.node(*other_edge_target).clone());
                union.add_edge(
                    NfaEdge {
                        criteria: other_edge_kind.criteria.clone(),
                    },
                    *other_id,
                    new_node,
                );
                union.copy_subtree(&new_node, other, other_edge_target);
            }
        }
        union
    }
}
