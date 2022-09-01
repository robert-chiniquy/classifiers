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

        println!("union!");
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

                    if found {
                        continue;
                    }
                    println!(
                        "found a matchign entry, doing a copy subtree for {}",
                        other_edge_kind.criteria.clone()
                    );
                    let source = *union.entry.iter().next().unwrap();
                    // copy subtree to an arbitrary union entry node
                    // need to create an edge of the appropriate type from entry to a new
                    // copy target node in union, where then the subtree can be copied?
                    let new_node = union.add_node(other.node(*other_edge_target).clone());
                    let _edge = union.safe_add_edge(
                        NfaEdge {
                            criteria: other_edge_kind.criteria.clone(),
                        },
                        source.clone(),
                        new_node,
                    );
                    union.copy_subtree(&new_node, other, other_edge_target);
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
                println!("adding in body subtree!: {:?}", &other_edges.unwrap());
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
                    println!("mutating existing node!: {:?}", &other_edges.unwrap());
                    union.node_mut(union_id).sum_mut(other.node(*other_id));
                    stack.push((union_edge_target, other_edge_target));
                    continue;
                }

                let new_node = union.add_node(other.node(*other_edge_target).clone());
                union.safe_add_edge(
                    NfaEdge {
                        criteria: other_edge_kind.criteria.clone(),
                    },
                    *other_id,
                    new_node,
                );
                union.branch(
                    other_id,
                    other_edge_kind.criteria.clone(),
                    other.node(*other_edge_target).clone(),
                );
                union.copy_subtree(&new_node, other, other_edge_target);
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
    pub fn safe_add_edge(&mut self, edge: NfaEdge<E>, source: NodeId, target: NodeId) -> EdgeId {
        self.branch(&source, edge.criteria.clone(), self.node(target).clone());
        self.add_edge(edge, source, target)
    }

    pub fn branch(
        &mut self,
        working_node_id: &NodeId,
        kind: E,
        new_node: NfaNode<M>,
    ) -> Vec<NodeId> {
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
                let _is_intersection = p.len() == 3;
                for branch in &p {
                    // branch.kind;
                    // println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (equality branch??) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                    match (&branch.left, &branch.right) {
                        // could be a Vs a OR !a VS !a OR * VS a OR !a vs !b  -> !a!b
                        (Advance, Advance) => {
                            // we need to merge both branches and change the kind of e.criteria to this...
                            // do we make a new branch with merged states?
                            if _is_intersection {
                                println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (add edge, copy subtree, merge our state) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                            } else {
                                println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (change their e, merge our state??) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                            }
                        }
                        (Advance, Stop) => {
                            // a,b,c VS !a,!b
                            //  we change kind to branch.kind (will often be the same thing)
                            //  we attempt to add the edge and node
                            if &kind != &branch.kind {
                                println!(
                                    "\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (change our kind) \n",
                                    &branch.left, &branch.right, &kind, e.criteria, branch.kind
                                );
                            } else {
                                println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (no change our kind) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                            }
                        }
                        (Stop, Advance) => {
                            //  we change e.kind to branch.kind (will often be the same thing)
                            //  we do nothing else
                            if &e.criteria != &branch.kind {
                                println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (change their kind) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                            } else {
                                println!("\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (no change their kind) \n", &branch.left, &branch.right, &kind, e.criteria, branch.kind);
                            }
                        }

                        (Advance, Stay) => {
                            // in this case, we need to merge both branches and change the kind of e.criteria to this...
                            println!(
                                "\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (change our kind??) \n",
                                &branch.left, &branch.right, &kind, e.criteria, branch.kind
                            );
                        }
                        (Stay, Advance) => {
                            println!(
                                "\n🌮🌮🌮🌮 ({:?}, {:?}) : {} VS {} -> {} (change their kind??) \n",
                                &branch.left, &branch.right, &kind, e.criteria, branch.kind
                            );
                        }
                        (Stay, Stay) | (Stay, Stop) | (Stop, Stay) | (Stop, Stop) => unreachable!(),
                    }
                }
                return vec![];
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
                if e.criteria.accepts(&kind) {
                    // superset case
                    superset_edge.push(*t);
                    println!(
                        "🍩🍩🍩🍩 found accepting path {:?} > {:?}",
                        e.criteria, kind
                    );
                }
            }
        }
        if !superset_edge.is_empty() {
            superset_edge
                .iter()
                .for_each(|t| self.node_mut(*t).sum_mut(&new_node));
            return superset_edge;
        }

        let mut subset = vec![];
        if let Some(edges) = self.edges_from(*working_node_id) {
            for (t, edge_id) in edges {
                if kind.clone().accepts(&self.edge(edge_id).criteria) {
                    // superset case
                    subset.push((*t, *edge_id));
                    println!(
                        "🌮🌮🌮🌮 we are accepting path {:?} > {:?}",
                        kind,
                        self.edge(edge_id).criteria.clone()
                    );
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
            let mut r = subset
                .into_iter()
                .map(|(t, _)| t.clone())
                .collect::<Vec<_>>();
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
            let _matching_edge = self.safe_add_edge(
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

#[test]
fn test_union() {
    let a = Nfa::from_symbols(&[Element::not_tokens(&['a'])], ());
    let b = Nfa::from_symbols(&[Element::not_tokens(&['b'])], ());
    let n1 = a.union(&b);
    n1.graphviz_file("unioned1.dot", "!a union !b");
    let n2 = n1.union(&Nfa::from_symbols(&[Element::not_tokens(&['c'])], ()));
    n2.graphviz_file("unioned2.dot", "!a U !b U !c");
}
