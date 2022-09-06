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

    /// Returns the ids of any nodes which are the targets of modified edges
    #[tracing::instrument(skip(self))]
    fn clean_edges(&mut self, source: NodeId, edges: Vec<EdgeId>) -> Vec<NodeId> {
        use EdgeTransition::*;

        let mut should_restart = false;
        let mut any_changed_targets = vec![];
        for (i, e1) in edges.clone().into_iter().enumerate() {
            if self.edge(&e1).is_none() {
                continue;
            }
            let c1 = self.edge(&e1).unwrap().criteria.clone();
            for e2 in edges[i + 1..].iter() {
                debug_assert!(e1 != *e2, "same edge");
                let c2 = self.edge(e2).unwrap().criteria.clone();
                if E::are_disjoint(vec![c1.clone(), c2.clone()]) {
                    continue;
                }
                // If you have 2 non-disjoint edges,
                // they must be split into at most 3 edges,
                // with 1 edge for each of the 2 original which represents their
                // non-overlapping values (if any), and an edge representing the overlap
                // product() only indirectly points to this result
                let p = E::product(&c1, &c2);

                let is_intersection = p.len() == 3;

                // println!("{} products for {c1} X {c2}", p.len());
                for branch in &p {
                    // println!(
                    //     "\n({:?}, {:?}) {} \n",
                    //     &branch.left, &branch.right, branch.kind
                    // );
                    match (&branch.left, &branch.right) {
                        // could be a Vs a OR !a VS !a OR * VS a OR !a vs !b  -> !a!b
                        (Advance, Advance) if is_intersection => {
                            // make a new branch
                            // merge e1 subtree!
                            // merge e2 subtree
                            // delete e1 subtree?
                            // ~delete e2 subtree?~\
                            println!("\n({:?}, {:?}) : {} VS {}, ?? -> {} (is intersection change their e, merge our state??) \n", &branch.left, &branch.right, &c1, c2, branch.kind);

                            let left = self.destination(&e1).unwrap();
                            let right = self.destination(&e2).unwrap();

                            debug_assert!(
                                left != right,
                                "left {left} is right {right} for {e1}/{e2} on {:?}",
                                self
                            );

                            let existing_edge = self.edge_by_kind(source, &branch.kind);
                            let center_node = match existing_edge.is_empty() {
                                true => {
                                    // copy subtree doesn't do this for us atm :-/
                                    let center_node = self
                                        .add_node(self.node(left).clone().sum(self.node(right)));
                                    self.add_edge(
                                        NfaEdge {
                                            criteria: branch.kind.clone(),
                                        },
                                        source,
                                        center_node,
                                    );
                                    center_node
                                }
                                false => existing_edge[0].0,
                            };

                            println!("\n\npre intersection: {left} {right}self...{:?}\n{:?} {:?} {:?}\n edges from left: {:?}\nedges from right: {:?}", self, self.node(left), self.node(center_node), self.node(right), self.edges_from(left), self.edges_from(right));
                            self.self_copy_subtree(&left, &center_node);
                            self.self_copy_subtree(&right, &center_node);
                            println!(
                                "\n\npost intersection: {:?} {:?} {:?}",
                                self.node(left),
                                self.node(center_node),
                                self.node(right)
                            );
                            any_changed_targets.push(center_node);
                            should_restart = true;
                        }
                        (Advance, Advance) => {
                            //if c1 == c2 {
                            println!(
                                "\n({:?}, {:?}) : {} VS {} -> {} (🌮🌮🌮🌮🌮 copied left into right..., deleted left) \n",
                                    &branch.left, &branch.right, &c1, c2, branch.kind
                                );

                            let left = self.destination(&e1).unwrap();
                            let right = self.destination(e2).unwrap();

                            self.graphviz_file("pre-copy.dot", "pre-copy");
                            println!("copying left: {left} to right: {right}");
                            self.self_copy_subtree(&left, &right);
                            println!("deleting left: {left}");
                            self.graphviz_file("post-copy.dot", "post-copy");

                            self.delete_subtree(&left);
                            self.graphviz_file("post-delete.dot", "post-delete");

                            self.edge_mut(*e2).unwrap().criteria = branch.kind.clone();

                            any_changed_targets.push(right);
                            should_restart = true;
                            //}
                        }
                        (Advance, Stop) => {
                            // a,b,c VS !a,!b
                            //  we change kind to branch.kind (will often be the same thing)
                            //  we attempt to add the edge and node
                            if c1 != branch.kind {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {}, {} -> {} (change our kind) \n",
                                    &branch.left, &branch.right, c1, c2, c2, branch.kind
                                );
                                self.edge_mut(e1).unwrap().criteria = branch.kind.clone();
                                any_changed_targets.push(self.destination(&e1).unwrap());
                                should_restart = true;
                            } else {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {}, ({}) (no change our kind) \n",
                                    &branch.left, &branch.right, c1, c2, branch.kind
                                );
                            }
                        }
                        (Stop, Advance) => {
                            // we change e.kind to branch.kind (will often be the same thing)
                            // if an edge of branch.kind already exists, use that
                            // we do nothing else
                            if c2 != branch.kind {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {} -> {} (change their kind) \n",
                                    &branch.left, &branch.right, &c1, c2, branch.kind
                                );
                                let same_edge = self.edge_by_kind(source, &branch.kind);
                                if !same_edge.is_empty() {
                                    let (same_edge_target, _same_edge) = same_edge[0];
                                    self.self_copy_subtree(
                                        &self.destination(e2).unwrap(),
                                        &same_edge_target,
                                    );
                                    self.delete_subtree(&self.destination(e2).unwrap());
                                } else if let Some(edge) = self.edge_mut(*e2) {
                                    edge.criteria = branch.kind.clone();
                                    should_restart = true;
                                }
                            } else {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {} ({}) (no change their kind) \n",
                                    &branch.left, &branch.right, &c1, c2, branch.kind
                                );
                            }
                        }

                        (Advance, Stay) => {
                            // in this case, we need to merge both branches and change the kind of e to this...
                            println!(
                                "\n({:?}, {:?}) : {} VS {} -> {} (change our kind??) \n",
                                &branch.left, &branch.right, c1, c2, branch.kind
                            );
                        }
                        (Stay, Advance) => {
                            println!(
                                "\n({:?}, {:?}) : {} VS {} -> {} (change their kind??) \n",
                                &branch.left, &branch.right, c1, c2, branch.kind
                            );
                        }
                        (Stay, Stay) | (Stay, Stop) | (Stop, Stay) | (Stop, Stop) => unreachable!(),
                    }
                }
                if should_restart {
                    break;
                }
            }
            if should_restart {
                break;
            }
        }
        any_changed_targets
    }

    /// Invariant-preserving edge->node insert
    /// Ensures that the edges from working_node_id remain consistent (disjoint)
    /// Returns the ids of any added nodes
    #[tracing::instrument(skip(self))]
    pub fn branch(
        &mut self,
        working_node_id: &NodeId,
        kind: E,
        // this node is instantiated, but not in the graph
        new_node: NfaNode<M>,
    ) -> Vec<NodeId> {
        println!("branching: {}", kind);



        /* new algo:
         
         1. find appropriate edge (sort in order of):
            a. identity
            b. subset
            c. superset
            d. intersection
            e. disjoint
         2. Add new edge/modify existing target edge as appropriate for the exact relationship
            a. take edge, return target id
            b. take edge, return target id
            c. split superset into two, add modified criteria, return both edge ids
            d. do product, update middle criteria, return both edge ids
            e. just add branch
        */
        let mut best = Relation::Disjoint;
        let mut best_edge = None;
        let mut best_target_node = None;
        use Relation::*;
        for (t, e) in self.edges_from(*working_node_id) {
            let rel = E::relation(&kind, &self.edge(e).unwrap().criteria);
            match rel {
                Disjoint => {},
                Intersection => {
                    if best == Equality ||  best == Subset || best == Superset {
                        continue;
                    }
                    best = rel;
                    best_edge = Some(e);
                    best_target_node = Some(t);
                },
                Superset => {
                    if best == Equality ||  best == Subset {
                        continue;
                    }
                    best = rel;
                    best_edge = Some(e);
                    best_target_node = Some(t);
                },
                Subset => {
                    if best == Equality {
                        continue;
                    }
                    best = rel;
                    best_edge = Some(e);
                    best_target_node = Some(t);
                },
                Equality => {
                    best = Equality;
                    best_edge = Some(e);
                    best_target_node = Some(t);
                    break;
                },
            }
        }
        // first, look for a match
        let edges = self.edge_by_kind(*working_node_id, &kind);
        if !edges.is_empty() {
            let mut nodes = vec![];
            println!("found matching edge for {}", kind);
            for (n, _) in edges {
                nodes.push(n);
                self.node_mut(n).sum_mut(&new_node);
            }
            return nodes;
        }

        // if no match, try to find a good place to add
        println!("adding new edge: {}", kind);
        let new_node_id = self.add_node(new_node);
        self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node_id);

        // let mut new_nodes_or_something = vec![new_node_id];
        let mut node_ids = vec![*working_node_id];
        let mut i = 1;
    


        loop {
            let name = &format!("auto-{i}.dot");
            self.graphviz_file(name, name);
            i += 1;
            let edges = self.edges_from(*working_node_id);
            let edge_criterias: Vec<_> = edges
                .iter()
                .map(|(_, e)| self.edge(e).unwrap().criteria.clone())
                .collect();

            // ? If any 2 (or more) edges from a given node are non-disjoint,
            // do we only need to operate on the non-disjoint set
            if E::are_disjoint(edge_criterias.clone()) {
                println!("stuff is disjoint!!: {:?}", edge_criterias);
                return node_ids;
            } else {
                println!("stuff isn't disjoint!?: {:?}", edge_criterias);
            }
            node_ids
                .extend(self.clean_edges(*working_node_id, edges.iter().map(|(_, e)| *e).collect()))
            
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
                *copy_target_node,
                new_edge_endpoint,
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
    println!("from symbols {a:?}\n{b:?}");
    let n1 = a.union(&b);
    n1.graphviz_file("unioned1.dot", "!a!b U !c");
    let n2 = n1.union(&Nfa::from_symbols(&[Element::not_tokens(&['c'])], ()));
    n2.graphviz_file("unioned2.dot", "!a U !b U !c");
}
