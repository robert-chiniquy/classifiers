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
                    other.edge(other_edge_id).criteria.clone(),
                    other.node(*other_edge_target).clone(),
                );

                for id in next_ids {
                    stack.push((id, other_edge_target.clone()));
                }
            }
        }
        union
    }

    pub fn safe_add_edge(
        &mut self,
        edge: NfaEdge<E>,
        source: NodeId,
        target: NodeId,
    ) -> Vec<NodeId> {
        self.branch(&source, edge.criteria.clone(), self.node(target).clone())
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
        self.node_mut(existing_node).sum_mut(&target_node);
        return vec![existing_node];
    }

    fn clean_edges(&mut self, source: NodeId, edges: Vec<EdgeId>) -> Vec<NodeId> {
        use EdgeTransition::*;

        let mut should_restart = false;
        let mut _new_nodes_or_something = vec![];
        for (i, e1) in edges.clone().into_iter().enumerate() {
            let c1 = self.edge(&e1).criteria.clone();
            for e2 in edges[i + 1..].iter() {
                debug_assert!(e1 != *e2, "same edge");
                let c2 = self.edge(&e2).criteria.clone();
                if E::are_disjoint(vec![c1.clone(), c2.clone()]) {
                    continue;
                }
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
                            _new_nodes_or_something.push(center_node);
                            should_restart = true;
                        }
                        (Advance, Advance) => {
                            println!(
                                "\n({:?}, {:?}) : {} VS {} -> {} (🌮🌮🌮🌮🌮 copied left into right..., deleted left) \n",
                                &branch.left, &branch.right, &c1, c2, branch.kind
                            );

                            if c1 == c2 {
                                let left = self.destination(&e1).unwrap();
                                let right = self.destination(&e2).unwrap();

                                self.self_copy_subtree(&left, &right);
                                println!("deleting right: {right}");
                                self.delete_subtree(&left);
                                _new_nodes_or_something.push(right);
                                should_restart = true;
                            }
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
                                _new_nodes_or_something.push(self.destination(&e1).unwrap());
                                should_restart = true;
                            } else {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {}, ({}) (no change our kind) \n",
                                    &branch.left, &branch.right, c1, c2, branch.kind
                                );
                            }
                        }
                        (Stop, Advance) => {
                            //  we change e.kind to branch.kind (will often be the same thing)
                            //  we do nothing else
                            if c2 != branch.kind {
                                println!(
                                    "\n({:?}, {:?}) : {} VS {} -> {} (change their kind) \n",
                                    &branch.left, &branch.right, &c1, c2, branch.kind
                                );
                                self.edge_mut(*e2).unwrap().criteria = branch.kind.clone();
                                should_restart = true;
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
        return _new_nodes_or_something;
    }

    pub fn branch(
        &mut self,
        working_node_id: &NodeId,
        kind: E,
        // this node is instantiated, but not in the graph
        new_node: NfaNode<M>,
    ) -> Vec<NodeId> {
        println!("branching: {}", kind.clone());

        let edges = self.edge_by_kind(*working_node_id, &kind);
        if !edges.is_empty() {
            let mut nodes = vec![];
            println!("found matching edge for {}", kind);
            for (n, _) in edges.clone() {
                nodes.push(n);
                self.node_mut(n).sum_mut(&new_node);
            }
            return nodes;
        }

        println!("adding new edge: {}", kind.clone());
        let new_node_id = self.add_node(new_node);
        self.add_edge(
            NfaEdge {
                criteria: kind.clone(),
            },
            *working_node_id,
            new_node_id,
        );

        // let mut new_nodes_or_something = vec![new_node_id];
        let mut stuff = vec![*working_node_id];
        loop {
            let edges = self.edges_from(*working_node_id);
            let vedges: Vec<_> = edges
                .iter()
                .map(|(_, e)| self.edge(e).criteria.clone())
                .collect();

            if E::are_disjoint(vedges.clone()) {
                println!("stuff is disjoint!!: {:?}", vedges);
                return stuff;
            } else {
                println!("stuff isn't disjoint!?: {:?}", vedges);
            }
            stuff
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
                source.edge(&edge).clone(),
                *copy_target_node,
                new_edge_endpoint,
            );
            self.copy_subtree(source, &source_edge_endpoint, &new_edge_endpoint);
        }
    }

    #[allow(dead_code)]
    pub(crate) fn self_copy_subtree(&mut self, source_node: &NodeId, copy_target_node: &NodeId) {
        for (target, edge) in self.edges_from(*source_node).clone() {
            println!("self_copy_subtree: {target} {edge}");

            let c = self.edge(&edge).criteria.clone();  

            for id in self.branch(copy_target_node, c, self.node(target).clone()) {
                self.self_copy_subtree(&target, &id);
            }
        }
    }
}

#[test]
fn test_union() {
    let nt = Element::not_tokens;
    let a = Nfa::from_symbols(&[nt(&['a']), nt(&['b'])], ());
    let b = Nfa::from_symbols(&[nt(&['c'])], ());
    println!("from symbols {a:?}\n{b:?}");
    let n1 = a.union(&b);
    n1.graphviz_file("unioned1.dot", "!a!b U !c");
    // let n2 = n1.union(&Nfa::from_symbols(&[Element::not_tokens(&['c'])], ()));
    // n2.graphviz_file("unioned2.dot", "!a U !b U !c");
}
