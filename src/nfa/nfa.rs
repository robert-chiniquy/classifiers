pub type NfaIndex = usize;

use super::*;

// TODO
// - remove NfaNode, NfaEdge?
// - Not logic, inversion, compilation, intersection

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nfa<N, E>
where
    E: Eq + std::hash::Hash + Default,
{
    pub(crate) count: usize,
    // Convenience for now, later switch to only a single initial node?
    pub(crate) entry: BTreeSet<NfaIndex>,
    pub(crate) nodes: BTreeMap<NfaIndex, N>,
    pub(crate) edges: BTreeMap<NfaIndex, E>,
    /// source node index -> Vec<(target node index, edge index)>
    // TODO: just split into 2 maps
    pub(crate) transitions: BTreeMap<NfaIndex, Vec<(NfaIndex, NfaIndex)>>,
    // pub(crate) chirality: BTreeMap<NfaIndex, LRSemantics>,
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: Eq + Clone + std::hash::Hash + Default + std::fmt::Debug,
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    #[tracing::instrument(skip_all)]
    pub fn from_language<C>(l: Vec<C>, m: M) -> Self
    where
        C: Into<E> + std::fmt::Debug,
        // L: IntoIterator<Item = C>, // + std::iter::FromIterator<C>,
    {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::Not));
        nfa.entry.insert(prior);
        for c in l {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
        nfa
    }

    #[tracing::instrument(skip_all)]
    pub fn from_symbols(l: &Vec<E>, m: M) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::Not));
        nfa.entry.insert(prior);
        for criteria in l.clone() {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(NfaEdge { criteria }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
        nfa
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn accepts<C>(&self, path: &Vec<C>) -> bool
    where
        E: Accepts<C>,
        C: Into<E> + Clone + std::fmt::Debug,
    {
        self.terminal_on(path, &|_| true)
    }

    /// TODO: Returning early rather than collecting all terminal states within the closure of the
    /// input is incorrect. This should be changed to visit all possible nodes.
    #[tracing::instrument(skip(filter, self), ret)]
    pub fn terminal_on<C>(
        &self,
        single_path: &Vec<C>,
        filter: &impl Fn(&LRSemantics) -> bool,
    ) -> bool
    where
        E: Accepts<C>,
        C: Into<E> + Clone + std::fmt::Debug,
    {
        for i in &self.entry {
            let i: NfaIndex = *i as NfaIndex;
            let mut stack: Vec<_> = Default::default();
            stack.push((i, single_path.clone()));
            while let Some((i, s)) = stack.pop() {
                let mut l = s.clone().into_iter();
                match &l.next() {
                    Some(c) => {
                        if let Some(v) = self.edges_from(i) {
                            for (target, edge) in v {
                                let edge = self.edge(edge);
                                if edge.accepts(c.clone().to_owned()) {
                                    // push target onto stack
                                    stack.push((*target, l.clone().collect()));
                                }
                            }
                        }
                    }
                    // Could just push these results onto a return stack instead
                    // just collect all terminal states period and return them instead of a bool
                    // TODO: remove use of Default here
                    None => {
                        // todo: clarify, maybe accepting should not check for rejection
                        if self.node_rejecting(i) {
                            return false;
                        } else if self.node_accepting(i) {
                            return filter(&self.node(i).chirality);
                        }
                    }
                }
            }
        }
        false
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn node_accepting(&self, i: NfaIndex) -> bool {
        match &self.node(i).state {
            Terminal::Not => false,
            Terminal::Accept(_) => true,
            Terminal::Reject(_) => false,
        }
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn node_rejecting(&self, i: NfaIndex) -> bool {
        match &self.node(i).state {
            Terminal::Not => false,
            Terminal::Accept(_) => false,
            Terminal::Reject(_) => true,
        }
    }
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: std::fmt::Debug + Clone + BranchProduct<E> + Eq + std::hash::Hash + Default + Universal,
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
{
    /// An intersection NFA only has accepting states where both input NFAs have accepting states
    #[tracing::instrument(skip_all)]
    pub fn intersection(&self, other: &Self) -> Self
    where
        E: Accepts<E>,
    {
        // For DFAs this is the cross-product
        let mut a = self.clone();
        a.set_chirality(LRSemantics::L);
        let mut b = other.clone();
        b.set_chirality(LRSemantics::R);
        let union = a.union(&b);
        // println!("XXX\n{a:?}\n{b:?}\n{union:?}");
        // FIXME accepting_paths is illogical, this must respect all terminal states
        // ... .terminal_paths() -> Paths (where Paths additionally stores terminal state and/or M)
        let paths = union.accepting_paths();
        // if a method here returned all terminal states with their associated paths,
        // (matt says intersection is a conjunction)
        // then each terminal state could be marked as in conjunction
        if paths.lr.is_empty() {
            return Nfa::universal(Default::default()).negate();
        }
        let lr_paths: Vec<_> = paths.lr.iter().collect();
        lr_paths[1..].iter().fold(
            Nfa::from_symbols(lr_paths[0], Default::default()),
            |acc, cur| acc.union(&Nfa::from_symbols(cur, Default::default())),
        )
    }

    // TODO: remove use of Default
    #[tracing::instrument(skip_all, ret)]
    pub(crate) fn accepting_paths(&self) -> Paths<E>
    where
        E: Accepts<E>,
    {
        // (current node id, current path: Vec<_>)
        let mut stack: Vec<(NfaIndex, Vec<E>)> = self
            .entry
            .iter()
            .cloned()
            .map(|e| (e, Default::default()))
            .collect();
        let mut paths: Paths<E> = Default::default();

        while let Some((current_node, current_path)) = stack.pop() {
            // for each edge from current node, append the edge criteria to a copy of the path
            // and push the edge target on the stack with the new path

            // if the current node is an accepting state, push the path onto Paths based on the chirality
            if self.node_accepting(current_node) {
                match self.node(current_node).chirality {
                    LRSemantics::L => {
                        paths.l.insert(current_path.clone());
                    }
                    LRSemantics::R => {
                        paths.r.insert(current_path.clone());
                    }
                    LRSemantics::LR => {
                        paths.lr.insert(current_path.clone());
                    }
                    LRSemantics::None => {
                        println!("🏡🏡🏡🏡🏡🏡🏡🏡🏡🏡");
                    }
                }
            }

            if let Some(edges) = self.edges_from(current_node) {
                for (next_node, edge_id) in edges {
                    let mut next_path = current_path.to_vec();
                    next_path.push(self.edge(edge_id).criteria.clone());
                    stack.push((*next_node, next_path));
                }
            }
        }
        for path in paths.l.iter() {
            if paths.r.remove(path) {
                paths.lr.insert(path.clone().to_owned());
            }
        }

        // ensure that all values in left_paths are not in left_paths.r
        for path in paths.lr.iter() {
            paths.l.remove(path);
            paths.r.remove(path);
        }

        let mut move_stuff = Default::default();
        (move_stuff, paths.l) = paths.l.iter().cloned().partition(|path| {
            self.terminal_on::<E>(path, &|t| {
                let r = t == &LRSemantics::R || t == &LRSemantics::LR;
                if r {
                    println!("found an r for l!?!? {path:?} {t:?}");
                }
                r
            })
        });

        paths.lr.extend(move_stuff.into_iter());

        (move_stuff, paths.r) = paths.r.iter().cloned().partition(|path| {
            self.terminal_on(path, &|t| t == &LRSemantics::L || t == &LRSemantics::LR)
        });

        paths.lr.extend(move_stuff.into_iter());

        paths
    }

    #[tracing::instrument]
    pub(crate) fn set_chirality(&mut self, c: LRSemantics) {
        self.nodes.iter_mut().for_each(|(_, mut n)| match n.state {
            Terminal::Not => (),
            Terminal::Reject(_) | Terminal::Accept(_) => n.chirality = c.clone(),
        })
    }
}

impl<M, E> Nfa<NfaNode<M>, E>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
    E: std::fmt::Debug + Clone + Eq + std::hash::Hash + std::default::Default,
{
    #[tracing::instrument]
    pub fn negate(&self) -> Self {
        // a NotToken here would need to flip edges as well
        // TODO: negate edges too
        let mut nfa: Nfa<NfaNode<M>, E> = self.clone();
        nfa.nodes = nfa
            .nodes
            .into_iter()
            .map(|(i, n)| (i, n.negate()))
            .collect();
        nfa
    }
}

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

impl<N, E> Default for Nfa<N, E>
where
    E: Eq + std::hash::Hash + std::default::Default,
{
    fn default() -> Self {
        Self {
            count: Default::default(),
            entry: Default::default(),
            nodes: Default::default(),
            edges: Default::default(),
            transitions: Default::default(),
            // chirality: Default::default(),
        }
    }
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: std::fmt::Debug + Clone + Universal + Eq + std::hash::Hash + std::default::Default,
    M: std::fmt::Debug + Clone + PartialOrd + Ord + std::default::Default,
{
    #[tracing::instrument(skip_all)]
    pub fn universal(m: M) -> Self {
        let mut nfa: Self = Default::default();
        let prior = nfa.add_node(NfaNode::new(Terminal::Not));
        let target = nfa.add_node(NfaNode::new(Terminal::Accept(m)));
        let _ = nfa.add_edge(
            NfaEdge {
                criteria: E::universal(),
            },
            prior,
            target,
        );
        nfa.entry.insert(prior);
        nfa
    }
}

impl<N, E> Nfa<N, E>
where
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
    #[tracing::instrument(skip_all)]
    pub(crate) fn index(&mut self) -> NfaIndex {
        self.count += 1;
        self.count as NfaIndex
    }

    #[tracing::instrument(skip_all)]
    pub fn add_node(&mut self, n: N) -> NfaIndex {
        let i = self.index();
        self.nodes.insert(i, n);
        i
    }

    #[tracing::instrument(skip_all)]
    pub fn node(&self, i: NfaIndex) -> &N {
        self.nodes.get(&i).unwrap()
    }

    /// Panic if unknown
    #[tracing::instrument(skip_all)]
    pub fn node_mut(&mut self, i: NfaIndex) -> &mut N {
        self.nodes.get_mut(&i).unwrap()
    }

    #[tracing::instrument(skip_all)]
    pub fn edge(&self, i: &NfaIndex) -> &E {
        self.edges.get(i).unwrap()
    }

    #[tracing::instrument(skip_all)]
    /// E is the edge weight, usually NfaEdge
    pub fn add_edge(&mut self, edge: E, source: NfaIndex, target: NfaIndex) -> NfaIndex {
        let i = self.index();
        self.edges.insert(i, edge);
        let entry = match self.transitions.entry(source) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Default::default()),
        };
        entry.push((target, i));
        i
    }

    /// Returns the edges from a given node in tuple format
    ///
    /// source node index -> (target node index, edge index)
    #[inline]
    #[tracing::instrument(skip_all, ret)]
    pub fn edges_from(&self, i: NfaIndex) -> Option<&Vec<(NfaIndex, NfaIndex)>> {
        self.transitions.get(&i)
    }
}

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: Clone,
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
    fn edge_by_kind(&self, i: NfaIndex, kind: &E) -> Vec<(NfaIndex, NfaIndex)> {
        self.transitions
            .get(&i)
            .unwrap_or(&vec![])
            .iter()
            .filter(|(_target_node_id, edge_id)| self.edge(edge_id).criteria == *kind)
            .cloned()
            .collect()
    }

    #[tracing::instrument(skip_all)]
    pub fn concatenate(&self, other: &Self) -> Self {
        let mut cat = self.clone();
        cat.count += other.count;
        cat.count += 1;
        cat.entry.extend(other.entry.iter().map(|i| i + self.count));
        cat.nodes.extend(
            other
                .nodes
                .iter()
                .map(|(i, n)| ((i + self.count), n.clone())),
        );
        cat.edges.extend(
            other
                .edges
                .iter()
                .map(|(i, e)| ((i + self.count), e.clone())),
        );
        cat.transitions
            .extend(other.transitions.iter().map(|(i, v)| {
                (
                    (i + self.count),
                    v.iter()
                        .map(|(t, e)| ((t + self.count), (e + self.count)))
                        .collect(),
                )
            }));
        cat
    }
}

#[derive(Default, Debug)]
// ? 6 fields, 1 for accepting and 1 for rejecting ?
// 1. need to visit all states and not return early
// 2. .... assume heterogenous NFAs
// 3. any path in L or R which is rejected is not in LR accepted
pub(crate) struct Paths<E: std::hash::Hash> {
    pub(crate) l: HashSet<Vec<E>>,
    pub(crate) lr: HashSet<Vec<E>>,
    pub(crate) r: HashSet<Vec<E>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LRSemantics {
    L,
    R,
    LR,
    None,
}

impl Default for LRSemantics {
    fn default() -> Self {
        LRSemantics::None
    }
}

impl LRSemantics {
    #[tracing::instrument(ret)]
    pub(crate) fn sum(&self, other: &LRSemantics) -> LRSemantics {
        match (self, other) {
            (x, y) if x == y => x.clone(),
            (_x, LRSemantics::LR) | (LRSemantics::LR, _x) => LRSemantics::LR,
            (LRSemantics::R, LRSemantics::L) | (LRSemantics::L, LRSemantics::R) => LRSemantics::LR,
            (x, LRSemantics::None) | (LRSemantics::None, x) => x.clone(),
            (_, _) => unreachable!(),
        }
    }
}
