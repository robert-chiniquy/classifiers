// use a u32 for one and a u64 for the other so that it's impossible to accidentally treat a node id as an edge id or vice-versa
pub type NodeId = u32;
pub type EdgeId = u64;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nfa<N, E>
where
    E: Eq + std::hash::Hash + Default,
{
    pub(crate) node_count: NodeId,
    pub(crate) edge_count: EdgeId,
    pub(crate) entry: NodeId,
    pub(crate) nodes: BTreeMap<NodeId, N>,
    pub(crate) edges: BTreeMap<EdgeId, E>,
    /// source node index -> Vec<(target node index, edge index)>
    // TODO: just split into 2 maps
    pub(crate) transitions: BTreeMap<NodeId, Vec<(NodeId, EdgeId)>>,
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: std::fmt::Debug + Clone + PartialOrd + Ord,
{
    #[tracing::instrument(skip_all)]
    pub fn from_language<C>(l: Vec<C>, m: Option<M>) -> Self
    where
        C: Into<E> + std::fmt::Debug,
    {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::InverseInclude(m)));
        nfa.entry = prior;
        for c in l {
            let target = nfa.add_node(NfaNode::new(Terminal::InverseInclude(m)));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Include(m);
        nfa
    }

    #[tracing::instrument(skip_all)]
    pub fn from_symbols(l: &[E], m: Option<M>) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::InverseInclude(m)));
        nfa.entry = prior;
        for criteria in l {
            let target = nfa.add_node(NfaNode::new(Terminal::InverseInclude(m)));
            let _ = nfa.add_edge(
                NfaEdge {
                    criteria: criteria.clone(),
                },
                prior,
                target,
            );
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Include(m);
        nfa
    }

    pub fn from_paths(paths: &[Vec<E>]) -> Self {
        let mut nfa: Self = Default::default();
        let mut items = paths.iter();

        if let Some(first) = items.next() {
            let init = Self::from_symbols(first, Default::default());
            nfa = items.fold(init, |acc, cur| {
                acc.union(&Self::from_symbols(cur, Default::default()))
            })
        }
        nfa
    }

    // #[tracing::instrument(skip(self), ret)]
    pub fn accepts<C>(&self, path: &Vec<C>) -> Result<bool, GeneralError>
    where
        E: Accepts<C>,
        C: Into<E> + Clone + std::fmt::Debug,
    {
        self.terminal_on(path, &|_| true)
    }

    /// TODO: Returning early rather than collecting all terminal states within the closure of the
    /// input is incorrect. This should be changed to visit all possible nodes.
    // #[tracing::instrument(skip(filter, self), ret)]
    pub fn terminal_on<C>(
        &self,
        #[allow(clippy::ptr_arg)] single_path: &Vec<C>,
        filter: &impl Fn(&LRSemantics) -> bool,
    ) -> Result<bool, GeneralError>
    where
        E: Accepts<C>,
        C: Into<E> + Clone + std::fmt::Debug,
    {
        let i: NodeId = self.entry;
        let mut stack: Vec<_> = Default::default();
        stack.push((i, single_path.clone()));
        while let Some((i, s)) = stack.pop() {
            let mut l = s.clone().into_iter();
            match &l.next() {
                Some(c) => {
                    for (target, edge) in self.edges_from(i) {
                        let edge = self.edge(edge);
                        let accepts = edge.unwrap().accepts(c);
                        if accepts {
                            // push target onto stack
                            stack.push((*target, l.clone().collect()));
                        }
                    }
                }
                // Could just push these results onto a return stack instead
                // just collect all terminal states period and return them instead of a bool
                // TODO: remove use of Default here
                None => {
                    // todo: clarify, maybe accepting should not check for rejection
                    if self.node_rejecting(i) {
                        return Ok(false);
                    } else if self.node_accepting(i) {
                        return Ok(filter(&self.node(i).chirality));
                    }
                }
            }
        }

        Ok(false)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn node_accepting(&self, i: NodeId) -> bool {
        match &self.node(i).state {
            Terminal::None => false,
            Terminal::Include(_) => true,
            Terminal::Exclude(_) => false,
        }
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn node_rejecting(&self, i: NodeId) -> bool {
        match &self.node(i).state {
            Terminal::None => false,
            Terminal::Include(_) => false,
            Terminal::Exclude(_) => true,
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn add_node_with_edge(
        &mut self,
        criteria: E,
        source: NodeId,
        target: NfaNode<M>,
    ) -> NodeId {
        let target = self.add_node(target);
        let _ = self.add_edge(NfaEdge { criteria }, source, target);
        target
    }
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    E: ElementalLanguage<E>,
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
{
    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// An intersection NFA only has accepting states where both input NFAs have accepting states
    // #[tracing::instrument(skip_all)]
    pub fn intersection(&self, other: &Self) -> Self
    where
        E: Accepts<E>,
    {
        todo!()
    }

    // TODO: remove use of Default
    #[tracing::instrument(skip_all, ret)]
    pub(crate) fn accepting_paths(&self) -> Paths<E>
    where
        E: Accepts<E>,
    {
        // (current node id, current path: Vec<_>)
        // let mut stack: Vec<(_, Vec<E>)> = vec![(self.entry, Default::default())];

        // let mut paths: Paths<E> = Default::default();

        // while let Some((current_node, current_path)) = stack.pop() {
        //     // for each edge from current node, append the edge criteria to a copy of the path
        //     // and push the edge target on the stack with the new path

        //     // if the current node is an accepting state, push the path onto Paths based on the chirality
        //     if self.node_accepting(current_node) {
        //         match self.node(current_node).chirality {
        //             LRSemantics::L => {
        //                 paths.l.insert(current_path.clone());
        //             }
        //             LRSemantics::R => {
        //                 paths.r.insert(current_path.clone());
        //             }
        //             LRSemantics::LR => {
        //                 paths.lr.insert(current_path.clone());
        //             }
        //             LRSemantics::None => {
        //                 paths.none.insert(current_path.clone());
        //             }
        //         }
        //     }

        //     for (next_node, edge_id) in self.edges_from(current_node) {
        //         let mut next_path = current_path.to_vec();
        //         next_path.push(self.edge(edge_id).unwrap().criteria.clone());
        //         stack.push((*next_node, next_path));
        //     }
        // }
        // for path in paths.l.iter() {
        //     if paths.r.remove(path) {
        //         paths.lr.insert(path.clone().to_owned());
        //     }
        // }

        // // ensure that all values in left_paths are not in left_paths.r
        // for path in paths.lr.iter() {
        //     paths.l.remove(path);
        //     paths.r.remove(path);
        // }

        // #[allow(unused_assignments)]
        // let mut move_stuff = Default::default();
        // (move_stuff, paths.l) = paths.l.iter().cloned().partition(|path| {
        //     self.terminal_on::<E>(path, &|t| {
        //         let r = t == &LRSemantics::R || t == &LRSemantics::LR;
        //         if r {
        //             println!("found an r for l!?!? {path:?} {t:?}");
        //         }
        //         r
        //     })
        //     .unwrap()
        // });

        // paths.lr.extend(move_stuff.into_iter());

        // (move_stuff, paths.r) = paths.r.iter().cloned().partition(|path| {
        //     self.terminal_on(path, &|t| t == &LRSemantics::L || t == &LRSemantics::LR)
        //         .unwrap()
        // });

        // paths.lr.extend(move_stuff.into_iter());

        // paths
        todo!()
    }

    #[tracing::instrument]
    pub(crate) fn set_chirality(&mut self, c: LRSemantics) {
        self.nodes.iter_mut().for_each(|(_, mut n)| match n.state {
            Terminal::None => (),
            Terminal::Exclude(_) | Terminal::Include(_) => n.chirality = c.clone(),
        })
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
        let prior = nfa.add_node(NfaNode::new(Terminal::None));
        let target = nfa.add_node(NfaNode::new(Terminal::Include(m)));
        let _ = nfa.add_edge(
            NfaEdge {
                criteria: E::universal(),
            },
            prior,
            target,
        );
        nfa.entry = prior;
        nfa
    }
}

static EMPTY_VECTOR: Vec<(NodeId, EdgeId)> = Vec::new();

impl<N, E> Nfa<N, E>
where
    E: Eq + Clone + std::hash::Hash + std::default::Default,
    N: Accepting,
{
    // #[tracing::instrument(skip_all)]
    pub(crate) fn node_index(&mut self) -> NodeId {
        let i = self.node_count;
        self.node_count += 1;
        i
    }

    pub(crate) fn edge_index(&mut self) -> EdgeId {
        self.edge_count += 1;
        self.edge_count
    }

    // #[tracing::instrument(skip_all)]
    pub fn add_node(&mut self, n: N) -> NodeId {
        let i = self.node_index();
        self.nodes.insert(i, n);
        i
    }

    // #[tracing::instrument(skip_all)]
    pub fn node(&self, i: NodeId) -> &N {
        self.nodes.get(&i).unwrap()
    }

    /// Panic if unknown
    // #[tracing::instrument(skip_all)]
    pub fn node_mut(&mut self, i: NodeId) -> &mut N {
        self.nodes.get_mut(&i).unwrap()
    }

    // #[tracing::instrument(skip_all)]
    pub fn edge(&self, i: &EdgeId) -> Option<&E> {
        self.edges.get(i)
    }

    pub fn edge_mut(&mut self, i: EdgeId) -> Option<&mut E> {
        self.edges.get_mut(&i)
    }

    #[tracing::instrument(skip_all)]
    /// E is the edge weight, usually NfaEdge
    pub fn add_edge(&mut self, edge: E, source: NodeId, target: NodeId) -> EdgeId {
        let i = self.edge_index();

        self.edges.insert(i, edge);
        let entry = match self.transitions.entry(source) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Default::default()),
        };
        entry.push((target, i));
        i
    }

    pub fn remove_edge(&mut self, source: NodeId, edge: EdgeId) {
        let t = self.transitions.get(&source);
        if let Some(t) = t {
            self.transitions.insert(
                source,
                t.iter()
                    .filter(|(_, edge_id)| *edge_id != edge)
                    .cloned()
                    .collect(),
            );
        }
        // TODO: empty remaining outbound edge list?
        self.edges.remove(&edge).unwrap();
    }

    /// Returns the edges from a given node in tuple format
    ///
    /// source node index -> (target node index, edge index)
    #[inline]
    // #[tracing::instrument(skip_all, ret)]
    pub fn edges_from(&self, i: NodeId) -> &Vec<(NodeId, EdgeId)> {
        self.transitions.get(&i).unwrap_or(&EMPTY_VECTOR)
    }

    #[inline]
    // #[tracing::instrument(skip_all, ret)]
    // returns a Vec of (source node, edge) where the source matches the requested NodeId
    pub fn edges_to(&self, to: NodeId) -> Vec<(NodeId, EdgeId)> {
        let mut ret = vec![];
        for (s, edges) in &self.transitions {
            for (t, e) in edges {
                if *t == to {
                    ret.push((*s, *e));
                }
            }
        }
        ret
    }

    // Shouldn't this always be able to return an ID for an edge?
    pub fn destination(&self, e: &EdgeId) -> Option<NodeId> {
        // let values: Vec<(_,_)>  =
        for value in self.transitions.values() {
            for (target, edge) in value {
                if edge == e {
                    return Some(*target);
                }
            }
        }
        None
    }

    // How does this handle cycles? convergence?
    // FIXME(not urgent): Definitionally this should only remove nodes which are solely reachable
    // from the subtree root.
    // Ensure any edges pointing to this node are removed also.
    #[tracing::instrument(skip(self))]
    pub(crate) fn delete_subtree(&mut self, sub_tree: &NodeId) {
        let mut dead_nodes = vec![*sub_tree].iter().cloned().collect::<HashSet<NodeId>>();
        println!("self transitions: {:?}", self.transitions);
        let mut dead_edges: HashSet<_> = self.edges_to(*sub_tree).iter().map(|(_, e)| *e).collect();

        println!("dead nodes: {:?}, dead_edges: {:?}", dead_nodes, dead_edges);
        // Find the closure of the subtree
        let mut stack = vec![*sub_tree];

        while let Some(n) = stack.pop() {
            for (target, next_edge) in self.edges_from(n).clone() {
                if dead_nodes.contains(&target.clone()) {
                    continue;
                }
                dead_nodes.insert(target);
                dead_edges.insert(next_edge);
                stack.push(target);
            }
        }

        println!("dead nodes: {:?}, dead_edges: {:?}", dead_nodes, dead_edges);

        for n in dead_nodes {
            if n == self.entry {
                self.nodes.clear();
                self.transitions.clear();
                self.edges.clear();
                return;
            }
            self.nodes.remove(&n);
            self.transitions.remove(&n);
        }

        for e in dead_edges {
            self.edges.remove(&e);
            // println!("XX {e} {:?}", self.transitions);
            self.transitions
                .iter_mut()
                .for_each(|(_, edges)| edges.retain(|(_, ee)| *ee != e));
            // println!("YY {e} {:?}", self.transitions);
        }
    }

    /// Restrict nodes and edges to those reachable from self.entry
    // - for any multi-edges of a given element variant to a given target,
    // condense to a single composite edge
    // - after this, any TokenSets which are subsets of a NotTokenSet to the same target
    // can be removed after subtracting the chars from the NotTokenSet,
    // - and the reverse for NotTokenSet against a superset TokenSet
    // - filter the dfa to only nodes which are reachable by root

    fn remove_node_set(&mut self, to_remove: HashSet<NodeId>) {
        println!("dropping nodes: {to_remove:?}");

        let mut to_remove_edges : HashSet<EdgeId>  = Default::default();
        for (s, edges) in &self.transitions {
            if to_remove.contains(s) {
                to_remove_edges.extend(edges.iter().map(|(_, e)| e.to_owned()));
                continue;
            }
            // drop all edges whose target is a node in to_remove
            for (t, e) in edges {
                if to_remove.contains(t) {
                    to_remove_edges.insert(e.to_owned());
                }
            }
        }

        self.edges.retain(|e, _| !to_remove_edges.contains(e));
        self.nodes.retain(|n, _| !to_remove.contains(n));
        self.transitions = self.transitions.iter().flat_map(|(s, edges)| {
            if to_remove.contains(s) {
                return None;
            }
            let edges = edges.iter().filter(|(t, e)| !to_remove.contains(t) || !to_remove_edges.contains(e)).cloned().collect();
            Some((*s, edges))
        }).collect();

    }

    pub(crate) fn shake(&mut self) {
        self.remove_node_set(self.not_accepting_branch());
        println!("transitions post shakup: {:?}", self.transitions);
    }

    /// recurses, returning true if a subtree reaches an accepting state
    fn accepting_branch(&self, node_id: &NodeId, visited: &mut HashSet<NodeId>, alive: &mut HashSet<NodeId>) -> bool {
        if visited.contains(node_id) {
            return alive.contains(node_id);
        }
        visited.insert(*node_id);

        let mut is_alive = false;
        if self.node(*node_id).accepting() {
            alive.insert(*node_id);
            is_alive = true;
        }

        for (target, _) in self.edges_from(*node_id) {
            if self.accepting_branch(&target.to_owned(), visited, alive) {
                is_alive = true;
                alive.insert(*node_id);
            }
        }
        is_alive
    }

    /// finds nodes that don't lead to an accepting node, including live/unreachable nodes.
    pub(crate) fn not_accepting_branch(&self) -> HashSet<NodeId>{
        let mut alive = Default::default();
        let mut visited = Default::default();
        self.accepting_branch(&self.entry, &mut visited, &mut alive);

        let all_nodes = self.nodes.keys().cloned().collect::<HashSet<_>>();
        // println!("entry: {:?}\nalive: {alive:?}\nvisited: {visited:?}\ndead: {:?}", self.entry, &visited - &alive);
        &all_nodes - &alive
    }

}

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: Clone,
    E: std::fmt::Debug + Eq + Clone + std::hash::Hash + std::default::Default,
{
    #[tracing::instrument(skip(self), ret)]
    pub(super) fn edge_by_kind(&self, i: NodeId, kind: &E) -> Vec<(NodeId, EdgeId)> {
        self.transitions
            .get(&i)
            .unwrap_or(&vec![])
            .iter()
            .filter(|(_target_node_id, edge_id)| match self.edges.get(edge_id) {
                Some(e) => e.criteria == *kind,
                None => false,
            })
            .cloned()
            .collect()
    }
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
    // #[tracing::instrument(ret)]
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

impl<N, E> Default for Nfa<N, E>
where
    E: Eq + std::hash::Hash + std::default::Default,
{
    fn default() -> Self {
        Self {
            node_count: Default::default(),
            edge_count: Default::default(),
            entry: Default::default(),
            nodes: Default::default(),
            edges: Default::default(),
            transitions: Default::default(),
            // chirality: Default::default(),
        }
    }
}

impl std::fmt::Display for LRSemantics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            LRSemantics::L => "L",
            LRSemantics::R => "R",
            LRSemantics::LR => "LR",
            LRSemantics::None => "",
        };
        f.write_str(s)
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
    pub(crate) none: HashSet<Vec<E>>,
}
