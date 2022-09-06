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
        nfa.entry = prior;
        for c in l {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
        nfa
    }

    #[tracing::instrument(skip_all)]
    pub fn from_symbols(l: &[E], m: M) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::Not));
        nfa.entry = prior;
        for criteria in l {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(
                NfaEdge {
                    criteria: criteria.clone(),
                },
                prior,
                target,
            );
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
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
                        let accepts = edge.accepts(c);
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
            Terminal::Not => false,
            Terminal::Accept(_) => true,
            Terminal::Reject(_) => false,
        }
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn node_rejecting(&self, i: NodeId) -> bool {
        match &self.node(i).state {
            Terminal::Not => false,
            Terminal::Accept(_) => false,
            Terminal::Reject(_) => true,
        }
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
        // For DFAs this is the cross-product
        let mut a = self.clone();
        a.set_chirality(LRSemantics::L);

        let mut b = other.clone();
        b.set_chirality(LRSemantics::R);

        if a.size() == 0 || b.size() == 0 {
            return Default::default();
        }
        let union = a.product(&b);
        // println!("XXX\n{a:?}\n{b:?}\n{union:?}");
        // FIXME accepting_paths is illogical, this must respect all terminal states (Accept & Reject, but not Not)
        // ... .terminal_paths() -> Paths (where Paths additionally stores terminal state and/or M)
        let paths = union.accepting_paths();

        // if a method here returned all terminal states with their associated paths,
        // (matt says intersection is a conjunction)
        // then each terminal state could be marked as in conjunction
        // println!("\n\n intersecting paths: {:?}\n\n", paths.lr);

        // [*] ¥ [?, !aa, ***] -> ****, !aa, !aa***, ***, !aa*, !aa
        // [*] ¥ [*** , !aa, ?] -> !aa, ?

        // match (paths.l.is_empty(), paths.lr.is_empty(), paths.r.is_empty());
        if paths.lr.is_empty() {
            // println!("making default");
            return Default::default();
            // return Nfa::universal(Default::default()).negate();
        }
        // TODO: reduce LR paths
        let lr_paths: Vec<_> = paths.lr.iter().collect();
        lr_paths[1..].iter().fold(
            Nfa::from_symbols(lr_paths[0], Default::default()),
            |acc, cur| {
                println!("adding in {:?}", cur);
                acc.union(&Nfa::from_symbols(cur, Default::default()))
            },
        )
    }

    // TODO: remove use of Default
    #[tracing::instrument(skip_all, ret)]
    pub(crate) fn accepting_paths(&self) -> Paths<E>
    where
        E: Accepts<E>,
    {
        // (current node id, current path: Vec<_>)
        let mut stack: Vec<(_, Vec<E>)> = vec![(self.entry, Default::default())];

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
                        paths.none.insert(current_path.clone());
                    }
                }
            }

            for (next_node, edge_id) in self.edges_from(current_node) {
                let mut next_path = current_path.to_vec();
                next_path.push(self.edge(edge_id).criteria.clone());
                stack.push((*next_node, next_path));
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

        #[allow(unused_assignments)]
        let mut move_stuff = Default::default();
        (move_stuff, paths.l) = paths.l.iter().cloned().partition(|path| {
            self.terminal_on::<E>(path, &|t| {
                let r = t == &LRSemantics::R || t == &LRSemantics::LR;
                if r {
                    println!("found an r for l!?!? {path:?} {t:?}");
                }
                r
            })
            .unwrap()
        });

        paths.lr.extend(move_stuff.into_iter());

        (move_stuff, paths.r) = paths.r.iter().cloned().partition(|path| {
            self.terminal_on(path, &|t| t == &LRSemantics::L || t == &LRSemantics::LR)
                .unwrap()
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
        nfa.entry = prior;
        nfa
    }
}

static EMPTY_VECTOR: Vec<(NodeId, EdgeId)> = Vec::new();

impl<N, E> Nfa<N, E>
where
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
    // #[tracing::instrument(skip_all)]
    pub(crate) fn node_index(&mut self) -> NodeId {
        self.node_count += 1;
        self.node_count
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
    pub fn edge(&self, i: &EdgeId) -> &E {
        self.edges.get(i).unwrap()
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
    pub fn edges_to(&self, i: NodeId) -> Vec<(NodeId, EdgeId)> {
        self.transitions
            .iter()
            .filter(|(_, edges)| edges.iter().any(|(target, _)| target == &i))
            .flat_map(|(_, t)| t)
            .cloned()
            .collect()
    }

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

    pub(crate) fn delete_subtree(&mut self, sub_tree: &NodeId) {
        let mut dead_nodes = vec![*sub_tree].iter().cloned().collect::<HashSet<NodeId>>();
        let mut dead_edges: HashSet<_> = Default::default();

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
        }
    }
}

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: Clone,
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
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
