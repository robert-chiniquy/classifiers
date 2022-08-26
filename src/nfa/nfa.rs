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
    E: std::fmt::Display
        + Eq
        + Clone
        + std::hash::Hash
        + Default
        + std::fmt::Debug
        + BranchProduct<E>,
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
    pub fn from_symbols(l: &[E], m: M) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::Not));
        nfa.entry.insert(prior);
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

    pub fn from_paths(paths: &Vec<Vec<E>>) -> Self {
        let mut nfa: Self = Default::default();
        let mut items = paths.iter();

        if let Some(first) = items.next() {
            let init = Self::from_symbols(first, Default::default());
            nfa = items.fold(init, |acc, cur| {
                acc.union(&Self::from_symbols(&cur, Default::default()))
            })
        }
        nfa
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn accepts<C>(&self, path: &Vec<C>) -> Result<bool, MatchingError>
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
    ) -> Result<bool, MatchingError>
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
                                let accepts = edge.accepts(c.clone().to_owned())?;
                                if accepts {
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
                            return Ok(false);
                        } else if self.node_accepting(i) {
                            return Ok(filter(&self.node(i).chirality));
                        }
                    }
                }
            }
        }
        Ok(false)
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

#[test]
fn test_accepting_paths() {
    tests::setup();
    let c = Classifier::literal("a");

    let n = c.compile::<Element, _, _>(());
    // assert!(n.accepts_string("aa"));

    let paths = n.accepting_paths();

    assert!(!paths.every_path().is_empty());
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    Vec<E>: Invertible,
    E: std::fmt::Display
        + std::fmt::Debug
        + Clone
        + BranchProduct<E>
        + Eq
        + std::hash::Hash
        + Default
        + Universal,
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
{
    pub fn size(&self) -> usize {
        return self.nodes.len();
    }

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

        if a.size() == 0 || b.size() == 0 {
            return Default::default();
        }
        let union = a.product(&b);
        // println!("XXX\n{a:?}\n{b:?}\n{union:?}");
        // FIXME accepting_paths is illogical, this must respect all terminal states
        // ... .terminal_paths() -> Paths (where Paths additionally stores terminal state and/or M)
        let paths = union.accepting_paths();

        // if a method here returned all terminal states with their associated paths,
        // (matt says intersection is a conjunction)
        // then each terminal state could be marked as in conjunction
        println!("\n\n🌮🌮🌮🌮 intersecting paths: {:?}\n\n", paths.lr);

        // [*] ¥ [?, !aa, ***] -> ****, !aa, !aa***, ***, !aa*, !aa
        // [*] ¥ [*** , !aa, ?] -> !aa, ?

        // match (paths.l.is_empty(), paths.lr.is_empty(), paths.r.is_empty());
        if paths.lr.is_empty() {
            println!("making default");
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
                        paths.none.insert(current_path.clone());
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

        #[allow(unused_assignments)]
        let mut move_stuff = Default::default();
        (move_stuff, paths.l) = paths.l.iter().cloned().partition(|path| {
            self.terminal_on::<E>(path, &|t| {
                let r = t == &LRSemantics::R || t == &LRSemantics::LR;
                if r {
                    println!("found an r for l!?!? {path:?} {t:?}");
                }
                r
            }).unwrap()
        });

        paths.lr.extend(move_stuff.into_iter());

        (move_stuff, paths.r) = paths.r.iter().cloned().partition(|path| {
            self.terminal_on(path, &|t| t == &LRSemantics::L || t == &LRSemantics::LR).unwrap()
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
        nfa.entry.insert(prior);
        nfa
    }
}

impl<N, E> Nfa<N, E>
where
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
    // #[tracing::instrument(skip_all)]
    pub(crate) fn index(&mut self) -> NfaIndex {
        self.count += 1;
        self.count as NfaIndex
    }

    // #[tracing::instrument(skip_all)]
    pub fn add_node(&mut self, n: N) -> NfaIndex {
        let i = self.index();
        self.nodes.insert(i, n);
        i
    }

    // #[tracing::instrument(skip_all)]
    pub fn node(&self, i: NfaIndex) -> &N {
        self.nodes.get(&i).unwrap()
    }

    /// Panic if unknown
    // #[tracing::instrument(skip_all)]
    pub fn node_mut(&mut self, i: NfaIndex) -> &mut N {
        self.nodes.get_mut(&i).unwrap()
    }

    // #[tracing::instrument(skip_all)]
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
    // #[tracing::instrument(skip_all, ret)]
    pub fn edges_from(&self, i: NfaIndex) -> Option<&Vec<(NfaIndex, NfaIndex)>> {
        self.transitions.get(&i)
    }
}

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: Clone,
    E: Eq + Clone + std::hash::Hash + std::default::Default,
{
    pub(super) fn edge_by_kind(&self, i: NfaIndex, kind: &E) -> Vec<(NfaIndex, NfaIndex)> {
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
            count: Default::default(),
            entry: Default::default(),
            nodes: Default::default(),
            edges: Default::default(),
            transitions: Default::default(),
            // chirality: Default::default(),
        }
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

impl<E> Paths<E>
where
    E: std::hash::Hash + Clone + Eq + std::fmt::Debug,
{
    #[tracing::instrument(ret)]
    pub(crate) fn every_path(&self) -> HashSet<Vec<E>> {
        self.l
            .iter()
            .chain(self.lr.iter())
            .chain(self.r.iter())
            .chain(self.none.iter())
            .cloned()
            .collect()
    }
}
