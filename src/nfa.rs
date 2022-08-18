use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

pub type NfaIndex = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Nfa<N, E: Eq> {
    count: usize,
    // Convenience for now, later switch to only a single initial node
    entry: BTreeSet<NfaIndex>,
    nodes: BTreeMap<NfaIndex, N>,
    edges: BTreeMap<NfaIndex, E>,
    /// source node index -> Vec<(target node index, edge index)>
    // TODO: just split into 2 maps
    transitions: BTreeMap<NfaIndex, Vec<(NfaIndex, NfaIndex)>>,
}

impl From<&str> for Nfa<NfaNode<()>, NfaEdge<Element>> {
    fn from(s: &str) -> Self {
        Nfa::from_str(s)
    }
}

impl<M, E> Nfa<NfaNode<M>, E>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq,
    E: std::fmt::Debug + Clone + Eq,
{
    #[tracing::instrument]
    pub fn negate(&self) -> Self {
        let mut nfa: Nfa<NfaNode<M>, E> = self.clone();
        nfa.nodes = nfa
            .nodes
            .into_iter()
            .map(|(i, n)| (i, n.negate()))
            .collect();
        nfa
    }
}

impl<E> Nfa<NfaNode<()>, NfaEdge<E>>
where
    E: std::fmt::Debug + Clone + Universal + Eq,
{
    #[tracing::instrument(skip_all)]
    pub fn universal() -> Self {
        let mut nfa: Self = Default::default();
        let prior = nfa.add_node(NfaNode {
            state: [Terminal::Not].into(),
        });
        let target = nfa.add_node(NfaNode {
            state: [Terminal::Accept(())].into(),
        });
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

impl Nfa<NfaNode<()>, NfaEdge<Element>> {
    #[tracing::instrument(skip_all)]
    pub fn from_str(s: &str) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode {
            state: [Terminal::Not].into(),
        });
        nfa.entry.insert(prior);
        for c in s.chars() {
            let target = nfa.add_node(NfaNode {
                state: [Terminal::Not].into(),
            });
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = [Terminal::Accept(())].into();
        nfa
    }

    /// TODO: Returning early rather than collecting all terminal states within the closure of the
    /// input is incorrect. This should be changed to visit all possible nodes.
    #[tracing::instrument]
    pub fn accepts(&self, s: &str) -> bool {
        for i in &self.entry {
            let i: NfaIndex = *i as NfaIndex;
            let mut stack: Vec<_> = Default::default();
            stack.push((i, s));
            while let Some((i, s)) = stack.pop() {
                match s.chars().next() {
                    Some(c) => {
                        if let Some(v) = self.edges_from(i) {
                            for (target, edge) in v {
                                let edge = self.edge(edge);
                                if edge.accepts(&c) {
                                    // push target onto stack
                                    stack.push((*target, &s[1..]));
                                }
                            }
                        }
                    }
                    // Could just push these results onto a return stack instead
                    None => {
                        if self.node(i).state.contains(&Terminal::Reject(())) {
                            return false;
                        } else if self.node(i).state.contains(&Terminal::Accept(())) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EdgeTransition {
    Advance,
    Stay,
    Drop,
}

#[derive(Debug)]
pub struct NfaBranch<El> {
    kind: El,
    left: EdgeTransition,
    right: EdgeTransition,
}

impl<E: Clone> NfaBranch<E> {
    pub fn new(kind: E, left: EdgeTransition, right: EdgeTransition) -> Self {
        debug_assert!(!(left == EdgeTransition::Drop && right == EdgeTransition::Drop));
        Self { kind, left, right }
    }
}

impl<N, E> Nfa<N, NfaEdge<E>>
where
    N: std::fmt::Debug + Clone + Default + NodeSum,
    E: std::fmt::Debug + Clone + BranchProduct<E> + Eq,
{
    /// An intersection NFA only has accepting states where both input NFAs have accepting states
    #[tracing::instrument(skip_all)]
    pub fn intersection(&self, other: &Self) -> Self {
        // this requires an NFA product
        todo!()
    }

    /// A union is a non-mimimal NFA with the same resulting states for every input as
    /// either of the two input NFAs.
    // Whatever invariant is enforced here, assume that the inputs have that invariant
    // 2 methods?:
    // - one for a single graph with multiple enter states
    // - one for two distinct graphs
    #[tracing::instrument(skip_all)]
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
        while let Some((self_id, other_id, working_union_node_id)) = stack.pop() {
            let self_edges = self.edges_from(*self_id);
            let other_edges = other.edges_from(*other_id);
            if self_edges == None && other_edges == None {
                return union;
            } else if self_edges == None {
                union.copy_subtree(&working_union_node_id, other, other_id);
            } else if other_edges == None {
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
                                EdgeTransition::Drop => None,
                            };
                            let right_node_id = match right {
                                EdgeTransition::Advance => Some(other_target_node_id),
                                EdgeTransition::Stay => Some(other_id),
                                EdgeTransition::Drop => None,
                            };
                            let new_node = match (left_node_id, right_node_id) {
                                (None, None) => unreachable!(),
                                (None, Some(right_node_id)) => other.node(*right_node_id).clone(),
                                (Some(left_node_id), None) => self.node(*left_node_id).clone(),
                                (Some(left_node_id), Some(right_node_id)) => {
                                    self.node(*left_node_id).sum(other.node(*right_node_id))
                                }
                            };
                            let next_working_node_id =
                                union.branch(&working_union_node_id, kind, new_node);

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
                let new_node = self.add_node(new_node);
                let _edge = self.add_edge(NfaEdge { criteria: kind }, *working_node_id, new_node);
                new_node
            }
        }
    }

    // there is no convergence yet but this is convergence-safe
    #[tracing::instrument(skip(source))]
    fn copy_subtree(&mut self, copy_target_node: &NfaIndex, source: &Self, source_node: &NfaIndex) {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NfaNode<M>
where
    M: std::fmt::Debug + Clone,
{
    state: BTreeSet<Terminal<M>>,
}

impl<N> Default for NfaNode<N>
where
    N: Default + std::fmt::Debug + Clone,
{
    fn default() -> Self {
        Self {
            state: Default::default(),
        }
    }
}

pub trait NodeSum {
    fn sum(&self, other: &Self) -> Self;
    fn sum_mut(&mut self, other: &Self);
}

impl<M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq> NodeSum for NfaNode<M> {
    fn sum(&self, other: &Self) -> Self {
        NfaNode {
            state: self.state.union(&other.state).cloned().collect(),
        }
    }

    fn sum_mut(&mut self, other: &Self) {
        self.state = self.state.union(&other.state).cloned().collect();
    }
}

impl<N: std::fmt::Debug + Clone> std::fmt::Display for NfaNode<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.state))
    }
}

impl<M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq> NfaNode<M> {
    #[tracing::instrument(ret)]
    fn negate(&self) -> Self {
        let mut node = self.clone();
        node.state = node.state.iter().map(|s| s.negate()).collect();
        node
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NfaEdge<E: Eq> {
    criteria: E,
}

pub trait Accepts<L> {
    fn accepts(&self, l: L) -> bool;
}

impl<L, E> Accepts<L> for NfaEdge<E>
where
    E: Accepts<L> + Eq,
{
    #[tracing::instrument(skip_all)]
    fn accepts(&self, l: L) -> bool {
        self.criteria.accepts(l)
    }
}

impl<E: std::fmt::Display + Eq> std::fmt::Display for NfaEdge<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.criteria.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Element {
    Token(char),
    Question,
    Star,
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Element::Token(c) => format!("'{c}'"),
                Element::Question => "?".to_string(),
                Element::Star => "*".to_string(),
            }
        ))
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Token(c) => c == l,
            Element::Question => true,
            Element::Star => true,
        }
    }
}

pub trait BranchProduct<E> {
    fn product(a: &Self, b: &Self) -> Vec<NfaBranch<E>>;
}

impl BranchProduct<Element> for Element {
    #[tracing::instrument(ret)]
    fn product(a: &Self, b: &Self) -> Vec<NfaBranch<Element>> {
        use EdgeTransition::*;
        use Element::*;

        match (a, b) {
            (Star, Star) => {
                // three edges, L, R, L+R
                vec![
                    NfaBranch::new(Star, Advance, Stay),
                    NfaBranch::new(Star, Stay, Advance),
                    NfaBranch::new(Star, Advance, Advance),
                ]
            }
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (Token(c1), Token(c2)) => {
                if c1 == c2 {
                    // advance both
                    vec![NfaBranch::new(*a, Advance, Advance)]
                } else {
                    // disjoint
                    vec![
                        NfaBranch::new(*a, Advance, Drop),
                        NfaBranch::new(*b, Drop, Advance),
                    ]
                }
            }
            (Star, Token(_)) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(*b, Stay, Advance),
                    NfaBranch::new(*b, Advance, Advance),
                ]
            }
            (Star, Question) => {
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(Question, Stay, Advance),
                    NfaBranch::new(Question, Advance, Advance),
                ]
            }
            (Token(_), Star) => {
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(*a, Advance, Stay),
                    NfaBranch::new(*a, Advance, Advance),
                ]
            }
            (Question, Star) => {
                // The union path is ?
                // the star path is * minus ?
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(Question, Advance, Stay),
                    NfaBranch::new(Question, Advance, Advance),
                ]
            }
            (Token(_), Question) => {
                vec![
                    NfaBranch::new(Question, Drop, Advance),
                    NfaBranch::new(*a, Advance, Advance),
                ]
            }

            (Question, Token(_)) => {
                vec![
                    NfaBranch::new(Question, Advance, Drop),
                    NfaBranch::new(*b, Advance, Advance),
                ]
            }
        }
    }
}

pub trait Universal {
    fn universal() -> Self;
}

impl Universal for Element {
    fn universal() -> Self {
        Element::Star
    }
}

impl From<char> for Element {
    fn from(c: char) -> Self {
        match c {
            '*' => Element::Star,
            '?' => Element::Question,
            c => Element::Token(c),
        }
    }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Terminal<M> {
    Not,
    Accept(M),
    Reject(M),
}

impl<M> Default for Terminal<M> {
    fn default() -> Self {
        Terminal::Not
    }
}

impl<M> std::fmt::Debug for Terminal<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "."),
            Self::Accept(_) => f.debug_tuple("Accept").finish(),
            Self::Reject(_) => f.debug_tuple("Reject").finish(),
        }
    }
}

impl<M: std::fmt::Debug + Clone> Terminal<M> {
    #[tracing::instrument(ret)]
    fn negate(&self) -> Self {
        match self {
            Terminal::Not => Terminal::Not,
            Terminal::Accept(m) => Terminal::Reject(m.clone()),
            Terminal::Reject(m) => Terminal::Accept(m.clone()),
        }
    }
}

impl<N, E: Eq> Default for Nfa<N, E> {
    fn default() -> Self {
        Self {
            count: Default::default(),
            entry: Default::default(),
            nodes: Default::default(),
            edges: Default::default(),
            transitions: Default::default(),
        }
    }
}

impl<N, E: Eq + Clone> Nfa<N, E> {
    #[tracing::instrument(skip_all)]
    fn index(&mut self) -> NfaIndex {
        self.count += 1;
        self.count as NfaIndex
    }

    #[tracing::instrument(skip_all)]
    fn add_node(&mut self, n: N) -> NfaIndex {
        let i = self.index();
        self.nodes.insert(i, n);
        i
    }

    #[tracing::instrument(skip_all)]
    fn node(&self, i: NfaIndex) -> &N {
        self.nodes.get(&i).unwrap()
    }

    /// Panic if unknown
    #[tracing::instrument(skip_all)]
    fn node_mut(&mut self, i: NfaIndex) -> &mut N {
        self.nodes.get_mut(&i).unwrap()
    }

    #[tracing::instrument(skip_all)]
    fn edge(&self, i: &NfaIndex) -> &E {
        self.edges.get(i).unwrap()
    }

    #[tracing::instrument(skip_all)]
    /// E is the edge weight, usually NfaEdge
    fn add_edge(&mut self, edge: E, source: NfaIndex, target: NfaIndex) -> NfaIndex {
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
    #[tracing::instrument(skip_all)]
    fn edges_from(&self, i: NfaIndex) -> Option<&Vec<(NfaIndex, NfaIndex)>> {
        self.transitions.get(&i)
    }
}

impl<N, E: Eq + Clone> Nfa<N, NfaEdge<E>> {
    fn edge_by_kind<'a>(&self, i: NfaIndex, kind: &E) -> Vec<(NfaIndex, NfaIndex)> {
        self.transitions
            .get(&i)
            .unwrap_or(&vec![])
            .iter()
            .filter(|(_target_node_id, edge_id)| self.edge(edge_id).criteria == *kind)
            .cloned()
            .collect()
    }
}

impl<N, E> Nfa<N, E>
where
    N: std::fmt::Display,
    E: std::fmt::Display + Eq + Clone,
{
    pub(crate) fn graphviz(&self) -> String {
        let mut ret = "".to_string();
        for (source, edges) in &self.transitions {
            for (target, edge) in edges {
                ret = format!(
                    r#"{ret}
  {} -> {} [label="{}" fontsize="20pt"];"#,
                    nodename(source),
                    nodename(target),
                    self.edge(edge)
                );
            }
        }
        for (id, node) in &self.nodes {
            let nodelabel = if self.entry.contains(id) {
                "enter".to_string()
            } else {
                node.to_string()
            };
            ret = format!(
                r#"{ret}
  {} [label="{}"]"#,
                nodename(id),
                nodelabel
            );
        }
        ret
    }
}

fn nodename(i: &NfaIndex) -> String {
    format!("node_{i}")
}

pub(crate) fn graphviz_wrap(s: String, label: &str) -> String {
    format!(
        r#"
strict digraph G {{
    rankdir = TB;
    remincross = true;
    splines = true;
    fontsize="40";
    label = "{label}";
    {}
}}
"#,
        s
    )
}
