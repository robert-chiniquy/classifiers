use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

pub type NfaIndex = usize;

#[derive(Debug, Clone)]
pub struct Nfa<N, E> {
    count: usize,
    // Convenience for now, later switch to only a single initial node
    initial: BTreeSet<NfaIndex>,
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
    M: std::fmt::Debug + Clone,
    E: std::fmt::Debug + Clone,
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
    E: std::fmt::Debug + Clone + Universal,
{
    #[tracing::instrument(skip_all)]
    pub fn universal() -> Self {
        let mut nfa: Self = Default::default();
        let prior = nfa.add_node(NfaNode {
            state: Terminal::Not,
        });
        let target = nfa.add_node(NfaNode {
            state: Terminal::Accept(()),
        });
        let _ = nfa.add_edge(
            NfaEdge {
                criteria: E::universal(),
            },
            prior,
            target,
        );
        nfa.initial.insert(prior);
        nfa
    }
}

impl Nfa<NfaNode<()>, NfaEdge<Element>> {
    #[tracing::instrument(skip_all)]
    pub fn from_str(s: &str) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode {
            state: Terminal::Not,
        });
        nfa.initial.insert(prior);
        for c in s.chars() {
            let target = nfa.add_node(NfaNode {
                state: Terminal::Not,
            });
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(());
        nfa
    }

    /// TODO: Returning early rather than collecting all terminal states within the closure of the
    /// input is incorrect. This should be changed to visit all possible nodes.
    #[tracing::instrument]
    pub fn accepts(&self, s: &str) -> bool {
        for i in &self.initial {
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
                    None => match self.node(i).state {
                        Terminal::Not => (),
                        Terminal::Accept(_) => return true,
                        Terminal::Reject(_) => return false,
                    },
                }
            }
        }
        false
    }
}

impl<N, E> Nfa<N, E>
where
    N: std::fmt::Debug + Clone,
    E: std::fmt::Debug + Clone,
{
    /// An intersection NFA only has accepting states where both input NFAs have accepting states
    #[tracing::instrument(skip_all)]
    pub fn intersection(&self, other: &Self) -> Self {
        // this requires an NFA product
        todo!()
    }

    /// A union is a non-mimimal NFA with the same resulting states for every input as
    /// either of the two input NFAs.
    /// For now can just merge structs adjusting indices for uniqueness,
    // TODO: combine trees starting from self's initial nodes
    #[tracing::instrument(skip_all)]
    pub fn union(&self, other: &Self) -> Self {
        let mut union: Nfa<N, E> = self.clone();
        union.count += other.count;
        union.count += 1;
        union
            .initial
            .extend(other.initial.iter().map(|i| i + self.count));
        union.nodes.extend(
            other
                .nodes
                .iter()
                .map(|(i, n)| ((i + self.count), n.clone())),
        );
        union.edges.extend(
            other
                .edges
                .iter()
                .map(|(i, e)| ((i + self.count), e.clone())),
        );
        union
            .transitions
            .extend(other.transitions.iter().map(|(i, v)| {
                (
                    (i + self.count),
                    v.iter()
                        .map(|(t, e)| ((t + self.count), (e + self.count)))
                        .collect(),
                )
            }));
        union
    }
}

#[derive(Debug, Clone)]
pub struct NfaNode<M: std::fmt::Debug + Clone> {
    state: Terminal<M>,
}

impl<N: std::fmt::Debug + Clone> std::fmt::Display for NfaNode<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.state))
    }
}

impl<M: std::fmt::Debug + Clone> NfaNode<M> {
    #[tracing::instrument(ret)]
    fn negate(&self) -> Self {
        let mut node = self.clone();
        node.state = node.state.negate();
        node
    }
}

#[derive(Debug, Clone)]
pub struct NfaEdge<E> {
    criteria: E,
}

pub trait Accepts<L> {
    fn accepts(&self, l: L) -> bool;
}

impl<L, E> Accepts<L> for NfaEdge<E>
where
    E: Accepts<L>,
{
    #[tracing::instrument(skip_all)]
    fn accepts(&self, l: L) -> bool {
        self.criteria.accepts(l)
    }
}

impl<E: std::fmt::Display> std::fmt::Display for NfaEdge<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.criteria.to_string())
    }
}

#[derive(Debug, Clone)]
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

impl<N, E> Default for Nfa<N, E> {
    fn default() -> Self {
        Self {
            count: Default::default(),
            initial: Default::default(),
            nodes: Default::default(),
            edges: Default::default(),
            transitions: Default::default(),
        }
    }
}

impl<N, E> Nfa<N, E> {
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

    /// node index -> (target index, edge index)
    #[inline]
    #[tracing::instrument(skip_all)]
    fn edges_from(&self, i: NfaIndex) -> Option<&Vec<(NfaIndex, NfaIndex)>> {
        self.transitions.get(&i)
    }
}

impl<N, E> Nfa<N, E>
where
    N: std::fmt::Display,
    E: std::fmt::Display,
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
            let nodelabel = if self.initial.contains(id) {
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
