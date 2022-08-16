mod tests;

use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    Universal,
    Literal(L),
    Not(Box<Classifier<L>>),
    Any(BTreeSet<Classifier<L>>),
    And(BTreeSet<Classifier<L>>),
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug
        + std::hash::Hash
        + PartialOrd
        + Ord
        + PartialEq
        + Eq
        + Clone
        + Into<Nfa<NfaNode<()>, NfaEdge<Element>>>,
{
    #[tracing::instrument(skip_all)]
    fn compile(&self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        match self {
            Classifier::Universal => Nfa::universal(),
            Classifier::Literal(l) => l.clone().into(),
            Classifier::Not(c) => c.compile().negate(),
            Classifier::Any(v) => v
                .iter()
                .fold(Nfa::universal(), |acc, cur| acc.union(&cur.compile())),
            Classifier::And(v) => v.iter().fold(Nfa::universal(), |acc, cur| {
                acc.intersection(&cur.compile())
            }),
        }
    }
}

impl<L> Classifier<L>
where
    L: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::hash::Hash,
{
    #[tracing::instrument(skip_all)]
    fn relation(&self, other: &Self) -> Relation {
        // 1. compile
        // 2. relate NFAs
        todo!()
    }

    #[tracing::instrument(skip_all)]
    fn not(c: Classifier<L>) -> Self {
        Classifier::Not(Box::new(c))
    }
}

type NfaIndex = usize;

#[derive(Debug, Clone)]
pub struct Nfa<N, E> {
    count: usize,
    initial: BTreeSet<NfaIndex>,
    nodes: BTreeMap<NfaIndex, N>,
    edges: BTreeMap<NfaIndex, E>,
    /// source node index -> Vec<(target node index, edge index)>
    transitions: BTreeMap<NfaIndex, Vec<(NfaIndex, NfaIndex)>>,
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
    fn add_edge(&mut self, e: E, source: NfaIndex, target: NfaIndex) -> NfaIndex {
        let i = self.index();
        self.edges.insert(i, e);
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

    #[tracing::instrument(skip_all)]
    fn union(&self, other: &Self) -> Self {
        todo!()
    }

    #[tracing::instrument(skip_all)]
    fn intersection(&self, other: &Self) -> Self {
        todo!()
    }
}

impl<M: std::fmt::Debug + Clone, E: std::fmt::Debug + Clone> Nfa<NfaNode<M>, E> {
    #[tracing::instrument]
    fn negate(&self) -> Self {
        let mut nfa: Nfa<NfaNode<M>, E> = self.clone();
        nfa.nodes = nfa
            .nodes
            .into_iter()
            .map(|(i, n)| (i, n.negate()))
            .collect();
        nfa
    }
}

impl From<&str> for Nfa<NfaNode<()>, NfaEdge<Element>> {
    fn from(s: &str) -> Self {
        Nfa::from_str(s)
    }
}

impl Nfa<NfaNode<()>, NfaEdge<Element>> {
    #[tracing::instrument(skip_all)]
    fn from_str(s: &str) -> Self {
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

    #[tracing::instrument(skip_all)]
    fn universal() -> Self {
        let mut nfa: Self = Default::default();
        let prior = nfa.add_node(NfaNode {
            state: Terminal::Not,
        });
        let target = nfa.add_node(NfaNode {
            state: Terminal::Accept(()),
        });
        let _ = nfa.add_edge(
            NfaEdge {
                criteria: Element::Star,
            },
            prior,
            target,
        );
        nfa.initial.insert(prior);
        nfa
    }

    /// TODO: Returning early rather than collecting all terminal states within the closure of the
    /// input is incorrect. This should be changed to visit all possible nodes.
    #[tracing::instrument]
    fn accepts(&self, s: &str) -> bool {
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

#[derive(Debug, Clone)]
pub struct NfaNode<M: std::fmt::Debug + Clone> {
    state: Terminal<M>,
}

impl<M: std::fmt::Debug + Clone> NfaNode<M> {
    #[tracing::instrument(skip_all)]
    fn negate(&self) -> Self {
        let node = self.clone();
        node.state.negate();
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

#[derive(Debug, Clone)]
pub enum Element {
    Token(char),
    Question,
    Star,
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

impl From<char> for Element {
    fn from(c: char) -> Self {
        match c {
            c => Element::Token(c),
            // todo
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Relation {
    Disjoint,
    Intersection,
    Subset,
    Superset,
    Equality,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Terminal<M> {
    Not,
    Accept(M),
    Reject(M),
}

impl<M: std::fmt::Debug + Clone> Terminal<M> {
    #[tracing::instrument(skip_all)]
    fn negate(&self) -> Self {
        match self {
            Terminal::Not => Terminal::Not,
            Terminal::Accept(m) => Terminal::Reject(m.clone()),
            Terminal::Reject(m) => Terminal::Accept(m.clone()),
        }
    }
}
