use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NfaNode<M>
where
    M: std::fmt::Debug + Clone + Default,
{
    pub(crate) state: Terminal<M>,
    pub(crate) chirality: LRSemantics,
}

// TODO: Consider chirality
impl<M> NfaNode<M>
where
    M: std::fmt::Debug + Clone + Default,
{
    pub fn new(state: Terminal<M>) -> Self {
        Self {
            state,
            ..Default::default()
        }
    }

    // pub(crate) fn state_filter(&self, visitor: impl Fn(&Terminal<M>) -> bool) -> bool {
    //     visitor(&self.state)
    // }
}

impl<N> Default for NfaNode<N>
where
    N: Default + std::fmt::Debug + Clone,
{
    fn default() -> Self {
        Self {
            state: Default::default(),
            chirality: LRSemantics::None,
        }
    }
}

#[test]
fn test_nodesum() {
    crate::tests::setup();
    let c = Classifier::literal("P");
    let mut d1: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());
    d1.set_chirality(LRSemantics::L);

    let c = Classifier::literal("Q");
    let mut d2: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());
    d2.set_chirality(LRSemantics::R);

    let n1 = d1.node(1);
    let n2 = d2.node(1);
    let n3 = n1.sum(n2);
    assert_eq!(n3.chirality, LRSemantics::None);
}

impl<M> NodeSum for NfaNode<M>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + Default,
{
    #[tracing::instrument(skip(self, other), ret)]
    fn sum(&self, other: &Self) -> Self {
        NfaNode {
            state: self.state.sum(&other.state),
            chirality: self.chirality.sum(&other.chirality),
        }
    }
    #[tracing::instrument(skip(self, other))]
    fn sum_mut(&mut self, other: &Self) {
        self.state = self.state.sum(&other.state);
        self.chirality = self.chirality.sum(&other.chirality);
    }
}

impl<M> std::fmt::Display for NfaNode<M>
where
    M: std::fmt::Debug + Clone + Default,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?} {:?}", &self.state, &self.chirality))
    }
}

impl<M> NfaNode<M>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
{
    #[tracing::instrument(ret)]
    pub fn negate(&self) -> Self {
        println!("negate in NfaNode wtf");
        let mut node = self.clone();
        node.state = node.state.negate();
        node
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NfaEdge<E: Eq> {
    pub criteria: E,
}

impl<E> Default for NfaEdge<E>
where
    E: Default + std::cmp::Eq,
{
    fn default() -> Self {
        Self {
            criteria: Default::default(),
        }
    }
}

impl<L, E> Accepts<L> for NfaEdge<E>
where
    E: Accepts<L> + Eq + std::fmt::Debug,
    L: std::fmt::Debug,
{
    #[tracing::instrument(ret)]
    fn accepts(&self, l: &L) -> bool {
        self.criteria.accepts(l)
    }
}

impl<E> Universal for NfaEdge<E>
where
    E: Universal + PartialOrd + Ord,
{
    fn universal() -> Self {
        NfaEdge {
            criteria: E::universal(),
        }
    }
}

impl<E: std::fmt::Display + Eq> std::fmt::Display for NfaEdge<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.criteria.to_string())
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

    fn sum(&self, other: &Self) -> Self {
        match (self, other) {
            (Terminal::Not, Terminal::Not) => Terminal::Not,
            (Terminal::Not, Terminal::Accept(_)) => other.clone(),
            (Terminal::Not, Terminal::Reject(_)) => other.clone(),
            (Terminal::Accept(_), Terminal::Not) => self.clone(),
            (Terminal::Accept(_), Terminal::Accept(_)) => {
                // TODO: Worry about merging Ms
                self.clone()
            }
            (Terminal::Accept(_), Terminal::Reject(_)) => other.clone(),
            (Terminal::Reject(_), Terminal::Not) => self.clone(),
            (Terminal::Reject(_), Terminal::Accept(_)) => self.clone(),
            (Terminal::Reject(_), Terminal::Reject(_)) => {
                // TODO: worry about merging Ms
                self.clone()
            }
        }
    }
}
