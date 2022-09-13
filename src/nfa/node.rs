use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NfaNode<M>
where
    M: std::fmt::Debug + Clone + Default,
{
    pub(crate) state: Terminal<M>,
    pub(crate) chirality: LRSemantics,
}

impl<M> Accepting for NfaNode<M> 
where
    M: std::fmt::Debug + Clone + Default,
{
    fn accepting(&self) -> bool {
        match self.state {
            Terminal::None => false,
            Terminal::Include(_) => true,
            Terminal::Exclude(_) => true,
        }
    }
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
        if self.chirality != LRSemantics::None {
            f.write_fmt(format_args!("{:?} {}", &self.state, &self.chirality))    
        } else {
            f.write_fmt(format_args!("{:?}", &self.state))
        }
        
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
    // #[tracing::instrument(ret)]
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

// Define complementation ... 
// a -> b -> c(accept)
// complement:
// a(accept) -> b(accept) -> c
// do non-accepting nodes contain an ption::None?
// for homogenous trees with only 1 accepting state or multiple identical accepting states, this is fine
// we need to also define complementation for trees with heterogenous accepting states
// matt suggests the default state is like ... InverseInclude(M), so when complementation occurs,
// you get the right match ... however ... 
// so then, any operation which transforms a tree by changing the accepting Include(M) 
// states to Exclude(M), (as in Classifier::Not) must also change the InverseInclude(M) to InverseExclude(M)
#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Terminal<M> {
    // rename: None? Use an option?
    NonAcceptingInclude(M),
    NonAcceptingExclude(M),
    Include(M),
    Exclude(M),
}

impl <M> Terminal<M> {
    pub fn invert(&self) -> Self {
        match self {
            Terminal::NonAcceptingInclude(m) =>Terminal::Include(*m),
            Terminal::NonAcceptingExclude(m) => Terminal::Exclude(*m),
            Terminal::Include(m) => Terminal::NonAcceptingInclude(*m),
            Terminal::Exclude(m) => Terminal::Exclude(*m),
        }
    }
}

impl<M> Default for Terminal<M> {
    fn default() -> Self {
        Terminal::None
    }
}

impl<M> std::fmt::Debug for Terminal<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, ""),
            Self::Include(_) => f.debug_tuple("Accept").finish(),
            Self::Exclude(_) => f.debug_tuple("Reject").finish(),
        }
    }
}

impl<M: std::fmt::Debug + Clone> Terminal<M> {
    #[tracing::instrument(ret)]
    fn negate(&self) -> Self {
        match self {
            Terminal::None => Terminal::None,
            Terminal::Include(m) => Terminal::Exclude(m.clone()),
            Terminal::Exclude(m) => Terminal::Include(m.clone()),
        }
    }

    fn sum(&self, other: &Self) -> Self {
        match (self, other) {
            (Terminal::None, Terminal::None) => Terminal::None,
            (Terminal::None, Terminal::Include(_)) => other.clone(),
            (Terminal::None, Terminal::Exclude(_)) => other.clone(),
            (Terminal::Include(_), Terminal::None) => self.clone(),
            (Terminal::Include(_), Terminal::Include(_)) => {
                // TODO: Worry about merging Ms
                self.clone()
            }
            (Terminal::Include(_), Terminal::Exclude(_)) => other.clone(),
            (Terminal::Exclude(_), Terminal::None) => self.clone(),
            (Terminal::Exclude(_), Terminal::Include(_)) => self.clone(),
            (Terminal::Exclude(_), Terminal::Exclude(_)) => {
                // TODO: worry about merging Ms
                self.clone()
            }
        }
    }
}
