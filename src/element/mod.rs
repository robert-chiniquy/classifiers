use std::collections::hash_map::Entry::*;
use std::collections::{BTreeMap, HashSet};
use std::{collections::HashMap, ops::Add};

use super::*;

const ASCII_TOTAL_CHARS: usize = 128;

#[test]
fn test_equility() {
    let a = Element::not_tokens(&['a', 'b', 'c']);
    let b = Element::not_tokens(&['a', 'b', 'c']);
    assert_eq!(a, b);
}

#[derive(Debug, Clone, Eq, PartialEq, std::hash::Hash)]
pub enum Element {
    TokenSet(BTreeSet<char>),
    NotTokenSet(BTreeSet<char>),
}

impl Element {
    pub fn token(c: char) -> Element {
        Element::TokenSet(FromIterator::from_iter(vec![c]))
    }

    pub fn not_token(c: char) -> Element {
        Element::NotTokenSet(FromIterator::from_iter(vec![c]))
    }

    pub fn tokens(v: &[char]) -> Element {
        Element::TokenSet(FromIterator::from_iter(v.iter().cloned()))
    }

    pub fn not_tokens(v: &[char]) -> Element {
        Element::NotTokenSet(FromIterator::from_iter(v.iter().cloned()))
    }
}

impl Complement<Element> for Element {
    fn complement(&self) -> Option<Self> {
        use Element::*;
        match self {
            TokenSet(n) => Some(NotTokenSet(n.clone())),
            NotTokenSet(n) => Some(TokenSet(n.clone())),
        }
    }
}

impl ElementalLanguage<Element> for Element {}

#[derive(Default, Debug)]
struct DfaBuilder {
    count: u32,
    // {2,3} -> 23
    states: HashMap<BTreeSet<NodeId>, NodeId>,
    // (2, b) -> {2,3}
    transitions: HashMap<NodeId, BTreeMap<Element, BTreeSet<NodeId>>>,
}

impl DfaBuilder {
    fn non_optimal_accepting_transitions(
        &self,
        from: NodeId,
        criteria: &Element,
    ) -> BTreeSet<NodeId> {
        todo!()
    }

    pub fn next_id(&mut self) -> NodeId {
        let r = self.count;
        self.count += 1;
        r
    }

    pub fn add_transition(&mut self, from: NodeId, e: Element, to: NodeId) {
        // surplus but not incorrect
        self.states
            .insert(FromIterator::from_iter(vec![from]), from);
        self.states.insert(FromIterator::from_iter(vec![to]), to);

        let entry = match self.transitions.entry((from, e)) {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(Default::default()),
        };
        entry.insert(to);
    }

    // abc
    // number of rows/nodes = number of chars in input + 1
    // number of columns: number of symbols in input
    //   a b c
    // 1 2
    // 2   3
    // 3     4
    // 4
    // a*b
    // the presence of * implies the complemement of all symbols
    // 2,3 is added to the table as there is a transition that goes to 2 and 3 ... ?
    //       | a   | b     | !a!b |
    // 1     | 2   | Ø     | Ø    |
    // 2     | 2,3 | 2,3   | 2,3  |
    // 3     | Ø   | 4     | Ø    |
    // 4     | Ø   | Ø     | Ø    |
    // 2,3   | 2,3 | 2,3,4 | 2,3  |
    // 2,3,4 | 2,3 | 2,3   | 2,3  |
    // where-ever you have columns such as b and ab,
    // a transition of ab from a node counts as a transition of b as well
    // for the purpose of unioning the resulting transitions

    // TODO: M
    pub fn build(&mut self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        self.construct();
        self.build_dfa()
    }

    fn build_dfa(&self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let mut dfa: Nfa<NfaNode<()>, NfaEdge<Element>> = Default::default();
        // builder NodeId -> dfa NodeId
        let mut node_id_map: HashMap<NodeId, NodeId> = Default::default();
        // add a node for each row
        let nodes: HashSet<_> = self.states.values().collect();
        // let rows: HashSet<_> = self.transitions.keys().map(|(k, _)| k).collect();
        // let mut dfa_node_id = 0;
        for node_id in nodes {
            let dfa_node_id = dfa.add_node(Default::default());
            node_id_map.insert(*node_id, dfa_node_id);
        }
        println!(
            "{node_id_map:?}\n{:?}\n{:?}",
            self.transitions.keys().collect::<Vec<_>>(),
            self.states
        );
        // add edges after nodes exist
        for ((builder_node_id, criteria), target) in &self.transitions {
            let t = self.states.get(target).unwrap();
            dfa.add_edge(
                NfaEdge {
                    criteria: criteria.clone(),
                },
                *node_id_map.get(builder_node_id).unwrap(),
                *node_id_map.get(t).unwrap(),
            );
        }

        // TODO
        // filter the dfa to only nodes which are reachable by root
        // after this step, from this specific construction, any reachable leaf node is accepting
        // .. add missing transitions here? nope
        dfa.graphviz_file("dfa.dot", "dfa");
        dfa
    }

    fn construct(&mut self) {
        let columns: HashSet<_> = self
            .transitions
            .keys()
            .map(|(_, col)| col)
            .cloned()
            .collect();
        // start with all compound values
        let mut stack: Vec<_> = self
            .transitions
            .values()
            .filter(|v| v.len() > 1)
            .cloned()
            .collect();
        while let Some(compound_state) = stack.pop() {
            // don't process the same compound state twice
            if self.states.contains_key(&compound_state) {
                continue;
            }
            // create a new state (index it in self.states) and push it on the stack
            let compound_id = self.next_id();
            self.states.insert(compound_state.clone(), compound_id);
            // for every compound state on the stack,
            // for every column,
            // populate its value by unioning the transitions of its component states for that column
            // where this entails creating a new compound state, push it on the stack
            for col in &columns {
                let mut unioned_transitions: BTreeSet<NodeId> = Default::default();
                for component_state in &compound_state {
                    // get the transitions of component states for this column
                    if let Some(transitions) =
                        self.transitions.get(&(*component_state, col.clone()))
                    {
                        unioned_transitions.extend(transitions);
                    }
                    // for every column, for every column which would accept that column,
                    // also add the resulting transition from the accepting column to unioned_transitions
                }
                if unioned_transitions.is_empty() {
                    continue;
                }
                self.transitions
                    .insert((compound_id, col.clone()), unioned_transitions.clone());
                // if unioned_transitions is already processed, this is caught at the beginning of the loop
                // if it is length 1, then it is already in self.states
                stack.push(unioned_transitions);
            }
        }
    }
}

#[test]
fn test_element_from_language() {
    let d = Element::from_language("a*b".to_string().chars().collect(), ());
}

impl FromLanguage<Element> for Element {
    type Language = Vec<char>;

    // FIXME
    type Metadata = ();

    fn from_language(
        l: Self::Language,
        m: Self::Metadata,
    ) -> Nfa<NfaNode<Self::Metadata>, NfaEdge<Element>> {
        let symbols: BTreeSet<_> = l
            .iter()
            .filter(|c| **c != '?' && **c != '*')
            .cloned()
            .collect();
        let mut builder: DfaBuilder = Default::default();
        let mut prior = builder.next_id();
        for c in &l {
            let current = builder.next_id();
            match c {
                '?' => {
                    // transition via all symbols from prior to current
                    builder.add_transition(prior, Element::TokenSet(symbols.clone()), current);
                    builder.add_transition(prior, Element::NotTokenSet(symbols.clone()), current);
                }
                '*' => {
                    // transition via all symbols from prior to current
                    // also have a self-loop (2 actually)
                    builder.add_transition(prior, Element::TokenSet(symbols.clone()), current);
                    builder.add_transition(prior, Element::NotTokenSet(symbols.clone()), current);
                    builder.add_transition(prior, Element::TokenSet(symbols.clone()), prior);
                    builder.add_transition(prior, Element::NotTokenSet(symbols.clone()), prior);
                }
                c => {
                    // transition from prior to current via c
                    builder.add_transition(prior, c.into(), current);
                }
            }
            prior = current;
        }
        builder.build()
    }
}

impl Universal for Element {
    fn universal() -> Self {
        Element::NotTokenSet(Default::default())
    }
}

impl From<char> for Element {
    fn from(c: char) -> Self {
        Element::token(c)
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        Element::token(*c)
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: &Element) -> bool {
        use Element::*;
        match (self, &l) {
            (x, y) if x == *y => true,
            // Are all other ASCII characters specified in the not token set?
            (TokenSet(x), NotTokenSet(y)) => {
                x.len() + y.len() == ASCII_TOTAL_CHARS && x.is_disjoint(y)
            }
            (NotTokenSet(x), TokenSet(y)) => x.is_disjoint(y),
            (_, _) => false,
        }
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &&char) -> bool {
        match self {
            Element::NotTokenSet(v) => !v.iter().any(|c| *c == **l),
            Element::TokenSet(v) => v.iter().any(|c| *c == **l),
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        self.accepts(&l)
    }
}

// TODO: Remove the dep on Default for this stuff
impl Default for Element {
    fn default() -> Self {
        Self::universal()
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Element::NotTokenSet(v) => {
                    if v.len() == 1 {
                        format!("!{}", v.iter().next().unwrap())
                    } else {
                        format!("!`{v:?}`")
                    }
                }
                Element::TokenSet(v) => {
                    if v.len() == 1 {
                        format!("{}", v.iter().next().unwrap())
                    } else {
                        format!("`{v:?}`")
                    }
                }
            }
        ))
    }
}

impl Product<Element> for Element {
    // #[tracing::instrument(ret)]
    fn product(a: &Self, b: &Self) -> Vec<NfaBranch<Element>> {
        use EdgeTransition::*;
        use Element::*;

        let branches: Vec<NfaBranch<Element>> = match (a, b) {
            (TokenSet(x), TokenSet(y)) => {
                let matching = x
                    .iter()
                    .filter(|c| y.contains(c))
                    .cloned()
                    .collect::<BTreeSet<_>>();

                let left = x
                    .iter()
                    .filter(|c| !matching.contains(c))
                    .cloned()
                    .collect();

                let right = y
                    .iter()
                    .filter(|c| !matching.contains(c))
                    .cloned()
                    .collect();

                vec![
                    NfaBranch::new(TokenSet(left), Advance, Stop),
                    NfaBranch::new(TokenSet(matching), Advance, Advance),
                    NfaBranch::new(TokenSet(right), Stop, Advance),
                ]
            }

            (NotTokenSet(x), NotTokenSet(y)) => {
                // [!a,!b] X [!a,!c] -> [c], [!a,!b,!c], [b]
                let left = y.iter().filter(|c| !x.contains(c)).cloned().collect();
                let center = x.clone().union(y).cloned().collect::<BTreeSet<char>>();
                let right = x.iter().filter(|c| !y.contains(c)).cloned().collect();

                vec![
                    NfaBranch::new(TokenSet(left), Advance, Stop),
                    NfaBranch::new(NotTokenSet(center), Advance, Advance),
                    NfaBranch::new(TokenSet(right), Stop, Advance),
                ]
            }

            (TokenSet(x), NotTokenSet(y)) => {
                // [a,b] X [!a,!c] = [a] [b] [!a,!b,!c]
                //  left is matching
                //  things in left not in right
                //  right is dedup sum

                let left = y.iter().filter(|c| x.contains(c)).cloned().collect();
                let center = x.iter().filter(|c| !y.contains(c)).cloned().collect();
                let right = y.clone().union(x).cloned().collect();

                vec![
                    NfaBranch::new(TokenSet(left), Advance, Stop),
                    NfaBranch::new(TokenSet(center), Advance, Advance),
                    NfaBranch::new(NotTokenSet(right), Stop, Advance),
                ]
            }
            (NotTokenSet(x), TokenSet(y)) => {
                let left = x.clone().union(y).cloned().collect();
                let center = y.iter().filter(|c| !x.contains(c)).cloned().collect();
                let right = x.iter().filter(|c| y.contains(c)).cloned().collect();

                vec![
                    NfaBranch::new(NotTokenSet(left), Advance, Stop),
                    NfaBranch::new(TokenSet(center), Advance, Advance),
                    NfaBranch::new(TokenSet(right), Stop, Advance),
                ]
            }
        };
        branches
            .into_iter()
            .filter(|b| match &b.kind {
                TokenSet(v) => !v.is_empty(),
                NotTokenSet(v) => !v.is_empty(),
            })
            .collect()
    }
}

impl Add for Element {
    type Output = Element;

    fn add(self, rhs: Self) -> Self::Output {
        use Element::*;

        match (&self, &rhs) {
            (TokenSet(x), TokenSet(y)) => TokenSet(x | y),
            (NotTokenSet(y), TokenSet(x)) | (TokenSet(x), NotTokenSet(y)) => {
                let z = y - x;
                if z.is_empty() {
                    Self::universal()
                } else {
                    NotTokenSet(z)
                }
            }
            (NotTokenSet(x), NotTokenSet(y)) => {
                // negation flips the semantics of the set relations of the btreesets
                // !a + !a!b = !a
                // !a!b + !a!b!c = !a!b
                if x.is_subset(y) {
                    self.clone()
                } else if y.is_subset(x) {
                    rhs.clone()
                } else {
                    Self::universal()
                }
            }
        }
    }
}

impl Subtraction<Element> for Element {
    fn difference(a: &Element, b: &Element) -> Element {
        use Element::*;

        match (a, b) {
            (TokenSet(x), TokenSet(y)) => {
                // ab - bc = a
                TokenSet(x - y)
            }
            (TokenSet(x), NotTokenSet(y)) => {
                // remove from x any value which is not in y
                TokenSet(x & y)
                // let mut x = x.clone();
                // x.retain(|c| y.contains(c));
                // TokenSet(x).simplify()
            }
            (NotTokenSet(x), TokenSet(y)) => {
                // !a!b abc
                // !a!b!c
                NotTokenSet(x | y)
            }

            (NotTokenSet(x), NotTokenSet(y)) => {
                // things on right not on the left
                // let everything = a,b,c
                // !a = b,c
                // !c = a,b
                // !a - !c = b,c - a,b = c
                // !a + !c = b,c + a,b = a,b,c
                // !a + !c = ? - !c =

                //  !a - !c = c (b,c,d... - a,b,d.. = c)
                TokenSet(y - x)
                // let mut x = x.clone();
                // x.retain(|c| !y.contains(c));
                // NotTokenSet(x).simplify()
            }
        }
    }
}

#[test]
fn test_arithmetic() {
    // use Element::*;
    let nt = Element::not_token;
    let nts = Element::not_tokens;
    let ts = Element::tokens;
    let t = Element::token;

    // !c - a - b = !a!b!c
    let r = Element::difference(&nt('c'), &t('a'));
    let r: Element = Element::difference(&r, &t('b'));
    assert_eq!(r, Element::not_tokens(&['a', 'b', 'c']));

    // * - a - b = !a!b
    let mut r = Element::universal();
    r = Element::difference(&r, &t('a'));
    r = Element::difference(&r, &t('b'));
    assert_eq!(r, nts(&['a', 'b']));

    // ? - a - b = !a!b
    let r = Element::difference(&Element::universal(), &t('a'));
    let r = Element::difference(&r, &t('b'));
    assert_eq!(r, nts(&['a', 'b']));

    // ab - a -b = None
    let r = Element::difference(&ts(&['a', 'b']), &t('a'));
    assert_eq!(ts(&[]), Element::difference(&r, &t('b')));

    // !a - !c = c
    assert_eq!(Element::difference(&nt('a'), &nt('c')), t('c'));
}

impl<M> Nfa<NfaNode<M>, NfaEdge<Element>>
where
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    pub fn accepts_string(&self, s: &str) -> bool {
        self.accepts(&str_to_chars(s)).unwrap()
    }

    #[tracing::instrument(skip_all)]
    pub fn from_str(s: &str, m: M) -> Self {
        let mut nfa: Self = Default::default();
        let mut prior = nfa.add_node(NfaNode::new(Terminal::Not));
        nfa.entry = prior;
        for c in s.chars() {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
        nfa
    }
}

impl Element {
    fn simplify(&self) -> Option<Element> {
        use Element::*;
        match &self {
            TokenSet(x) => {
                if x.is_empty() {
                    None
                } else {
                    Some(self.clone())
                }
            }
            NotTokenSet(x) => {
                if x.is_empty() {
                    None
                } else {
                    // TODO: see if we get rid of all ascii chars??
                    Some(self.clone())
                }
            }
        }
    }
}
