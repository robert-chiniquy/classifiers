mod builder;

use builder::*;

use std::{collections::HashMap, ops::Add};

use super::*;

const ASCII_TOTAL_CHARS: usize = 128;

#[test]
fn test_equility() {
    let a = Element::not_tokens(&['a', 'b', 'c']);
    let b = Element::not_tokens(&['a', 'b', 'c']);
    assert_eq!(a, b);
}

#[derive(Debug, Clone, Eq, PartialEq, std::hash::Hash, PartialOrd, Ord)]
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

#[test]
fn test_element_from_language() {
    let _ = Element::from_language("a*b".to_string().chars().collect(), ());
    let _ = Element::from_language("a*b*c".to_string().chars().collect(), ());
    let _ = Element::from_language("a**b**c".to_string().chars().collect(), ());
}

impl FromLanguage<Element> for Element {
    type Language = Vec<char>;

    // FIXME
    type Metadata = ();

    fn from_language(
        l: Self::Language,
        _m: Self::Metadata,
    ) -> Nfa<NfaNode<Self::Metadata>, NfaEdge<Element>> {
        let mut builder: DfaBuilder = DfaBuilder::from_language(l);
        let stack = builder.find_compound_ids();
        builder.complete_transitions(stack);
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

    pub fn simplify(&mut self) {
        for (s, targets) in &self.transitions.clone() {
            if targets.len() <= 1 {
                continue;
            }
            // let target_map = targets.iter().cloned().collect::<HashMap<u32, Vec<u64>>>();
            let mut collected_targets: HashMap<_, Vec<_>> = HashMap::new();
            for (t, e) in targets {
                collected_targets
                    .entry(t)
                    .and_modify(|tt| tt.push(e))
                    .or_insert_with(|| vec![e]);
            }
            let mut positives = BTreeSet::new();
            let mut negatives = BTreeSet::new();
            for (target, ees) in collected_targets {
                if ees.len() <= 1 {
                    continue;
                }
                for e in &ees {
                    match self.edge(e).map(|edge| &edge.criteria) {
                        Some(Element::TokenSet(ref s)) => {
                            positives = &positives | s;
                        }
                        Some(Element::NotTokenSet(ref s)) => {
                            negatives = &negatives | s;
                        }
                        None => unreachable!(),
                    }
                }
                // println!("positives {positives:?} negatives: {negatives:?}");
                // abd !a!b!c  -> d, !c
                let overlapping = &negatives - &positives;
                negatives = overlapping.clone();
                positives = &positives - &overlapping;
                // println!("modified: positives {positives:?} negatives: {negatives:?}");
                for e in ees {
                    self.remove_edge(*s, *e);
                }

                if !negatives.is_empty() {
                    self.add_edge(
                        NfaEdge {
                            criteria: Element::NotTokenSet(negatives.clone()),
                        },
                        *s,
                        *target,
                    );
                } else if !positives.is_empty() {
                    self.add_edge(
                        NfaEdge {
                            criteria: Element::TokenSet(positives.clone()),
                        },
                        *s,
                        *target,
                    );
                } else {
                    self.add_edge(
                        NfaEdge {
                            criteria: Element::NotTokenSet(negatives.clone()),
                        },
                        *s,
                        *target,
                    );
                }
            }
        }
        self.shake();
    }
}

// impl Element {
//     fn simplify(&self) -> Option<Element> {
//         use Element::*;
//         match &self {
//             TokenSet(x) => {
//                 if x.is_empty() {
//                     None
//                 } else {
//                     Some(self.clone())
//                 }
//             }
//             NotTokenSet(x) => {
//                 if x.is_empty() {
//                     None
//                 } else {
//                     // TODO: see if we get rid of all ascii chars??
//                     Some(self.clone())
//                 }
//             }
//         }
//     }
// }
