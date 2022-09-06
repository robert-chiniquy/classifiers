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
    Question,
    Star,
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

#[test]
fn test_disjoint() {
    // assert!(Element::are_disjoint(vec![
    //     Element::tokens(&['a', 'b']),
    //     Element::tokens(&['c', 'd']),
    // ]));

    // assert!(!Element::are_disjoint(vec![
    //     Element::tokens(&['a', 'b']),
    //     Element::tokens(&['a', 'c']),
    // ]));

    // assert!(Element::are_disjoint(vec![
    //     Element::not_tokens(&['a', 'b']),
    //     Element::tokens(&['a', 'b']),
    // ]));

    // assert!(!Element::are_disjoint(vec![
    //     Element::not_tokens(&['a', 'b']),
    //     Element::tokens(&['a', 'b']),
    //     Element::not_tokens(&['c']),
    // ]));

    // let all_ascii = (0_u8..128)
    //     .collect::<Vec<u8>>()
    //     .into_iter()
    //     .map(|n| n as char)
    //     .collect::<Vec<char>>();

    // let v = all_ascii.clone()[0..127]
    //     .iter()
    //     .cloned()
    //     .collect::<Vec<char>>();
    // let c = 127_u8 as char;

    // assert!(Element::are_disjoint(vec![
    //     Element::not_tokens(&v),
    //     Element::not_tokens(&[c]),
    // ]));

    assert!(Element::are_disjoint(vec![
        Element::token('a'),
        Element::not_tokens(&['a', 'b']),
    ]));

    assert!(!Element::are_disjoint(vec![
        Element::token('a'),
        Element::not_tokens(&['b']),
    ]));
}

impl Disjointsome<Element> for Element {
    fn are_disjoint(v: Vec<Element>) -> bool {
        use Element::*;
        for i in 0..v.len() - 1 {
            let e1 = &v[i];
            for e2 in v[i + 1..].iter() {
                match (e1, e2) {
                    (_, Star) | (_, Question) | (Question, _) | (Star, _) => {
                        return false;
                    }
                    (TokenSet(x), TokenSet(y)) => {
                        if !x.is_disjoint(y) {
                            return false;
                        }
                    }
                    // ab vs !c!d -> no
                    // ab vs !a -> no
                    // a vs !a  -> yes
                    // a vs !a!b -> yes
                    // a vs !b -> yes
                    (TokenSet(x), NotTokenSet(y)) | (NotTokenSet(y), TokenSet(x)) => {
                        // all of left must be in right
                        // println!("{:?} {:?} {}", x, y, !y.is_superset(&x));
                        if !y.is_superset(x) {
                            return false;
                        }
                    }
                    // !a vs !b -> intersect
                    // !a vs !a -> intersect
                    // !a,,!c,!d...!z vs  !b
                    (NotTokenSet(x), NotTokenSet(y)) => {
                        if x.len() + y.len() != ASCII_TOTAL_CHARS || !x.is_disjoint(y) {
                            return false;
                        }
                    }
                };
            }
        }
        true
    }
}

impl Complement<Element> for Element {
    fn complement(&self) -> Option<Self> {
        use Element::*;
        match self {
            Question => None,
            Star => None,
            TokenSet(n) => Some(NotTokenSet(n.clone())),
            NotTokenSet(n) => Some(TokenSet(n.clone())),
        }
    }
}

impl ElementalLanguage<Element> for Element {}

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
            c => Element::token(c),
        }
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        match c {
            '*' => Element::Star,
            '?' => Element::Question,
            c => Element::token(*c),
        }
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: &Element) -> bool {
        use Element::*;
        match (self, &l) {
            (x, y) if x == *y => true,
            (Star, _) => true,
            (_, Star) => false,
            (Question, _) => true,
            (_, Question) => false,
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
            Element::Question => true,
            Element::Star => true,
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
        Element::Star
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Element::Question => "?".to_string(),
                Element::Star => "*".to_string(),
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
            (Star, Star) => vec![
                NfaBranch::new(Star, Advance, Stay),
                NfaBranch::new(Star, Stay, Advance),
                NfaBranch::new(Star, Advance, Advance),
            ],
            (Star, Question) | (Star, TokenSet(_)) | (Star, NotTokenSet(_)) => {
                let mut v = vec![
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                ];
                let c = b.clone().complement();
                if let Some(c) = c {
                    v.push(NfaBranch::new(c, Advance, Stop));
                }
                v
            }

            (Question, Star) | (TokenSet(_), Star) | (NotTokenSet(_), Star) => {
                let mut v = vec![
                    NfaBranch::new(a.clone(), Advance, Stay),
                    NfaBranch::new(a.clone(), Advance, Advance),
                ];
                let c = a.clone().complement();
                if let Some(c) = c {
                    v.push(NfaBranch::new(c, Stop, Advance));
                }
                v
            }

            /* Question */
            (Question, Question) | (Question, TokenSet(_)) | (Question, NotTokenSet(_)) => {
                let mut v = vec![NfaBranch::new(b.clone(), Advance, Advance)];
                let c = b.clone().complement();
                if let Some(c) = c {
                    v.push(NfaBranch::new(c, Advance, Stop));
                }
                v
            }

            (TokenSet(_), Question) | (NotTokenSet(_), Question) => vec![
                NfaBranch::new(a.clone(), Advance, Advance),
                NfaBranch::new(a.clone().complement().unwrap(), Stop, Advance),
            ],

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
                Question => true,
                Star => true,
                TokenSet(v) => !v.is_empty(),
                NotTokenSet(v) => !v.is_empty(),
            })
            .collect()
    }
}

#[test]
fn asdfasdf() {
    use Element::*;
    let nt = Element::not_token;
    let nts = Element::not_tokens;
    let ts = Element::tokens;
    let t = Element::token;

    let mut r = nt('c');
    r = Element::difference(&r, &t('a')).unwrap();
    r = Element::difference(&r, &t('b')).unwrap();
    assert_eq!(r, Element::not_tokens(&['a', 'b', 'c']));

    let mut r = Star;
    r = Element::difference(&r, &t('a')).unwrap();
    r = Element::difference(&r, &t('b')).unwrap();
    assert_eq!(r, nts(&['a', 'b']));

    let mut r = Question;
    r = Element::difference(&r, &t('a')).unwrap();
    r = Element::difference(&r, &t('b')).unwrap();
    assert_eq!(r, nts(&['a', 'b']));

    let mut r = ts(&['a', 'b']);
    r = Element::difference(&r, &t('a')).unwrap();
    assert_eq!(None, Element::difference(&r, &t('b')));
}

impl Element {
    fn simplify(&self) -> Option<Element> {
        use Element::*;
        match &self {
            Question | Star => Some(self.clone()),
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

impl Subtraction<Element> for Element {
    fn difference(a: &Element, b: &Element) -> Option<Element> {
        use Element::*;

        match (a, b) {
            (TokenSet(_), Question)
            | (TokenSet(_), Star)
            | (NotTokenSet(_), Question)
            | (NotTokenSet(_), Star) => None,

            (Question, Question) => None,
            (Question, Star) => None,
            (Star, Question) => None,
            (Star, Star) => None,

            (Star, NotTokenSet(v)) | (Question, NotTokenSet(v)) => Some(TokenSet(v.clone())),
            (Star, TokenSet(y)) | (Question, TokenSet(y)) => Some(NotTokenSet(y.clone())),

            (TokenSet(x), TokenSet(y)) => {
                // ab - bc = a
                let mut x = x.clone();
                x.retain(|c| !y.contains(c));
                TokenSet(x).simplify()
            }
            (TokenSet(x), NotTokenSet(y)) => {
                let mut x = x.clone();
                x.retain(|c| y.contains(c));
                TokenSet(x).simplify()
            }
            (NotTokenSet(x), TokenSet(y)) => {
                // !a!b abc
                NotTokenSet(x.clone().union(y).cloned().collect()).simplify()
            }

            (NotTokenSet(x), NotTokenSet(y)) => {
                let mut x = x.clone();
                x.retain(|c| !y.contains(c));
                NotTokenSet(x).simplify()
            }
        }
    }
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
