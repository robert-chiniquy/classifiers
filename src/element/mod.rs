// pub(crate) use negation::*;

use std::hash::{Hash, Hasher};
use std::{collections::HashSet, ops::Sub};

use super::*;

// TODO: explicit Hash impl
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Element {
    Question,
    Star,
    TokenSet(HashSet<char>),
    NotTokenSet(HashSet<char>),
}

impl Hash for Element {
    // TODO: this is **really** inefficient
    fn hash<H: Hasher>(&self, state: &mut H) {
        let s = match self {
            Element::Question => "?".to_string(),
            Element::Star => "*".to_string(),
            Element::TokenSet(v) => {
                let mut v = v.iter().cloned().collect::<Vec<_>>();
                v.sort();
                format!("ts|{v:?}")
            }
            Element::NotTokenSet(v) => {
                let mut v = v.iter().cloned().collect::<Vec<_>>();
                v.sort();
                format!("nts|{v:?}")
            }
        };
        state.write(s.as_bytes());
    }
}

impl Element {
    pub fn token(c: char) -> Element {
        Element::TokenSet(HashSet::from_iter(vec![c]))
    }

    pub fn not_token(c: char) -> Element {
        Element::NotTokenSet(HashSet::from_iter(vec![c]))
    }

    pub fn tokens(v: Vec<char>) -> Element {
        Element::TokenSet(HashSet::from_iter(v.into_iter()))
    }

    pub fn not_tokens(v: Vec<char>) -> Element {
        Element::NotTokenSet(HashSet::from_iter(v.into_iter()))
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
    fn accepts(&self, l: Element) -> Result<bool, GeneralError> {
        use Element::*;
        let r = match (self, &l) {
            (x, y) if x == y => true,
            (Star, _) => true,
            (_, Star) => false,
            (Question, _) => true,
            (_, Question) => false,
            // Are all other ASCII characters specified in the not token set?
            (TokenSet(x), NotTokenSet(y)) => 128 - y.len() == x.len() && y.sub(x).len() == y.len(),
            (NotTokenSet(x), TokenSet(y)) => x.is_disjoint(y),
            (_, _) => false,
        };
        Ok(r)
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> Result<bool, GeneralError> {
        let r = match self {
            Element::Question => true,
            Element::Star => true,
            Element::NotTokenSet(v) => !v.into_iter().any(|c| c == l),
            Element::TokenSet(v) => v.into_iter().any(|c| c == l),
        };
        Ok(r)
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: char) -> Result<bool, GeneralError> {
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
                Element::NotTokenSet(v) => format!("!`{v:?}`"),
                Element::TokenSet(v) => format!("`{v:?}`"),
            }
        ))
    }
}

impl Product<Element> for Element {
    #[tracing::instrument(ret)]
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
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Advance, Stop));
                }
                v
            }

            (Question, Star) | (TokenSet(_), Star) | (NotTokenSet(_), Star) => {
                let mut v = vec![
                    NfaBranch::new(a.clone(), Advance, Stay),
                    NfaBranch::new(a.clone(), Advance, Advance),
                ];
                let c = a.clone().complement();
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Stop, Advance));
                }
                v
            }

            /* Question */
            (Question, Question) | (Question, TokenSet(_)) | (Question, NotTokenSet(_)) => {
                let mut v = vec![NfaBranch::new(b.clone(), Advance, Advance)];
                let c = b.clone().complement();
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Advance, Stop));
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
                    .collect::<HashSet<_>>();

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
                let center = x.clone().union(y).cloned().collect::<HashSet<char>>();
                let right = x.iter().filter(|c| !y.contains(c)).cloned().collect();

                vec![
                    NfaBranch::new(TokenSet(left), Advance, Stop),
                    NfaBranch::new(NotTokenSet(center), Advance, Advance),
                    NfaBranch::new(TokenSet(right), Advance, Stop),
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
        branches.into_iter().filter(|b| match &b.kind {
            Question => true,
            Star => true,
            TokenSet(v) => !v.is_empty(),
            NotTokenSet(v) => !v.is_empty(),
        }).collect()
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
    assert_eq!(r, Element::not_tokens(vec!['a', 'b', 'c']));

    let mut r = Star;
    r = Element::difference(&r, &t('a')).unwrap();
    r = Element::difference(&r, &t('b')).unwrap();
    assert_eq!(r, nts(vec!['a', 'b']));

    let mut r = Question;
    r = Element::difference(&r, &t('a')).unwrap();
    r = Element::difference(&r, &t('b')).unwrap();
    assert_eq!(r, nts(vec!['a', 'b']));

    let mut r = ts(vec!['a', 'b']);
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
        nfa.entry.insert(prior);
        for c in s.chars() {
            let target = nfa.add_node(NfaNode::new(Terminal::Not));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = Terminal::Accept(m);
        nfa
    }
}
