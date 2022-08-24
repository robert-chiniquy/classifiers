use super::*;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Question,
    Star,
    TokenSeq(Vec<char>),
    NotTokenSeq(Vec<char>),
    NotTokenSeqLoop(Vec<char>),
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
                Element::Star => "*°".to_string(),
                Element::NotTokenSeq(c) => {
                    if c.len() > 1 {
                        format!("!'{c:?}'")
                    } else {
                        format!("!{c:?}")
                    }
                }
                Element::TokenSeq(c) => {
                    if c.len() > 1 {
                        format!("'{c:?}'")
                    } else {
                        format!("{c:?}")
                    }
                }
                Element::NotTokenSeqLoop(c) => {
                    if c.len() > 1 {
                        format!("!'{c:?}'°")
                    } else {
                        format!("!{c:?}°")
                    }
                }
            }
        ))
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> bool {
        use Element::*;
        match (self, &l) {
            (x, y) if x == y => true,
            // (Element::Question, Element::Question) => true,
            (Star, _) => true,
            (NotTokenSeq(b), TokenSeq(a)) => {
                // this should never happen
                debug_assert!(!b.is_empty());
                debug_assert!(!a.is_empty());
                return a != b;
            }
            (Question, NotTokenSeq(n)) => {
                return n.len() == 1;
            }
            (Question, TokenSeq(n)) => {
                return n.len() == 1;
            }
            (NotTokenSeqLoop(a), TokenSeq(b)) => {
                return a != b;
            }
            (NotTokenSeqLoop(a), NotTokenSeq(b)) => {
                return a != b;
            }

            (_, Star) => false,
            // if len(seq) == 1, Q can match seq.  Otherwise, Q can't match multi chars.
            (NotTokenSeqLoop(_), Question) => false, 
            (TokenSeq(_), NotTokenSeq(_)) => false,
            (NotTokenSeq(_), Question) => false,
            (TokenSeq(_), Question) => false,
            (_, _) => false,
        }
    }
}

// TODO: FIXME: terminal_on needs to decompose seqs of len > 1 into component token/not token to call this method
impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Question => true,
            Element::Star => true,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == *l,
            Element::NotTokenSeq(n) if n.len() == 1 => n[0] != *l,
            Element::NotTokenSeqLoop(n) if n.len() == 1 => n[0] != *l,
            _ => false,
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: char) -> bool {
        return self.accepts(&l);
    }
}

#[test]
fn test_blah() {
    let sources = vec![("a", ["!a", "**"])];
    for (s, v) in sources {
        let p = path_from_str(s);
        let expected_paths: HashSet<_> = v.into_iter().map(path_from_str).collect();
        assert_eq!(p.inverse(), expected_paths);
    }
}

fn path_from_str(s: &str) -> Vec<Element> {
    let mut negate_next = false;
    let mut v: Vec<_> = Default::default();
    for c in s.chars() {
        if c == '!' {
            negate_next = true;
            continue;
        }
        let mut e = c.into();
        if negate_next {
            e = Element::NotTokenSeq(vec![c]);
            negate_next = false;
        }
        v.push(e);
    }
    v
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Elementals {
    Tokens(Vec<char>),
    NotTokens(Vec<char>),
    Questions(usize),
    Globulars(usize),
}

impl Invertible for Vec<Element> {
    fn inverse(&self) -> HashSet<Self> {
        todo!()
    }
}

fn diverge(a: &Element, b: &Element) -> Vec<NfaBranch<Element>> {
    use EdgeTransition::*;
    vec![
        NfaBranch::new(a.clone(), Advance, Dropp),
        NfaBranch::new(b.clone(), Dropp, Advance),
    ]
}

fn converge(a: &Element) -> Vec<NfaBranch<Element>> {
    use EdgeTransition::*;
    vec![NfaBranch::new(a.clone(), Advance, Advance)]
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
            (Star, NotTokenSeqLoop(_)) => {
                vec![
                    NfaBranch::new(Star, Advance, Dropp),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                    NfaBranch::new(b.clone(), Advance, Stay),
                ]
            },
            (NotTokenSeqLoop(_), Star) => {
                vec![
                    NfaBranch::new(Star, Dropp, Advance),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                    NfaBranch::new(b.clone(), Advance, Stay),
                ]
            },
            (Star, _) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Dropp),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    // NfaBranch::new(*a, Stay, Advance), //?
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (_, Star) => {
                vec![
                    NfaBranch::new(Star, Dropp, Advance),
                    NfaBranch::new(a.clone(), Advance, Stay),
                    //NfaBranch::new(*b, Advance, Stay), // ?
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            (Question, Question) => converge(a),
            (TokenSeq(n), Question) => {
                if n.len() == 1 {
                    vec![
                        NfaBranch::new(Question, Dropp, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (TokenSeq(x), TokenSeq(y)) => {
                if x == y {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }
            (TokenSeq(x), NotTokenSeq(y)) => {
                if x == y {
                    diverge(a, b)
                } else if x.len() == y.len() {
                    // a < b
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(b.clone(), Dropp, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokenSeq(x), Question) => {
                if x.len() == 1 {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(Element::TokenSeq(x.clone()), Dropp, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokenSeq(x), TokenSeq(y)) => {
                if x == y {
                    diverge(a, b)
                } else {
                    // x > y
                    vec![
                        NfaBranch::new(a.clone(), Advance, Dropp),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                }
            }
            (NotTokenSeq(n1), NotTokenSeq(n2)) => {
                if n1 == n2 {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }
            (Question, TokenSeq(y)) => {
                if y.len() == 1 {
                    // a > b
                    vec![
                        NfaBranch::new(Question, Advance, Dropp),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Question, NotTokenSeq(y)) => {
                if y.len() == 1 {
                    // a > b
                    vec![
                        NfaBranch::new(Element::TokenSeq(y.clone()), Advance, Dropp),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Question, NotTokenSeqLoop(y)) => {
                if y.len() == 1 {
                    vec![
                        NfaBranch::new(Question, Advance, Dropp), 
                        NfaBranch::new(b.clone(), Advance, Stay),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }

            },
            (TokenSeq(x), NotTokenSeqLoop(y)) => {
                if x == y {
                    diverge(a, b)
                } else if x.len() == y.len() {
                    // a < b
                    vec![
                        NfaBranch::new(b.clone(), Dropp, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Stay),
                    ]
                } else {
                    diverge(a, b)
                }
            },
            (NotTokenSeq(x), NotTokenSeqLoop(y)) => {
                if x == y {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Stay),
                    ]

                } else if x.len() == y.len() {
                    vec![
                        NfaBranch::new(b.clone(), Dropp, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Dropp),
                    ]
                } else {
                    diverge(a, b)
                }
            },
            (NotTokenSeqLoop(_), Question) => todo!(),
            (NotTokenSeqLoop(_), TokenSeq(_)) => todo!(),
            (NotTokenSeqLoop(_), NotTokenSeq(_)) => todo!(),
            (NotTokenSeqLoop(_), NotTokenSeqLoop(_)) => todo!(),
        }
    }
}

impl<M> Nfa<NfaNode<M>, NfaEdge<Element>>
where
    M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
{
    pub fn accepts_string(&self, s: &str) -> bool {
        self.accepts(&str_to_chars(s))
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
            c => Element::TokenSeq(vec![c]),
        }
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        match c {
            '*' => Element::Star,
            '?' => Element::Question,
            c => Element::TokenSeq(vec![*c]),
        }
    }
}
