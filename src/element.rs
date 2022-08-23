use super::*;
use std::{collections::HashSet, default};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Token(char),
    TokenSeq(Vec<char>),
    Question,
    Star,
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
                Element::Token(c) => format!("'{c}'"),
                Element::Question => "?".to_string(),
                Element::Star => "*".to_string(),
                Element::TokenSeq(c) => format!("'{c:?}'"),
            }
        ))
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> bool {
        match (self, &l) {
            (x, y) if x == y => true,
            (Element::Token(_), Element::Question) => false,
            (Element::Question, Element::Token(_)) => true,
            (Element::Question, Element::Question) => true,
            (_, Element::Star) => false,
            (Element::Star, _) => true,
            (Element::TokenSeq(a), Element::Token(b))
            | (Element::Token(b), Element::TokenSeq(a)) => {
                if a.is_empty() || a.len() > 1 {
                    return false;
                }
                return a[0] == *b;
            }

            (Element::Question, Element::TokenSeq(n)) => {
                return n.len() == 1;
            }
            (Element::TokenSeq(_), Element::Question) => false,
            (_, _) => false,
        }
    }
}

// TODO: FIXME: terminal_on needs to decompose seqs of len > 1 into component token/not token to call this method
impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Token(c) => c == l,
            Element::Question => true,
            Element::Star => true,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == *l,
            _ => false,
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: char) -> bool {
        match self {
            Element::Token(c) => c == &l,
            Element::Question => true,
            Element::Star => true,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == l,
            _ => false,
        }
    }
}

#[test]
fn test_blah() {
    let sources = vec![("a", ["!a", "**"])];
    for (s, v) in sources {
        let p = path_from_str(s);
        let expected_paths: HashSet<_> = v.into_iter().map(path_from_str).collect();
        // assert_eq!(p.inverse(), expected_paths);
    }
}

fn path_from_str(s: &str) -> Vec<Element> {
    let mut v: Vec<_> = Default::default();
    for c in s.chars() {
        v.push(c.into());
    }
    v
}

fn diverge(a: &Element, b: &Element) -> Vec<NfaBranch<Element>> {
    use EdgeTransition::*;
    vec![
        NfaBranch::new(a.clone(), Advance, Drop),
        NfaBranch::new(b.clone(), Drop, Advance),
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
            // todo case for not token plus token and vice versa
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (Token(c1), Token(c2)) => {
                if c1 == c2 {
                    // advance both
                    vec![NfaBranch::new(a.clone(), Advance, Advance)]
                } else {
                    // disjoint
                    vec![
                        NfaBranch::new(a.clone(), Advance, Drop),
                        NfaBranch::new(b.clone(), Drop, Advance),
                    ]
                }
            }
            (Star, _) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    // NfaBranch::new(*a, Stay, Advance), //?
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (_, Star) => {
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(a.clone(), Advance, Stay),
                    //NfaBranch::new(*b, Advance, Stay), // ?
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            (Token(_), Question) => {
                // TODO: Optimization: Token(c) and ? optimizes to Token(c) and NotToken(c)
                // ? > t
                // L < R
                // branch for L+R and just R
                vec![
                    NfaBranch::new(Question, Drop, Advance),
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            (Question, Token(_)) => {
                vec![
                    NfaBranch::new(Question, Advance, Drop),
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (TokenSeq(n), Question) => {
                if n.len() == 1 {
                    vec![
                        NfaBranch::new(Question, Drop, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (TokenSeq(x), Token(y)) => {
                if x.len() == 1 && x[0] == *y {
                    converge(b)
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

            (Question, TokenSeq(y)) => {
                if y.len() == 1 {
                    // a > b
                    vec![
                        NfaBranch::new(Question, Advance, Drop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Token(x), TokenSeq(y)) => {
                if y.len() == 1 && *x == y[0] {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }
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
            c => Element::Token(c),
        }
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        match c {
            '*' => Element::Star,
            '?' => Element::Question,
            c => Element::Token(*c),
        }
    }
}
