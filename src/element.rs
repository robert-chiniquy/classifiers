use super::*;
use std::{collections::HashSet, default};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Token(char),
    TokenSeq(Vec<char>),
    Question,
    Star,
    NotToken(char),
    NotTokenSeq(Vec<char>),
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
                Element::NotTokenSeq(c) => format!("!'{c:?}'"),
                Element::TokenSeq(c) => format!("'{c:?}'"),
                Element::NotToken(c) => format!("!'{c:?}'"),
            }
        ))
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> bool {
        match (self, &l) {
            (x, y) if x == y => true,
            (Element::NotToken(_), Element::Question) | (Element::Token(_), Element::Question) => {
                false
            }
            (Element::Question, Element::NotToken(_)) | (Element::Question, Element::Token(_)) => {
                true
            }
            (Element::Question, Element::Question) => true,
            (_, Element::Star) => false,
            (Element::Star, _) => true,
            (Element::Token(c), Element::NotToken(nc))
            | (Element::NotToken(nc), Element::Token(c)) => c != nc,
            (Element::TokenSeq(a), Element::Token(b))
            | (Element::Token(b), Element::TokenSeq(a)) => {
                if a.is_empty() || a.len() > 1 {
                    return false;
                }
                return a[0] == *b;
            }
            (Element::TokenSeq(a), Element::NotToken(b))
            | (Element::NotToken(b), Element::TokenSeq(a)) => {
                if a.is_empty() || a.len() > 1 {
                    return false;
                }
                return a[0] != *b;
            }
            (Element::TokenSeq(_), Element::NotTokenSeq(_)) => false,
            (Element::NotTokenSeq(b), Element::TokenSeq(a)) => {
                // this should never happen
                debug_assert!(!b.is_empty());
                debug_assert!(!a.is_empty());
                return a != b;
            }
            (Element::Question, Element::NotTokenSeq(n)) => {
                return n.len() == 1;
            }
            (Element::NotTokenSeq(_), Element::Question) => false,
            (Element::Question, Element::TokenSeq(n)) => {
                return n.len() == 1;
            }
            (Element::TokenSeq(_), Element::Question) => false,
            // ¿![ab] accept ![a]?  : NO
            // ¿![a] accept ![ab]?  : Yes

            //  !ab = Any([!a, ?], [?, !b], [?], [***])
            //  !Tokens(ab) = [!a, b], [!a, !b], [a, !b]

            // TODO: not token needed here
            (_, _) => false,
        }
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Token(c) => c == l,
            Element::Question => true,
            Element::Star => true,
            Element::NotToken(c) => c != l,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == *l,
            Element::NotTokenSeq(n) if n.len() == 1 => n[0] != *l,
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
            Element::NotToken(c) => c != &l,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == l,
            Element::NotTokenSeq(n) if n.len() == 1 => n[0] != l,
        }
    }
}

#[test]
fn blah() {
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
            e = Element::NotToken(c);
            negate_next = false;
        }
        v.push(e);
    }
    v
}

impl Invertible for Vec<Element> {
    fn inverse(&self) -> HashSet<Self> {
        // ab -> [a, b] -> [!a, ?], [?, !b] + [?], [***] OR [!ab], [?], [***]
        // a*b -> [a, *, b]
        // a**b -> [a, **, b]
        // a?*b -> [a, ?*, b]
        // a?b -> [a, ?, b]
        // !a -> [a]

        // [!a, ?], [?, !b], [?], [*, *, *]
        let mut parts: Vec<Vec<_>> = Default::default();
        let mut buf: Vec<_> = vec![];

        for c in self {
            match c {
                Element::Token(c) => {
                    if buf.len() > 0 {
                        parts.push(buf.clone());
                        buf.truncate(0);
                    }
                    parts.push(vec![Element::NotToken(c.clone())]);
                }
                Element::Star | Element::Question => {
                    buf.push(c.clone());
                }

                Element::NotToken(c) => {
                    if buf.len() > 0 {
                        parts.push(buf.clone());
                        buf.truncate(0);
                    }
                    parts.push(vec![Element::Token(c.clone())]);
                }
                Element::TokenSeq(_) => todo!(),
                Element::NotTokenSeq(_) => todo!(),
            }
        }
        todo!();
    }
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
            (Token(c), NotToken(n)) => {
                if c == n {
                    // mutually exclusive, add 2 paths, one for each side
                    vec![
                        NfaBranch::new(a.clone(), Drop, Advance),
                        NfaBranch::new(b.clone(), Advance, Drop),
                    ]
                } else {
                    // L < R
                    // one edge for both advancing, one edge for only R advancing
                    vec![
                        NfaBranch::new(b.clone(), Drop, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                    ]
                }
            }
            (NotToken(n), Token(c)) => {
                if c == n {
                    // mutually exclusive, add 2 paths, one for each side
                    vec![
                        NfaBranch::new(a.clone(), Drop, Advance),
                        NfaBranch::new(b.clone(), Advance, Drop),
                    ]
                } else {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Drop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                }
            }
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (NotToken(c1), NotToken(c2)) | (Token(c1), Token(c2)) => {
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
            (NotToken(_), Question) | (Token(_), Question) => {
                // TODO: Optimization: Token(c) and ? optimizes to Token(c) and NotToken(c)
                // ? > t
                // L < R
                // branch for L+R and just R
                vec![
                    NfaBranch::new(Question, Drop, Advance),
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            (Question, NotToken(_)) | (Question, Token(_)) => {
                vec![
                    NfaBranch::new(Question, Advance, Drop),
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (TokenSeq(n), Question) => {
                vec![]
            }
            (TokenSeq(_), Token(_)) => {
                vec![]
            }
            (TokenSeq(_), NotToken(_)) => {
                vec![]
            }
            (TokenSeq(_), TokenSeq(_)) => {
                vec![]
            }
            (TokenSeq(_), NotTokenSeq(_)) => {
                vec![]
            }
            (NotTokenSeq(_), Question) => {
                vec![]
            }
            (NotTokenSeq(_), Token(_)) => {
                vec![]
            }
            (NotTokenSeq(_), NotToken(_)) => {
                vec![]
            }
            (NotTokenSeq(_), TokenSeq(_)) => {
                vec![]
            }
            (NotTokenSeq(n1), NotTokenSeq(n2)) => {
                if n1 == n2 {
                    vec![]
                } else {
                    // diverge
                    vec![
                        NfaBranch::new(b.clone(), Drop, Advance),
                        NfaBranch::new(a.clone(), Advance, Drop),
                    ]
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
