use super::*;
use std::{collections::HashSet, default};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
                Element::NotTokenSeq(c) => format!("'!{c}'"),
                Element::TokenSeq(c) => format!("'{c:?}'"),
                Element::NotSe(c) => format!("'!{c:?}'"),
            }
        ))
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> bool {
        match (self, l) {
            (x, y) if x == &y => true,
            (Element::Token(_), Element::Question) => false,
            (Element::Token(_), Element::Star) => false,
            (Element::Question, Element::Token(_)) => true,
            (Element::Question, Element::Question) => true,
            (Element::Question, Element::Star) => false,
            (Element::Question, Element::NotToken(_)) => ,

            (Element::Star, Element::Token(_)) => true,
            (Element::Star, Element::Question) => true,
            (Element::Star, Element::Star) => true,
            (Element::Token(c), Element::NotTokenSeq(nc))
            | (Element::NotToken(nc), Element::Token(c)) => {
                c != nc
            },
            (Element::TokenSeq(a), Element::Token(b)) |
            (Element::Token(b), Element::TokenSeq(a)) => {
                if a.is_empty() || a.len() > 1 {
                    return false;
                }
                return a[0] == b;
            },
            (Element::TokenSeq(a), Element::NotToken(b)) |
            (Element::NotToken(b), Element::TokenSeq(a)) => {
                if a.is_empty() || a.len() > 1 {
                    return false;
                }
                return a[0] != b;
            },

            (Element::TokenSeq(a), Element::NotTokenSeq(b)) => false,
            (Element::NotTokenSeq(b), Element::TokenSeq(a)) => {
                /// this should never happen
                debug_assert!(!b.is_empty());
                debug_assert!(!a.is_empty());
                return a != b;
            }
            // ¿![ab] accept ![a]?  : NO
            // ¿![a] accept ![ab]?  : Yes

            //  !ab = Any([!a, ?], [?, !b], [?], [***])
            //  !Tokens(ab) = [!a, b], [!a, !b], [a, !b]

            // TODO: not token needed here
            // (_, _) => false,
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
                        NfaBranch::new(*a, Drop, Advance),
                        NfaBranch::new(*b, Advance, Drop),
                    ]
                } else {
                    // L < R
                    // one edge for both advancing, one edge for only R advancing
                    vec![
                        NfaBranch::new(*b, Drop, Advance),
                        NfaBranch::new(*a, Advance, Advance),
                    ]
                }
            }
            (NotToken(n), Token(c)) => {
                if c == n {
                    // mutually exclusive, add 2 paths, one for each side
                    vec![
                        NfaBranch::new(*a, Drop, Advance),
                        NfaBranch::new(*b, Advance, Drop),
                    ]
                } else {
                    vec![
                        NfaBranch::new(*a, Advance, Drop),
                        NfaBranch::new(*b, Advance, Advance),
                    ]
                }
            }
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (NotToken(c1), NotToken(c2)) | (Token(c1), Token(c2)) => {
                if c1 == c2 {
                    // advance both
                    vec![NfaBranch::new(*a, Advance, Advance)]
                } else {
                    // disjoint
                    vec![
                        NfaBranch::new(*a, Advance, Drop),
                        NfaBranch::new(*b, Drop, Advance),
                    ]
                }
            }
            (Star, NotToken(_)) | (Star, Token(_)) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(*b, Stay, Advance),
                    // NfaBranch::new(*a, Stay, Advance), //?
                    NfaBranch::new(*b, Advance, Advance),
                ]
            }
            (Star, Question) => {
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(Question, Stay, Advance),
                    NfaBranch::new(Question, Advance, Advance),
                ]
            }
            (NotToken(_), Star) | (Token(_), Star) => {
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(*a, Advance, Stay),
                    //NfaBranch::new(*b, Advance, Stay), // ?
                    NfaBranch::new(*a, Advance, Advance),
                ]
            }
            (Question, Star) => {
                // The union path is ?
                // the star path is * minus ?
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(Question, Advance, Stay),
                    NfaBranch::new(Question, Advance, Advance),
                ]
            }
            (NotToken(_), Question) | (Token(_), Question) => {
                // TODO: Optimization: Token(c) and ? optimizes to Token(c) and NotToken(c)
                // ? > t
                // L < R
                // branch for L+R and just R
                vec![
                    NfaBranch::new(Question, Drop, Advance),
                    NfaBranch::new(*a, Advance, Advance),
                ]
            }
            (Question, NotToken(_)) | (Question, Token(_)) => {
                vec![
                    NfaBranch::new(Question, Advance, Drop),
                    NfaBranch::new(*b, Advance, Advance),
                ]
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
