use super::*;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Question,
    Star,
    Tokens(Vec<char>),
    NotTokens(Vec<char>),
    LoopNotTokens(Vec<char>),
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
                Element::Tokens(c) =>
                    format!("{}", c.iter().map(|c| c.to_string()).collect::<String>()),
                Element::NotTokens(c) => {
                    let s = c.iter().map(|c| c.to_string()).collect::<String>();
                    if &s.len() > &1 {
                        format!("!`{s}`")
                    } else {
                        format!("!{s}")
                    }
                }
                Element::LoopNotTokens(c) => {
                    let s = c.iter().map(|c| c.to_string()).collect::<String>();
                    if &s.len() > &1 {
                        format!("!`{s}`°")
                    } else {
                        format!("!{s}°")
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
            (NotTokens(b), Tokens(a)) => {
                // this should never happen
                debug_assert!(!b.is_empty());
                debug_assert!(!a.is_empty());
                return a != b;
            }
            (Question, NotTokens(n)) => {
                return n.len() == 1;
            }
            (Question, Tokens(n)) => {
                return n.len() == 1;
            }
            (LoopNotTokens(a), Tokens(b)) => {
                return a != b;
            }
            (LoopNotTokens(a), NotTokens(b)) => {
                return a != b;
            }

            (_, Star) => false,
            // if len(seq) == 1, Q can match seq.  Otherwise, Q can't match multi chars.
            (LoopNotTokens(_), Question) => false,
            (Tokens(_), NotTokens(_)) => false,
            (NotTokens(_), Question) => false,
            (Tokens(_), Question) => false,
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
            Element::Tokens(n) if n.len() == 1 => n[0] == *l,
            Element::NotTokens(n) if n.len() == 1 => n[0] != *l,
            Element::LoopNotTokens(n) if n.len() == 1 => n[0] != *l,
            _ => false,
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: char) -> bool {
        self.accepts(&l)
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

#[cfg(test)]
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
            e = Element::NotTokens(vec![c]);
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
        NfaBranch::new(a.clone(), Advance, Stop),
        NfaBranch::new(b.clone(), Stop, Advance),
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
            (Star, LoopNotTokens(_)) => {
                vec![
                    NfaBranch::new(Star, Advance, Stop),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                    NfaBranch::new(b.clone(), Advance, Stay),
                ]
            }
            (LoopNotTokens(_), Star) => {
                vec![
                    NfaBranch::new(Star, Stop, Advance),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                    NfaBranch::new(b.clone(), Advance, Stay),
                ]
            }
            (Star, _) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Stop),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    // NfaBranch::new(*a, Stay, Advance), //?
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (_, Star) => {
                vec![
                    NfaBranch::new(Star, Stop, Advance),
                    NfaBranch::new(a.clone(), Advance, Stay),
                    //NfaBranch::new(*b, Advance, Stay), // ?
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            (Question, Question) => converge(a),
            (Tokens(n), Question) => {
                if n.len() == 1 {
                    vec![
                        NfaBranch::new(Question, Stop, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Tokens(x), Tokens(y)) => {
                if x == y {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }
            (Tokens(x), NotTokens(y)) => {
                if x == y {
                    diverge(a, b)
                } else if x.len() == y.len() {
                    // a < b
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokens(x), Question) => {
                if x.len() == 1 {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(Element::Tokens(x.clone()), Stop, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokens(x), Tokens(y)) => {
                if x == y {
                    diverge(a, b)
                } else {
                    // x > y
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                }
            }
            (NotTokens(n1), NotTokens(n2)) => {
                if n1 == n2 {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }
            (Question, Tokens(y)) => {
                if y.len() == 1 {
                    // a > b
                    vec![
                        NfaBranch::new(Question, Advance, Stop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Question, NotTokens(y)) => {
                if y.len() == 1 {
                    // a > b
                    vec![
                        NfaBranch::new(Element::Tokens(y.clone()), Advance, Stop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Question, LoopNotTokens(y)) => {
                if y.len() == 1 {
                    vec![
                        NfaBranch::new(Question, Advance, Stop),
                        NfaBranch::new(b.clone(), Advance, Stay),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (Tokens(x), LoopNotTokens(y)) => {
                //  we really need some form of lookahead here!
                if x == y {
                    diverge(a, b)
                } else if x.len() == y.len() {
                    // a < b
                    vec![
                        NfaBranch::new(b.clone(), Stop, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Stay),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokens(x), LoopNotTokens(y)) => {
                if x == y {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Stay),
                    ]
                } else if x.len() == y.len() {
                    vec![
                        NfaBranch::new(b.clone(), Stop, Advance),
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(a.clone(), Advance, Stop),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (LoopNotTokens(_), Question) => todo!(),
            (LoopNotTokens(_), Tokens(_)) => todo!(),
            (LoopNotTokens(_), NotTokens(_)) => todo!(),
            (LoopNotTokens(_), LoopNotTokens(_)) => todo!(),
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
            c => Element::Tokens(vec![c]),
        }
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        match c {
            '*' => Element::Star,
            '?' => Element::Question,
            c => Element::Tokens(vec![*c]),
        }
    }
}
