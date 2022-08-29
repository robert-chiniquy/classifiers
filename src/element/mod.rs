mod negation;

// pub(crate) use negation::*;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    // exactly 1 token
    Question,
    // 1 or more or tokens
    Star,
    // The literal token seqence
    Token(char),
    // This matches any single char but this one.
    NotToken(char),
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

impl Invertible for Vec<Element> {
    #[tracing::instrument(ret)]
    fn inverse(&self) -> Vec<Self> {
        todo!();
    }
}

#[test]
fn test_one() {
    let sources = vec![("a", ["!a", "**"])];
    for (s, v) in sources {
        let p = path_from_str(s);
        let expected_paths: Vec<_> = v.into_iter().map(path_from_str).collect();
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
            e = Element::NotToken(c);
            negate_next = false;
        }
        v.push(e);
    }
    v
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
// ![aa] accepts - ?, ***, a -> !a

// 1. !a -> accept

impl BranchProduct<Element> for Element {
    #[tracing::instrument(ret)]
    fn product(a: &Self, b: &Self) -> Result<Vec<NfaBranch<Element>>, MatchingError> {
        use EdgeTransition::*;
        use Element::*;
        // println!("{a} <-> {b}");
        let r = match (a, b) {
            (Star, Star) => {
                // three edges, L, R, L+R
                vec![
                    NfaBranch::new(Star, Advance, Stay),
                    NfaBranch::new(Star, Stay, Advance),
                    NfaBranch::new(Star, Advance, Advance),
                ]
            }
            (Question, Question) => converge(a),
            (NotToken(y), NotToken(x)) => {
                if x == y {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            },
            (Token(x), Token(y)) => {
                if x == y {
                    converge(a)
                } else {
                    diverge(a, b)
                }
            }



            // (Star, Token(y)) => {
            //     // consume lr, consume left, or drop right...
            //     vec![
            //         // FIXME: expressly this should be a NotToken branch, then we have a DFA
            //         NfaBranch::new(Star, Advance, Stop),
            //         NfaBranch::new(b.clone(), Stay, Advance),
            //         // NfaBranch::new(*a, Stay, Advance), //?
            //         NfaBranch::new(b.clone(), Advance, Advance),
            //     ]
            // }
            // (Token(y), Star) => {
            //     vec![
            //         // FIXME: expressly this should be a NotToken branch
            //         NfaBranch::new(Star, Stop, Advance),
            //         NfaBranch::new(a.clone(), Advance, Stay),
            //         //NfaBranch::new(*b, Advance, Stay), // ?
            //         NfaBranch::new(a.clone(), Advance, Advance),
            //     ]
            // }


            (Star, _) => {
                // consume lr, consume left, or drop right...
                vec![
                    // FIXME: expressly this should be a NotToken branch, then we have a DFA
                    NfaBranch::new(Star, Advance, Stop),
                    NfaBranch::new(b.clone(), Stay, Advance),
                    // NfaBranch::new(*a, Stay, Advance), //?
                    NfaBranch::new(b.clone(), Advance, Advance),
                ]
            }
            (_, Star) => {
                vec![
                    // FIXME: expressly this should be a NotToken branch
                    NfaBranch::new(Star, Stop, Advance),
                    NfaBranch::new(a.clone(), Advance, Stay),
                    //NfaBranch::new(*b, Advance, Stay), // ?
                    NfaBranch::new(a.clone(), Advance, Advance),
                ]
            }
            
            (Question, Token(y)) => {
                vec![
                    // FIXME: expressly this should be a NotToken branch, then we have a DFA
                    NfaBranch::new(NotToken(y.clone()), Advance, Stop),
                    NfaBranch::new(Token(y.clone()), Advance, Advance),
                ]
            }

            (Token(x), Question) => {
                vec![
                    NfaBranch::new(Token(x.clone()), Advance, Advance),
                    NfaBranch::new(NotToken(x.clone()), Stop, Advance),
                ]
            }

            (Question, NotToken(y)) => {
                vec![
                    NfaBranch::new(NotToken(y.clone()), Advance, Advance),
                    NfaBranch::new(Token(y.clone()), Advance, Stop),
                ]
            }
            (NotToken(x), Question) => {
                vec![
                    NfaBranch::new(NotToken(x.clone()), Advance, Advance),
                    NfaBranch::new(Token(x.clone()), Advance, Stop),
                ]
            },
            (Token(x), NotToken(y)) => {
                if x == y {
                    diverge(a, b)
                } else {
                    converge(a)
                }
            }
            (NotToken(x), Token(y)) => {
                if x == y {
                    diverge(a, b)
                } else {
                    converge(a)
                }
            },
        };
        Ok(r)
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

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> Result<bool, MatchingError> {
        use Element::*;
        let r = match (self, &l) {
            (x, y) if x == y => true,
            // (Element::Question, Element::Question) => true,
            (Star, _) => true,
            (NotToken(b), Token(a)) => a != b,
            (Question, NotToken(_)) => true,
            (Question, Token(_)) => true,
            (_, Star) => false,
            // if len(seq) == 1, Q can match seq.  Otherwise, Q can't match multi chars.
            (Token(_), NotToken(_)) => false,
            (NotToken(_), Question) => false,
            (Token(_), Question) => false,
            (_, _) => false,
        };
        Ok(r)
    }
}

// TODO: FIXME: terminal_on needs to decompose seqs of len > 1 into component token/not token to call this method
impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> Result<bool, MatchingError> {
        let r = match self {
            Element::Question => true,
            Element::Star => true,
            Element::Token(n) => n == l,
            // Element::NotTokens(n) if n.len() == 1 => n[0] != *l,
            Element::NotToken(n) => n != l,
        };
        Ok(r)
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: char) -> Result<bool, MatchingError> {
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
                Element::NotToken(c) => format!("!{c}"),
                Element::Token(c) => format!("{c}"),
            }
        ))
    }
}
