use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Token(char),
    Question,
    Star,
    NotToken(char),
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
                Element::NotToken(c) => format!("'{c}'"),
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
            (Element::Star, Element::Token(_)) => true,
            (Element::Star, Element::Question) => true,
            (Element::Star, Element::Star) => true,
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
        let mut prior = nfa.add_node(NfaNode::new([Terminal::Not].into()));
        nfa.entry.insert(prior);
        for c in s.chars() {
            let target = nfa.add_node(NfaNode::new([Terminal::Not].into()));
            let _ = nfa.add_edge(NfaEdge { criteria: c.into() }, prior, target);
            prior = target;
        }
        nfa.node_mut(prior).state = [Terminal::Accept(m)].into();
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
