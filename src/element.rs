use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Token(char),
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
            }
        ))
    }
}

impl Accepts<Element> for Element {
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
            (_, _) => false,
        }
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Token(c) => c == l,
            Element::Question => true,
            Element::Star => true,
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all)]
    fn accepts(&self, l: char) -> bool {
        match self {
            Element::Token(c) => c == &l,
            Element::Question => true,
            Element::Star => true,
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
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (Token(c1), Token(c2)) => {
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
            (Star, Token(_)) => {
                // consume lr, consume left, or drop right...
                vec![
                    NfaBranch::new(Star, Advance, Drop),
                    NfaBranch::new(*b, Stay, Advance),
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
            (Token(_), Star) => {
                vec![
                    NfaBranch::new(Star, Drop, Advance),
                    NfaBranch::new(*a, Advance, Stay),
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
            (Token(_), Question) => {
                vec![
                    NfaBranch::new(Question, Drop, Advance),
                    NfaBranch::new(*a, Advance, Advance),
                ]
            }

            (Question, Token(_)) => {
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
            c => Element::Token(c.clone()),
        }
    }
}
