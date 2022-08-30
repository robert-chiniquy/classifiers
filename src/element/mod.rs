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
    TokenSet(Vec<char>),
    // This matches any single char but this one.
    NotToken(char),
    // This matches any set of single chars but those in the vector
    NotTokensSet(Vec<char>),
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

impl BranchProduct<Element> for Element {
    #[tracing::instrument(ret)]
    fn product(a: &Self, b: &Self) -> Result<Vec<NfaBranch<Element>>, MatchingError> {
        use EdgeTransition::*;
        use Element::*;

        let r = match (a, b) {
            (Star, Star) => vec![
                NfaBranch::new(Star, Advance, Stay),
                NfaBranch::new(Star, Stay, Advance),
                NfaBranch::new(Star, Advance, Advance),
            ],
            (Star, Question) => vec![
                NfaBranch::new(Question, Stay, Advance),
                NfaBranch::new(Question, Advance, Advance),
            ],
            (Question, Star) => vec![
                NfaBranch::new(Question, Advance, Stay),
                NfaBranch::new(Question, Advance, Advance),
            ],
            (Star, Token(y)) => vec![
                NfaBranch::new(NotToken(*y), Advance, Stop),
                NfaBranch::new(Token(*y), Stay, Advance),
                NfaBranch::new(Token(*y), Advance, Advance),
            ],
            (Star, TokenSet(y)) => vec![
                NfaBranch::new(NotTokensSet(y.clone()), Advance, Stop),
                NfaBranch::new(TokenSet(y.clone()), Stay, Advance),
                NfaBranch::new(TokenSet(y.clone()), Advance, Advance),
            ],
            (Star, NotTokensSet(y)) => vec![
                NfaBranch::new(TokenSet(y.clone()), Advance, Stop),
                NfaBranch::new(NotTokensSet(y.clone()), Stay, Advance),
                NfaBranch::new(NotTokensSet(y.clone()), Advance, Advance),
            ],
            (TokenSet(x), Star) => vec![
                NfaBranch::new(NotTokensSet(x.clone()), Stop, Advance),
                NfaBranch::new(TokenSet(x.clone()), Advance, Stay),
                NfaBranch::new(TokenSet(x.clone()), Advance, Advance),
            ],
            (NotTokensSet(x), Star) => vec![
                NfaBranch::new(TokenSet(x.clone()), Stop, Advance),
                NfaBranch::new(NotTokensSet(x.clone()), Advance, Stay),
                NfaBranch::new(NotTokensSet(x.clone()), Advance, Advance),
            ],

            (Token(y), Star) => vec![
                NfaBranch::new(NotToken(*y), Stop, Advance),
                NfaBranch::new(Token(*y), Advance, Stay),
                NfaBranch::new(Token(*y), Advance, Advance),
            ],
            (Star, NotToken(y)) => vec![
                NfaBranch::new(Token(*y), Advance, Stop),
                NfaBranch::new(NotToken(*y), Stay, Advance),
                NfaBranch::new(NotToken(*y), Advance, Advance),
            ],
            (NotToken(y), Star) => vec![
                NfaBranch::new(Token(*y), Stop, Advance),
                NfaBranch::new(NotToken(*y), Advance, Stay),
                NfaBranch::new(NotToken(*y), Advance, Advance),
            ],

            /* Question */
            (Question, Question) => vec![NfaBranch::new(Question, Advance, Advance)],
            (Question, Token(y)) => vec![
                NfaBranch::new(NotToken(*y), Advance, Stop),
                NfaBranch::new(Token(*y), Advance, Advance),
            ],
            (Token(x), Question) => vec![
                NfaBranch::new(Token(*x), Advance, Advance),
                NfaBranch::new(NotToken(*x), Stop, Advance),
            ],
            (Question, NotToken(y)) => vec![
                NfaBranch::new(NotToken(*y), Advance, Advance),
                NfaBranch::new(Token(*y), Advance, Stop),
            ],
            (NotToken(x), Question) => vec![
                NfaBranch::new(NotToken(*x), Advance, Advance),
                NfaBranch::new(Token(*x), Advance, Stop),
            ],
            (Question, TokenSet(y)) => vec![
                NfaBranch::new(TokenSet(y.clone()), Advance, Advance),
                NfaBranch::new(NotTokensSet(y.clone()), Advance, Stop)
            ],
            (Question, NotTokensSet(y)) => vec![
                NfaBranch::new(NotTokensSet(y.clone()), Advance, Advance),
                NfaBranch::new(TokenSet(y.clone()), Advance, Stop)
            ],
            (TokenSet(x), Question) => vec![
                NfaBranch::new(TokenSet(x.clone()), Advance, Advance),
                NfaBranch::new(NotTokensSet(x.clone()), Stop, Advance),
            ],
            (NotTokensSet(x), Question) => vec![
                NfaBranch::new(NotTokensSet(x.clone()), Advance, Advance),
                NfaBranch::new(TokenSet(x.clone()), Stop, Advance),
            ],


            (Token(x), Token(y)) | (NotToken(y), NotToken(x)) => {
                if x == y {
                    vec![NfaBranch::new(a.clone(), Advance, Advance)]
                } else {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                }
            }
            (Token(x), NotTokensSet(y)) => {
                todo!()
            },

            (Token(x), NotToken(y)) => {
                if x != y {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                } else {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                }
            }
            (NotToken(x), Token(y)) => {
                if x == y {
                    diverge(a, b)
                } else {
                    converge(a)
                }
            }
            (Token(x), TokenSet(y)) => {
                if y.contains(x) {
                    let y: Vec<_> = y.iter().filter(|c| *c != x).cloned().collect();
                    if y.is_empty() {
                        vec![
                            NfaBranch::new(a.clone(), Advance, Advance),
                        ]
                    } else {
                        vec![
                            NfaBranch::new(a.clone(), Advance, Advance),
                            NfaBranch::new(TokenSet(y), Stop, Advance),
                        ]
                    }
                } else {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                }
            },

            (TokenSet(_), Token(_)) => todo!(),
            (TokenSet(_), TokenSet(_)) => todo!(),
            (TokenSet(_), NotToken(_)) => todo!(),
            (TokenSet(_), NotTokensSet(_)) => todo!(),
            (NotToken(_), TokenSet(_)) => todo!(),
            (NotToken(_), NotTokensSet(_)) => todo!(),
            (NotTokensSet(_), Token(_)) => todo!(),
            (NotTokensSet(_), TokenSet(_)) => todo!(),
            (NotTokensSet(_), NotToken(_)) => todo!(),
            (NotTokensSet(_), NotTokensSet(_)) => todo!(),
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
impl Remaindery<Element> for Element {
    fn is_valid(a: Element, b: Element) -> bool {
        use Element::*;
        match (a, b) {
            (Token(_), Question)
            | (NotToken(_), Question)
            | (TokenSet(_), Question)
            | (Token(_), Star)
            | (NotToken(_), Star)
            | (TokenSet(_), Star)
            | (NotTokensSet(_), Question)
            | (NotToken(_), NotToken(_))
            | (NotToken(_), NotTokensSet(_))
            | (NotTokensSet(_), NotToken(_))
            | (NotTokensSet(_), NotTokensSet(_))
            | (NotTokensSet(_), Star) => false,

            (Question, Question) => true,
            (Question, Star) => true,
            (Star, Question) => true,
            (Star, Star) => true,

            (Star, Token(_)) | (Question, Token(_)) => true,
            (Star, NotToken(_)) | (Question, NotToken(_)) => true,
            (Star, NotTokensSet(_)) | (Question, NotTokensSet(_)) => true,
            (Star, TokenSet(_)) | (Question, TokenSet(_)) => true,

            (Token(x), Token(y)) => x != y,
            (Token(x), NotToken(y)) => x == y,
            (Token(x), NotTokensSet(y)) => y.len() == 1 && x == y[0],

            (TokenSet(y), Token(x)) | (Token(x), TokenSet(y)) => !y.iter().any(|c| x == *c),
            (TokenSet(x), TokenSet(y)) => !x.into_iter().any(|c| y.contains(&c)),
            (TokenSet(x), NotToken(y)) => x.len() == 1 && x[0] == y,
            (TokenSet(x), NotTokensSet(y)) => y.len() == 1 && y[0] == x[0],

            (NotToken(x), Token(y)) => x == y,
            (NotToken(x), TokenSet(y)) => y.len() == 1 && y[0] == x,

            (NotTokensSet(x), Token(y)) => x.len() == 1 && y == x[0],
            (NotTokensSet(x), TokenSet(y)) => x.len() == 1 && y.len() == 1 && y[0] == x[0],
        }
    }
    fn remainder(a: Element, b: Element) -> Result<Option<Element>, String> {
        use Element::*;
        let err = Err("to much stuff".to_string());

        let d = match (a, b) {
            (Token(_), Question)
            | (Token(_), Star)
            | (NotToken(_), Question)
            | (NotToken(_), Star)
            | (TokenSet(_), Question)
            | (TokenSet(_), Star)
            | (NotTokensSet(_), Question)
            | (NotToken(_), NotToken(_))
            | (NotToken(_), NotTokensSet(_))
            | (NotTokensSet(_), NotToken(_))
            | (NotTokensSet(_), NotTokensSet(_))
            | (NotTokensSet(_), Star) => return err,

            (Question, Question) => None,
            (Question, Star) => None,
            (Star, Question) => None,
            (Star, Star) => None,

            (Star, Token(n)) | (Question, Token(n)) => Some(NotToken(n)),
            (Star, NotToken(n)) | (Question, NotToken(n)) => Some(Token(n)),
            (Star, NotTokensSet(v)) | (Question, NotTokensSet(v)) => Some(TokenSet(v)),
            (Star, TokenSet(y)) | (Question, TokenSet(y)) => Some(NotTokensSet(y)),

            (Token(x), Token(y)) => {
                if x == y {
                    return err;
                } else {
                    Some(NotTokensSet(vec![x, y]))
                }
            }
            (Token(x), NotToken(y)) => {
                if x == y {
                    None
                } else {
                    return err;
                }
            }
            (Token(x), NotTokensSet(y)) => {
                if y.len() == 0 {
                    Some(NotToken(x))
                } else if y.len() > 1 || x != y[0] {
                    return err;
                } else {
                    None
                }
            }

            (NotToken(x), Token(y)) => {
                if x == y {
                    None
                } else {
                    return err;
                }
            }

            (NotTokensSet(x), Token(y)) => {
                if x.len() == 0 {
                    Some(NotToken(y))
                } else if x.len() > 1 || y != x[0] {
                    return err;
                } else {
                    None
                }
            }

            (TokenSet(y), Token(x)) | (Token(x), TokenSet(y)) => {
                if y.iter().any(|c| x == *c) {
                    return err;
                }
                let mut y = y.clone();
                y.push(x.clone());
                Some(NotTokensSet(y))
            }

            (TokenSet(x), TokenSet(y)) => {
                if x.clone().into_iter().any(|c| y.contains(&c)) {
                    return err;
                }
                let mut x = x.clone();
                x.extend(y.iter());
                Some(NotTokensSet(x))
            }
            (TokenSet(x), NotToken(y)) => {
                if x.len() != 1 || x[0] != y {
                    return err;
                }
                None
            }
            (TokenSet(x), NotTokensSet(y)) => {
                if y.len() != 1 || y[0] != x[0] {
                    return err;
                }
                None
            }
            (NotToken(x), TokenSet(y)) => {
                if y.len() != 1 || y[0] != x {
                    return err;
                }
                None
            }
            (NotTokensSet(x), TokenSet(y)) => {
                if x.len() != 1 || y.len() != 1 || y[0] != x[0] {
                    return err;
                }
                None
            }
        };

        Ok(d)
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
            Element::NotToken(n) => n != l,
            Element::NotTokensSet(v) => !v.into_iter().any(|c| c == l),
            Element::TokenSet(v) => v.into_iter().any(|c| c == l),
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
                Element::Token(c) => format!("{c}"),
                Element::NotToken(c) => format!("!{c}"),
                Element::NotTokensSet(v) => format!("!`{v:?}`"),
                Element::TokenSet(v) => format!("`{v:?}`"),
            }
        ))
    }
}
