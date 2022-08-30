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
    NotTokenSet(Vec<char>),
}
impl Compliment<Element> for Element {
    fn compliment(self) -> Option<Self> {
        use Element::*;
        match self {
            Question => None,
            Star => None,
            Token(n) => Some(NotToken(n.clone())),
            TokenSet(n) => {
                if n.len() == 1 {
                    Some(NotTokenSet(n.clone()))
                } else {
                    Some(NotToken(n[0].clone()))
                }
            }
            NotToken(n) => Some(Token(n.clone())),
            NotTokenSet(n) => {
                if n.len() == 1 {
                    Some(Token(n[0].clone()))
                } else {
                    Some(TokenSet(n.clone()))
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
            (Star, Question)
            | (Star, Token(_))
            | (Star, NotToken(_))
            | (Star, TokenSet(_))
            | (Star, NotTokenSet(_)) => {
                let mut v = vec![
                    NfaBranch::new(b.clone(), Stay, Advance),
                    NfaBranch::new(b.clone(), Advance, Advance),
                ];
                let c = b.clone().compliment();
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Advance, Stop));
                }
                v
            }
            (Token(_), Star)
            | (Question, Star)
            | (NotToken(_), Star)
            | (TokenSet(_), Star)
            | (NotTokenSet(_), Star) => {
                let mut v = vec![
                    NfaBranch::new(a.clone(), Advance, Stay),
                    NfaBranch::new(a.clone(), Advance, Advance),
                ];
                let c = a.clone().compliment();
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Stop, Advance));
                }
                v
            }

            /* Question */
            (Question, Question)
            | (Question, Token(_))
            | (Question, NotToken(_))
            | (Question, TokenSet(_))
            | (Question, NotTokenSet(_)) => {
                let mut v = vec![NfaBranch::new(b.clone(), Advance, Advance)];
                let c = b.clone().compliment();
                if c.is_some() {
                    v.push(NfaBranch::new(c.unwrap(), Advance, Stop));
                }
                v
            }

            (Token(_), Question)
            | (NotToken(_), Question)
            | (TokenSet(_), Question)
            | (NotTokenSet(_), Question) => vec![
                NfaBranch::new(a.clone(), Advance, Advance),
                NfaBranch::new(a.clone().compliment().unwrap(), Stop, Advance),
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

            (Token(x), NotToken(y)) | (NotToken(y), Token(x)) => {
                if x == y {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                } else {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                }
            }

            (Token(x), TokenSet(y)) | (TokenSet(y), Token(x)) => {
                if y.contains(x) {
                    let y: Vec<_> = y.iter().filter(|c| *c != x).cloned().collect();
                    if y.is_empty() {
                        vec![NfaBranch::new(a.clone(), Advance, Advance)]
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
            }

            (TokenSet(x), TokenSet(y)) => {
                let mut v = vec![];

                let matching: Vec<_> = x.iter().filter(|c| y.contains(c)).cloned().collect();
                if matching.len() == 1 {
                    v.push(NfaBranch::new(Token(matching[0].clone()), Advance, Advance));
                } else if matching.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(matching.clone()), Advance, Advance));
                }

                let left: Vec<_> = x.iter().filter(|c|!matching.contains(c)).cloned().collect();
                if left.len() == 1 {
                    v.push(NfaBranch::new(Token(left[0].clone()), Advance, Stop));
                } else if matching.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(left.clone()), Advance, Stop));
                }

                let right: Vec<_> = y.iter().filter(|c|!matching.contains(c)).cloned().collect();
                if right.len() == 1 {
                    v.push(NfaBranch::new(Token(right[0].clone()), Stop, Advance));
                } else if matching.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(right.clone()), Stop, Advance));
                }
                v
            },

            (NotTokenSet(x), NotTokenSet(y)) => {
                // [!a,!b] X [!a,!c] -> [c], [!a,!b,!c], [b]
                let mut v = vec![];

                let mut sum = x.clone();
                sum.extend(y.clone().iter());
                sum.dedup();
                
                if !sum.is_empty() {
                    v.push(NfaBranch::new(NotTokenSet(sum), Advance, Advance));
                }
                
                let left: Vec<_> = y.iter().filter(|c| !x.contains(c)).cloned().collect();
                if left.len() == 1 {
                    v.push(NfaBranch::new(Token(left[0].clone()), Advance, Stop));
                } else if left.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(left), Advance, Stop));
                }

                let right: Vec<_> = x.iter().filter(|c| !y.contains(c)).cloned().collect();
                if right.len() == 1 {
                    v.push(NfaBranch::new(Token(right[0].clone()), Advance, Stop));
                } else if right.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(right), Advance, Stop));
                }

                v
            },
            (NotTokenSet(x), Token(y)) => {
                if x.contains(y) {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                } else {
                    let mut expanded_set = x.clone();
                    expanded_set.push(y.clone());
                    vec![
                        NfaBranch::new(Token(y.clone()), Advance, Advance),
                        NfaBranch::new(NotTokenSet(expanded_set), Advance, Stop),
                    ]
                }
            }
            (Token(x), NotTokenSet(y)) => {
                if y.contains(x) {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Stop),
                        NfaBranch::new(b.clone(), Stop, Advance),
                    ]
                } else {
                    let mut expanded_set = y.clone();
                    expanded_set.push(x.clone());
                    vec![
                        NfaBranch::new(Token(x.clone()), Advance, Advance),
                        NfaBranch::new(NotTokenSet(expanded_set), Stop, Advance),
                    ]
                }
            }

            (NotToken(x), TokenSet(y)) => {
                // [!a] X [a,b,c]
                // join on non matching 
                // left on sum
                // right on matching

                let mut v = vec![];

                let mut sum = vec![x];
                sum.extend(y.iter());
                sum.dedup();

                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0].clone()), Advance, Stop));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum.into_iter().cloned().collect()), Advance, Stop));
                }

                let excluded: Vec<_> = y.iter().filter(|c|*c != x).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Advance));
                }

                if y.contains(x) {
                    v.push(NfaBranch::new(Token(x.clone()), Stop, Advance));
                }
                v
            },
            (TokenSet(x), NotToken(y)) => {
                let mut v = vec![];

                let mut sum = vec![y];
                sum.extend(x.iter());
                sum.dedup();

                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0].clone()), Stop, Advance));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum.into_iter().cloned().collect()), Stop, Advance));
                }

                let excluded: Vec<_> = x.iter().filter(|c|*c != y).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Advance));
                }

                if x.contains(y) {
                    v.push(NfaBranch::new(Token(y.clone()), Advance, Stop));
                }
                v
            },
            (TokenSet(x), NotTokenSet(y)) => {
                // [a,b] X [!a,!c] = [a] [b] [!a,!b,!c]
                //  left is matching
                //  things in left not in right
                //  right is dedup sum

                let mut v = vec![];

                let excluded: Vec<_> = y.iter().filter(|c|x.contains(c)).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Stop));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Stop));
                }

                let excluded: Vec<_> = x.iter().filter(|c|!y.contains(c)).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Advance));
                }

                let mut sum = y.clone();
                sum.extend(x.iter());
                sum.dedup();

                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0].clone()), Stop, Advance));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum), Stop, Advance));
                }
                v
            },
            (NotTokenSet(x), TokenSet(y)) => {

                let mut v = vec![];

                let mut sum = x.clone();
                sum.extend(y.iter());
                sum.dedup();

                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0].clone()), Advance, Stop));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum), Advance, Stop));
                }

                let excluded: Vec<_> = y.iter().filter(|c|!x.contains(c)).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Advance));
                }

                let excluded: Vec<_> = x.iter().filter(|c|y.contains(c)).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Stop, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Stop, Advance));
                }
                v

            },
            (NotToken(x), NotTokenSet(y)) => {
                // [!a] X [!b,!c] -> [c, b], [!a,!b,!c], [a]
                //  left is stuff in y not in x
                //  join is dedup sum
                //  right x if x not in y
                let mut v = vec![];

                let mut sum = y.clone();
                sum.push(x.clone());
                sum.dedup();
                
                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0]), Advance, Advance));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum), Advance, Advance));
                }
                
                let excluded: Vec<_> = y.iter().filter(|c| *c != x).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Advance, Stop));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Advance, Stop));
                }

                if !y.contains(x) {
                    v.push(NfaBranch::new(Token(x.clone()), Stop, Advance));
                }
                v
            },
            
            
            (NotTokenSet(x), NotToken(y)) => {
                let mut v = vec![];

                if !x.contains(y) {
                    v.push(NfaBranch::new(Token(y.clone()), Advance, Stop));
                }

                let mut sum = x.clone();
                sum.push(y.clone());
                sum.dedup();
                
                if sum.len() == 1 {
                    v.push(NfaBranch::new(NotToken(sum[0]), Advance, Advance));
                } else if sum.len() > 1 {
                    v.push(NfaBranch::new(NotTokenSet(sum), Advance, Advance));
                }


                let excluded: Vec<_> = x.iter().filter(|c| *c != y).cloned().collect();
                if excluded.len() == 1 {
                    v.push(NfaBranch::new(Token(excluded[0].clone()), Stop, Advance));
                } else if excluded.len() > 1 {
                    v.push(NfaBranch::new(TokenSet(excluded), Stop, Advance));
                }

                v
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
            | (NotTokenSet(_), Question)
            | (NotToken(_), NotToken(_))
            | (NotToken(_), NotTokenSet(_))
            | (NotTokenSet(_), NotToken(_))
            | (NotTokenSet(_), NotTokenSet(_))
            | (NotTokenSet(_), Star) => false,

            (Question, Question) => true,
            (Question, Star) => true,
            (Star, Question) => true,
            (Star, Star) => true,

            (Star, Token(_)) | (Question, Token(_)) => true,
            (Star, NotToken(_)) | (Question, NotToken(_)) => true,
            (Star, NotTokenSet(_)) | (Question, NotTokenSet(_)) => true,
            (Star, TokenSet(_)) | (Question, TokenSet(_)) => true,

            (Token(x), Token(y)) => x != y,
            (Token(x), NotToken(y)) => x == y,
            (Token(x), NotTokenSet(y)) => y.len() == 1 && x == y[0],

            (TokenSet(y), Token(x)) | (Token(x), TokenSet(y)) => !y.iter().any(|c| x == *c),
            (TokenSet(x), TokenSet(y)) => !x.into_iter().any(|c| y.contains(&c)),
            (TokenSet(x), NotToken(y)) => x.len() == 1 && x[0] == y,
            (TokenSet(x), NotTokenSet(y)) => y.len() == 1 && y[0] == x[0],

            (NotToken(x), Token(y)) => x == y,
            (NotToken(x), TokenSet(y)) => y.len() == 1 && y[0] == x,

            (NotTokenSet(x), Token(y)) => x.len() == 1 && y == x[0],
            (NotTokenSet(x), TokenSet(y)) => x.len() == 1 && y.len() == 1 && y[0] == x[0],
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
            | (NotTokenSet(_), Question)
            | (NotToken(_), NotToken(_))
            | (NotToken(_), NotTokenSet(_))
            | (NotTokenSet(_), NotToken(_))
            | (NotTokenSet(_), NotTokenSet(_))
            | (NotTokenSet(_), Star) => return err,

            (Question, Question) => None,
            (Question, Star) => None,
            (Star, Question) => None,
            (Star, Star) => None,

            (Star, Token(n)) | (Question, Token(n)) => Some(NotToken(n)),
            (Star, NotToken(n)) | (Question, NotToken(n)) => Some(Token(n)),
            (Star, NotTokenSet(v)) | (Question, NotTokenSet(v)) => Some(TokenSet(v)),
            (Star, TokenSet(y)) | (Question, TokenSet(y)) => Some(NotTokenSet(y)),

            (Token(x), Token(y)) => {
                if x == y {
                    return err;
                } else {
                    Some(NotTokenSet(vec![x, y]))
                }
            }
            (Token(x), NotToken(y)) => {
                if x == y {
                    None
                } else {
                    return err;
                }
            }
            (Token(x), NotTokenSet(y)) => {
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

            (NotTokenSet(x), Token(y)) => {
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
                Some(NotTokenSet(y))
            }

            (TokenSet(x), TokenSet(y)) => {
                if x.clone().into_iter().any(|c| y.contains(&c)) {
                    return err;
                }
                let mut x = x.clone();
                x.extend(y.iter());
                Some(NotTokenSet(x))
            }
            (TokenSet(x), NotToken(y)) => {
                if x.len() != 1 || x[0] != y {
                    return err;
                }
                None
            }
            (TokenSet(x), NotTokenSet(y)) => {
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
            (NotTokenSet(x), TokenSet(y)) => {
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
            Element::NotTokenSet(v) => !v.into_iter().any(|c| c == l),
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
                Element::NotTokenSet(v) => format!("!`{v:?}`"),
                Element::TokenSet(v) => format!("`{v:?}`"),
            }
        ))
    }
}
