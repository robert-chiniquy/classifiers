use super::*;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    // Token(char),
    // NotToken(char),
    TokenSeq(Vec<char>),
    Question,
    Star,
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
                Element::Question => "?".to_string(),
                Element::Star => "*".to_string(),
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
            }
        ))
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: Element) -> bool {
        match (self, &l) {
            (x, y) if x == y => true,
            (Element::Question, Element::Question) => true,
            (_, Element::Star) => false,
            (Element::Star, _) => true,
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

// TODO: FIXME: terminal_on needs to decompose seqs of len > 1 into component token/not token to call this method
impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        match self {
            Element::Question => true,
            Element::Star => true,
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
            Element::Question => true,
            Element::Star => true,
            Element::TokenSeq(n) if n.len() == 1 => n[0] == l,
            Element::NotTokenSeq(n) if n.len() == 1 => n[0] != l,
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
        // use Elementals::*;

        // let mut parts: Vec<Elementals> = Default::default();
        // let mut buf: Elementals = Tokens(Default::default());

        // for e in self.iter() {
        //     match e {
        //         // Element::Token(t) => {
        //         //     if let Tokens(ref mut v) = buf {
        //         //         v.push(*t);
        //         //         continue;
        //         //     }
        //         //     parts.push(buf.clone());
        //         //     buf = Tokens(vec![*t]);
        //         // }
        //         Element::Star => {
        //             if let Globulars(ref mut n) = buf {
        //                 *n += 1;
        //                 continue;
        //             }

        //             if let Questions(n) = buf {
        //                 buf = Globulars(n + 1);
        //                 continue;
        //             }
        //             // do look ahead...
        //             parts.push(buf.clone());
        //             buf = Globulars(1);
        //         }
        //         Element::Question => match buf {
        //             Questions(ref mut n) | Globulars(ref mut n) => {
        //                 *n += 1;
        //                 continue;
        //             }
        //             _ => {
        //                 parts.push(buf.clone());
        //                 buf = Questions(1);
        //             }
        //         },
        //         Element::TokenSeq(a) => {
        //             if let Tokens(ref mut b) = buf {
        //                 b.extend(a.clone());
        //                 continue;
        //             }
        //             // do look ahead...
        //             // ...
        //             parts.push(buf.clone());
        //             buf = Tokens(a.clone());
        //         }
        //         Element::NotToken(a) => {
        //             if let NotTokens(ref mut b) = buf {
        //                 b.push(*a);
        //                 continue;
        //             }
        //             parts.push(buf.clone());
        //             buf = NotTokens(vec![*a]);
        //         }
        //         Element::NotTokenSeq(a) => {
        //             if let NotTokens(ref mut b) = buf {
        //                 b.extend(a.clone());
        //                 continue;
        //             }
        //             parts.push(buf.clone());
        //             buf = NotTokens(a.clone());
        //         }
        //     }
        // }

        // parts.push(buf.clone());

        // let mut new_parts = vec![];
        // enum Mode {
        //     Start,
        //     PartOfGlobSeq,
        // }
        // let mut mode = Mode::Start;
        // let mut working_glob = Elementals::Globulars(0);
        // for (i, part) in parts.iter().enumerate() {
        //     match part {
        //         Tokens(_) | NotTokens(_) => {
        //             new_parts.push(part);
        //             match mode {
        //                 Mode::Start => (),
        //                 Mode::PartOfGlobSeq => {
        //                     parts.push(working_glob.clone());
        //                     working_glob = Elementals::Globulars(0);
        //                     mode = Mode::Start;
        //                 },
        //             }
        //         },
        //         Questions(q) => match mode {
        //             Mode::Start => {
        //                 // Look Ahead!
        //                 let mut j = i + 1;
        //                 // let mut working_question = 0;
        //                 while let Some(part) = parts.get(j) {
        //                     j += 1;
        //                     match part {
        //                         Questions(_) => (),
        //                         Globulars(_) => {
        //                             mode = Mode::PartOfGlobSeq;
        //                             break;
        //                         }
        //                         _ => {
        //                             break;
        //                         }
        //                     }
        //                 }

        //                 match mode {
        //                     Mode::Start => {
        //                         new_parts.push(part);
        //                     },
        //                     Mode::PartOfGlobSeq => {
        //                         if let Globulars(ref mut n) = working_glob {
        //                             *n += q;
        //                         }
        //                     },
        //                 }
        //             }
        //             Mode::PartOfGlobSeq => {
        //                 if let Globulars(ref mut n) = working_glob {
        //                     *n += q;
        //                 }
        //             }
        //         },

        //         Globulars(q) => {
        //             if let Globulars(ref mut n) = working_glob {
        //                 *n += q;
        //             }
        //             mode = Mode::PartOfGlobSeq;

        //         },
        //     }
        // }



        // concatenate adjacent parts where correct to

        // generate combinations of inverses
        // for &p1 in parts {
        //     for &p2 in parts {

        //     }
        // }
        // *** -> ??, ?
        // ab -> !ab, ?, ***
        // [**?] => len(s) -1 * '?'
        // ab*c -> [!ab, *, ?], [?, ?, ?], [??*!c]
        // ab -> [a, b] -> [!a, ?], [?, !b] + [?], [***] OR [!ab], [?], [***]
        // a*b -> [a, *, b]
        // a**b -> [a, **, b]
        // a?*b -> [a, ?*, b]
        // a?b -> [a, ?, b]
        // !a -> [a]

        // [!a, ?], [?, !b], [?], [*, *, *]
        todo!()
    }
}

// fn flip(v: Vec<Element>) -> Vec<Vec<Element>> {
//     if matches!(v[0], Element::Token(_)) {
//         if v.len() == 1 {
//             if let Element::Token(e) = v[0] {
//                 return vec![vec![Element::NotToken(e)]];
//             } else {
//                 panic!();
//             }
//         }
//         return vec![vec![Element::NotTokenSeq(
//             v.iter()
//                 .flat_map(|e| {
//                     if let Element::Token(e) = e {
//                         Ok(e)
//                     } else {
//                         Err(())
//                     }
//                 })
//                 .cloned()
//                 .collect(),
//         )]];
//     }

//     if matches!(v[0], Element::Question) {
//         if v.len() == 1 {
//             return vec![vec![Element::Star, Element::Star]];
//         }
//         //  ?** -> ??, ?
//         let len = v.len();
//         if v.into_iter().any(|e| matches!(e, Element::Star)) {
//             let mut r: Vec<_> = Default::default();
//             for i in 1..len - 1 {
//                 r.push(vec![Element::Question; i]);
//             }
//             return r;
//         }
//         // ?? -> ***
//         return vec![vec![Element::Star; len + 1]];
//     }

//     if matches!(v[0], Element::Star) {
//         if v.len() == 1 {
//             return Default::default();
//         }
//         //  *?* -> ??, ?
//         let mut r: Vec<_> = Default::default();
//         for i in 1..v.len() - 1 {
//             r.push(vec![Element::Question; i]);
//         }
//         return r;
//     }

//     if matches!(v[0], Element::NotToken(_)) {
//         if v.len() == 1 {
//             if let Element::NotToken(e) = v[0] {
//                 return vec![vec![Element::Token(e)]];
//             } else {
//                 panic!();
//             }
//         }
//         return vec![vec![Element::TokenSeq(
//             v.iter()
//                 .flat_map(|e| {
//                     if let Element::NotToken(e) = e {
//                         Ok(e)
//                     } else {
//                         Err(())
//                     }
//                 })
//                 .cloned()
//                 .collect(),
//         )]];
//     }
//     // unreachable!
//     unreachable!();
// }

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
                        NfaBranch::new(b.clone(), Drop, Advance),
                    ]
                } else {
                    diverge(a, b)
                }
            }
            (NotTokenSeq(x), Question) => {
                if x.len() == 1 {
                    vec![
                        NfaBranch::new(a.clone(), Advance, Advance),
                        NfaBranch::new(Element::TokenSeq(x.clone()), Drop, Advance),
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
                        NfaBranch::new(a.clone(), Advance, Drop),
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
                        NfaBranch::new(Question, Advance, Drop),
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
                        NfaBranch::new(Element::TokenSeq(y.clone()), Advance, Drop),
                        NfaBranch::new(b.clone(), Advance, Advance),
                    ]
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
