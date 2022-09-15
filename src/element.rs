use std::ops::Add;

use super::*;

const ASCII_TOTAL_CHARS: usize = 128;

#[derive(Debug, Clone, Eq, PartialEq, std::hash::Hash, PartialOrd, Ord)]
pub enum Element {
    TokenSet(BTreeSet<char>),
    NotTokenSet(BTreeSet<char>),
}

impl Element {
    pub fn token(c: char) -> Element {
        Element::TokenSet(FromIterator::from_iter(vec![c]))
    }

    pub fn not_token(c: char) -> Element {
        Element::NotTokenSet(FromIterator::from_iter(vec![c]))
    }

    pub fn tokens(v: &[char]) -> Element {
        Element::TokenSet(FromIterator::from_iter(v.iter().cloned()))
    }

    pub fn not_tokens(v: &[char]) -> Element {
        Element::NotTokenSet(FromIterator::from_iter(v.iter().cloned()))
    }
}

impl ElementalLanguage<Element> for Element {
    fn universal() -> Self {
        Element::NotTokenSet(Default::default())
    }

    fn difference(a: &Element, b: &Element) -> Element {
        use Element::*;

        match (a, b) {
            (TokenSet(x), TokenSet(y)) => {
                // ab - bc = a
                TokenSet(x - y)
            }
            (TokenSet(x), NotTokenSet(y)) => {
                // remove from x any value which is not in y
                TokenSet(x & y)
                // let mut x = x.clone();
                // x.retain(|c| y.contains(c));
                // TokenSet(x).simplify()
            }
            (NotTokenSet(x), TokenSet(y)) => {
                // !a!b abc
                // !a!b!c
                NotTokenSet(x | y)
            }

            (NotTokenSet(x), NotTokenSet(y)) => {
                // things on right not on the left
                // let everything = a,b,c
                // !a = b,c
                // !c = a,b
                // !a - !c = b,c - a,b = c
                // !a + !c = b,c + a,b = a,b,c
                // !a + !c = ? - !c =

                //  !a - !c = c (b,c,d... - a,b,d.. = c)
                TokenSet(y - x)
                // let mut x = x.clone();
                // x.retain(|c| !y.contains(c));
                // NotTokenSet(x).simplify()
            }
        }
    }
}

impl From<char> for Element {
    fn from(c: char) -> Self {
        Element::token(c)
    }
}

impl From<&char> for Element {
    fn from(c: &char) -> Self {
        Element::token(*c)
    }
}

impl Accepts<Element> for Element {
    #[tracing::instrument(ret)]
    fn accepts(&self, l: &Element) -> bool {
        use Element::*;
        match (self, &l) {
            (x, y) if x == *y => true,
            // Are all other ASCII characters specified in the not token set?
            (TokenSet(x), NotTokenSet(y)) => {
                x.len() + y.len() == ASCII_TOTAL_CHARS && x.is_disjoint(y)
            }
            (NotTokenSet(x), TokenSet(y)) => x.is_disjoint(y),
            (NotTokenSet(x), NotTokenSet(y)) => x.is_subset(y),
            _ => false,
        }
    }
}

impl Accepts<&char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &&char) -> bool {
        match self {
            Element::NotTokenSet(v) => !v.iter().any(|c| *c == **l),
            Element::TokenSet(v) => v.iter().any(|c| *c == **l),
        }
    }
}

impl Accepts<char> for Element {
    #[tracing::instrument(skip_all, ret)]
    fn accepts(&self, l: &char) -> bool {
        self.accepts(&l)
    }
}

impl std::iter::Sum for Element {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Element::TokenSet(Default::default()), |acc, cur| {
            Element::add(acc, cur)
        })
    }
}

impl<'a> std::iter::Sum<&'a Element> for Element {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(Element::TokenSet(Default::default()), |acc, cur| {
            Element::add(acc, cur.clone())
        })
    }
}

impl Add for Element {
    type Output = Element;

    fn add(self, rhs: Self) -> Self::Output {
        use Element::*;
        match (&self, &rhs) {
            (TokenSet(x), TokenSet(y)) => TokenSet(x | y),
            (NotTokenSet(y), TokenSet(x)) | (TokenSet(x), NotTokenSet(y)) => {
                let z = y - x;
                if z.is_empty() {
                    Self::universal()
                } else {
                    NotTokenSet(z)
                }
            }
            (NotTokenSet(x), NotTokenSet(y)) => {
                // negation flips the semantics of the set relations of the btreesets
                // !a + !a!b = !a
                // !a!b + !a!b!c = !a!b
                if x.is_subset(y) {
                    self.clone()
                } else if y.is_subset(x) {
                    rhs.clone()
                } else {
                    Self::universal()
                }
            }
        }
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                Element::NotTokenSet(v) => {
                    if v.len() == 1 {
                        format!("!{}", v.iter().next().unwrap())
                    } else {
                        format!("!`{v:?}`")
                    }
                }
                Element::TokenSet(v) => {
                    if v.len() == 1 {
                        format!("{}", v.iter().next().unwrap())
                    } else {
                        format!("`{v:?}`")
                    }
                }
            }
        ))
    }
}
