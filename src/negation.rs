#![allow(unused)]
use std::{
    iter::{Copied, Enumerate},
    ops::RangeFrom,
    slice::Iter,
};

use super::*;
use nom::{
    branch::alt,
    character::complete::one_of,
    combinator::{eof, fail, opt},
    error::{ErrorKind, ParseError},
    multi::{many1, many_till},
    sequence::{delimited, pair, tuple},
    FindToken, IResult, InputIter, InputLength, Needed, Slice,
};

// one parser for rule, enum variant for each rule
enum Transform {
    // this would be cool
    StarAStar(Vec<Elementals>),
    Globulars(usize),
    Questions(usize),
    TokenSeq(Vec<char>),
    NotTokenSeq(Vec<char>),
    QNotLoopQ(Vec<Elementals>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Elementals {
    Tokens(Vec<char>),
    NotTokens(Vec<char>),
    Questions(usize),
    Globulars(usize),
}

// negation transforms must be collectively symmetrical - often individually
fn apply_negation_transformations(input: ElementContainer) -> Vec<Vec<Element>> {
    // 1. run input through parser to produce transform variants
    // 2. for each Transform variant, turn it into a list of paths
    // 3. for each set of paths, produce permutations of inverses? ... or can this occur in the caller
    //    (probably better imo - robert)
    let (_, txms) = transforms(input).unwrap();
    todo!()
}

fn transforms(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    // need to ordered by precedence such that no prefix matches early
    let (rest, (elementals, _)) = many_till(
        alt((
            star_a_star_rule,
            q_to_s_rule,
            flatten_only_questions_rule,
            tokens_rule,
            not_tokens_rule,
        )),
        eof,
    )(input)?;
    Ok((rest, elementals.into_iter().flatten().collect()))
}

fn not_tokens_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, t) = not_tokens(input)?;
    Ok((rest, vec![Transform::NotTokenSeq(t)]))
}

fn tokens_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, t) = tokens(input)?;
    Ok((rest, vec![Transform::TokenSeq(t)]))
}

// `!?` -> `**` (number of questions)
fn q_to_s_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, s) = globulars(input)?;
    Ok((rest, vec![Transform::Globulars(s)]))
}

fn flatten_only_questions_rule(
    input: ElementContainer,
) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, q) = questions(input)?;
    Ok((rest, vec![Transform::Questions(q)]))
}

// *a*
// *a**b*c*
// only contains terms separates by stars and bounded on the outside by stars
fn star_a_star_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, (first, pairs)) = tuple((globulars, many1(pair(tokens, globulars))))(input)?;
    // produce a transform
    let mut ret = vec![Elementals::Globulars(first)];
    for (token, last) in pairs {
        ret.push(Elementals::Tokens(token));
        ret.push(Elementals::Globulars(last));
    }
    Ok((rest, vec![Transform::StarAStar(ret)]))
}
// TODO: ?[TokenSeq self-loop]? ->

// concatenate any initial sequence of Element::TokenSeq() into a single Vec and return
fn tokens(input: ElementContainer) -> IResult<ElementContainer, Vec<char>> {
    let mut chars: Vec<_> = vec![];
    let mut elements = input.v().iter();
    while let Some(Element::Tokens(c)) = elements.next() {
        chars.extend(c);
    }
    if chars.is_empty() {
        return fail(input);
    }
    Ok((ElementContainer(elements.cloned().collect()), chars))
}

fn not_tokens(input: ElementContainer) -> IResult<ElementContainer, Vec<char>> {
    let mut chars: Vec<_> = vec![];
    let mut elements = input.v().iter();
    while let Some(Element::NotTokens(c)) = elements.next() {
        chars.extend(c);
    }
    if chars.is_empty() {
        return fail(input);
    }
    Ok((ElementContainer(elements.cloned().collect()), chars))
}

/// any number of stars or questions such that there is at least one star
/// returns the number of stars and questions
fn globulars(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, (or1, stars, or2))) =
        tuple((opt(stars_or_questions), stars, opt(stars_or_questions)))(input.clone())
    {
        Ok((rest, stars + or1.unwrap_or(0) + or2.unwrap_or(0)))
    } else {
        fail(input)
    }
}

/// returns the number of continuous stars and/or questions in the input
/// This does not need to require the presence of at least one star, the caller
/// should ensure that if required in context
fn stars_or_questions(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, stars)) = many1(our_one_of(&ElementContainer(vec![
        Element::Question,
        Element::Star,
    ])))(input.clone())
    {
        Ok((rest, stars.len()))
    } else {
        fail(input)
    }
}

/// returns the number of stars in the input
fn stars(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, stars)) =
        many1(our_one_of(&ElementContainer(vec![Element::Star])))(input.clone())
    {
        Ok((rest, stars.len()))
    } else {
        fail(input)
    }
}

/// returns the number of consecutive questions in the input
fn questions(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, questions)) =
        many1(our_one_of(&ElementContainer(vec![Element::Question])))(input.clone())
    {
        Ok((rest, questions.len()))
    } else {
        fail(input)
    }
}

// not needless
#[allow(clippy::needless_lifetimes)]
fn our_one_of<'list>(
    list: &'list ElementContainer,
) -> impl Fn(
    ElementContainer,
) -> IResult<ElementContainer, Element, nom::error::Error<ElementContainer>>
       + 'list {
    move |i: ElementContainer| match (&i)
        .iter_elements()
        .next()
        .map(|c| (c, list.find_token(c.clone())))
    {
        Some((c, true)) => {
            let r = i.slice(1..);
            Ok((r, c.clone()))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        ))),
    }
}

enum CheapError {
    Error,
}

impl ParseError<&ElementContainer> for CheapError {
    fn from_error_kind(input: &ElementContainer, kind: ErrorKind) -> Self {
        todo!()
    }

    fn append(input: &ElementContainer, kind: ErrorKind, other: Self) -> Self {
        todo!()
    }
}

#[derive(Clone, Debug)]
struct ElementContainer(Vec<Element>);
impl<'a> ElementContainer {
    fn v(&'a self) -> &'a Vec<Element> {
        &self.0
    }

    fn as_ref(&self) -> &ElementContainer {
        self
    }
}

impl nom::Slice<RangeFrom<usize>> for ElementContainer {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        ElementContainer(self.v()[range].to_vec())
    }
}

impl FindToken<Element> for &ElementContainer {
    fn find_token(&self, token: Element) -> bool {
        self.v().iter().any(|i| i == &token)
    }
}

impl InputLength for &ElementContainer {
    fn input_len(&self) -> usize {
        self.v().len()
    }
}

impl InputLength for ElementContainer {
    fn input_len(&self) -> usize {
        self.v().len()
    }
}

impl<'a> InputIter for &'a ElementContainer {
    type Item = &'a Element;

    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Iter<'a, Element>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.v().iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.v().iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.v().iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.v().len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.v().len()))
        }
    }
}
