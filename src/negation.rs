#![allow(unused)]
use std::{
    collections::HashSet,
    fmt::Debug,
    iter::{Copied, Enumerate},
    ops::RangeFrom,
    slice::Iter,
};

#[cfg(test)]
use crate::tests::setup;

use super::*;
use nom::{
    branch::alt,
    character::complete::one_of,
    combinator::{eof, fail, opt},
    error::{ErrorKind, ParseError},
    multi::{many1, many_till},
    sequence::{delimited, pair, terminated, tuple},
    FindToken, IResult, InputIter, InputLength, Needed, Slice,
};

// anything not captured by a larger rule is evaluated piecewise
// negate(?a) -> (!?)a, ?(!a), !?!a, ?
// !? -> ** ... !a -> !a
// **a, ?!a, **!a -> ?!a, ***
// negate(*a) -> *(!a), ?
// negate(*) -> !* ... but is this necessary? only if every case cannot be captured in a larger rule
// ... (!*)a, *!a, !*!a, ?
// drop !* from results -> .. *!a, !a, ?
// negate(?a*) -> (!?)a*, ?(!a)*, ?a(!*), ?(!a)!*, !?(!a)*, (!?)a!*
// drop !* from results -> (!?)a*, ?(!a)*, ?a, ?(!a), !?(!a)*, (!?)a
// !? -> ** ... -> **a*, ?(!a)*, ?a, ?(!a), **(!a)*, **a
// there are intersections between the input: ?a* and the above

// one parser for rule, enum variant for each rule
#[derive(Eq, PartialEq, Clone, Debug)]
enum Transform {
    StarAStar(Vec<Elementals>),
    Globulars(usize),
    Questions(usize),
    TokenSeq(Vec<char>),
    NotTokenSeq(Vec<char>),
    QNotLoopQ(Vec<Elementals>),
    FrontAnchoredTokens(Vec<Elementals>),
    EndAnchoredTokens(Vec<Elementals>),
}

#[test]
fn test_negation() {
    setup();
    use Element::*;
    let input = vec![Star, Tokens(vec!['a'])];
    let negative = negation_of(input.clone());
    pretty_print_path(&input, &negative);

    let input = vec![Star, Tokens(vec!['a']), Star];
    let negative = negation_of(input.clone());
    pretty_print_path(&input,&negative);

    // ?a -> ?, ?(not a), ??*
    let input = vec![Question, Tokens(vec!['a'])];
    let negative = negation_of(input.clone());
    pretty_print_path(&input,&negative);

    for p in negative {
        assert!(p != vec![Star, Star, NotTokens(vec!['a'])]);
    }

    let input = vec![Question, Tokens(vec!['a']), Star, Star];
    let negative = negation_of(input.clone());
    pretty_print_path(&input,&negative);

    for p in negative {
        assert!(p != vec![Star, Star, NotTokens(vec!['a']), Star, Star]);
    }
}

// 2 methods which could call apply_negation_txms
// - returns a Vec<Vec<Element>>, a set of paths to union into a graph
// #[tracing::instrument(ret)]
pub fn negation_of(input: Vec<Element>) -> Vec<Vec<Element>> {
    let sequence_of_choices = interpret_negation_rules(ElementContainer(input.clone()));
    let mut paths = visit_choices(&input, &sequence_of_choices);
    // need to correct for length
    // for finite input, all longer paths
    // for looping input, all shorter paths
    // detect looping in input, then add to paths based on input.len()
    let min_length = element_sequence_minimum_unit_length(&input);
    match input
        .iter()
        .any(|e| matches!(e, Element::Star | Element::LoopNotTokens(_)))
    {
        true => {
            for i in 1..min_length {
                paths.push(vec![Element::Question; i]);
            }
        }
        false => paths.push(vec![
            // could be questions up front instead
            Element::Star;
            min_length + 1
        ]),
    }
    paths
}

#[test]
fn test_visit_choices() {
    setup();
    use Element::*;
    let path = vec![Tokens(vec!['a']), NotTokens(vec!['a'])];
    let p = visit_choices(&path, &[HashSet::from_iter(vec![path.clone()])]);
    assert!(!p.is_empty());
}

// #[tracing::instrument(ret)]
fn visit_choices(
    original_input: &Vec<Element>,
    choices: &[HashSet<Vec<Element>>],
) -> Vec<Vec<Element>> {
    let mut paths = vec![];
    if let Some(current) = choices.first() {
        let mut tail = visit_choices(original_input, &choices[1..]);
        if tail.is_empty() {
            paths.extend(current.iter().cloned().collect::<Vec<_>>());
            return paths;
        }
        // possible globulars condition? current len == 1 and the item is an empty vec?
        // not the only item in the hashset no, so the special value is an empty vec in the hashset
        // no not an empty vec in the hashset, the special value is an item in the vec
        // todo figure out
        for alternative in current {
            paths.extend(
                tail.iter()
                    .map(|i| {
                        let mut a = alternative.clone();
                        a.extend(i.clone());
                        a
                    })
                    .collect::<Vec<_>>(),
            );
        }
    }
    paths
        .into_iter()
        .filter(|p| p != original_input)
        .clone()
        .collect()
}

// - returns an NFA
pub fn nfa_negation_of(input: Vec<Element>) {
    todo!()
}

// negation transforms must be collectively symmetrical - often individually
// entry -path-> Reject
// becomes ... entry -[some paths]-> Accept
// Produce paths which describe every Accept state which is not the Reject path
/// 1. run input through parser to produce transform variants
/// 2. for each Transform variant, turn it into a list of paths
// 3. for each set of paths, produce permutations of inverses?
//    ... or can this occur in the caller   (probably better imo, can make subgraphs converge - robert)
/// Caller must permute inverses of returned components and connect into graphs
// The innermost item is an expansion into a partial path (Vec<Element>)
// The inner hashset is a disjunction/collection of paths for any given rule where the paths are
// mutually exclusive components of the desired output
// The outer Vector is the sequential collection of rules
// Compose the sequence of hashsets into a sequence of choices of path fragments from the hashset
// #[tracing::instrument(ret)]
fn interpret_negation_rules(input: ElementContainer) -> Vec<HashSet<Vec<Element>>> {
    // todo: need a way to handle an error here
    // ?
    let (rest, txms) = transforms(input).unwrap();
    // Consider returning an error here?
    debug_assert!(
        rest.v().is_empty(),
        "🌮🌮🌮🌮🌮 Did not consume everything {rest:?}"
    );
    // Vec of Vec of Element
    let mut rules: Vec<HashSet<Vec<Element>>> = Default::default();
    let multiple_txns = &txms.len() > &1;

    for txm in txms {
        // println!("txm: {txm:?}");
        let mut set = match txm.clone() {
            // [ab, a?, ?b, ??]
            // negate(ab) -> [a(not b), (not a)b, (not a)(not b), ?] -> [a(not b), (not a)?, ?],
            // [a!b, !a?, ab]
            // Matt prefers to not include the shorter ?-style matches for TokenSeq in results currently
            // negate(ab**) -º-> [?**, ]
            // negate(a**) -> union([(not a)**, (a(not **) -> a?), ((not a)(not **) -> (not a)?), ??, ?])
            //   (more rejection logic possible -> [(not a)**, ??, ?])
            // negate(a*) -> [(not a)*, (a(not *) -> a), ?]
            // negate((not *)) -> union([]) (srsly)
            // negate(***) -> [??, ?]
            Transform::Globulars(n) => {
                // before doing this rule, matt wants to capture a* in a rule (different from star a star)
                // insert ?..? for the shorter elements
                let mut rule: HashSet<_> = Default::default();
                for i in 1..n {
                    rule.insert(vec![Element::Question; i]);
                }
                // is the empty value here for an a* situation?
                // .. otherwise dependent upon the larger context where this globulars was read

                // rule.insert(vec![Element::Tokens(vec![' '])]);
                rule
            }
            Transform::Questions(n) => {
                //  ??? -> [****, ??, ?]
                let mut rule: HashSet<_> = Default::default();
                rule.insert(vec![Element::Star; n + 1]);
                for qs in 1..n - 1 {
                    rule.insert(vec![Element::Question; qs]);
                }
                rule
            }
            Transform::TokenSeq(v) => HashSet::from_iter(vec![vec![Element::NotTokens(v)]]),
            Transform::NotTokenSeq(v) => HashSet::from_iter(vec![vec![Element::Tokens(v)]]),
            Transform::StarAStar(v) => {
                let mut items = vec![];
                for e in v {
                    match e {
                        Elementals::Tokens(t) => items.push(Element::LoopNotTokens(t)),
                        Elementals::Globulars(g) => {
                            for i in 1..g+1 {
                                items.push(Element::Question);
                            }
                        }
                        _ => unreachable!(),
                    };
                }
                HashSet::from_iter(vec![items])
            }
            Transform::QNotLoopQ(v) => {
                // ![a*] -> [!a*], [a!*], [!a!*] -> [!a*], [a], [!a]
                // ![a**] -> !a** , a!(**), !a!(**) -> !a**, a?, a, !a?, !a
                let mut items = vec![];
                for e in v {
                    match e {
                        Elementals::LoopNotTokens(t) => items.push(Element::Tokens(t)),
                        Elementals::Questions(g) => {
                            for i in 1..g {
                                items.push(Element::Star);
                            }
                        }
                        _ => unreachable!(),
                    };
                }
                HashSet::from_iter(vec![items])
            }
            Transform::FrontAnchoredTokens(elementals)
            | Transform::EndAnchoredTokens(elementals) => {
                // ?a?b -> ?a?!b, ?!a?!b, ?!a?b, {???, ??, ?} -> ???!b, ?!a??
                // ?ac?b -> ?ac?!b, ?(!ac)?!b, ?(!ac)?b -> ???!b, ?(!ac)??
                // ?a?b?c ->
                // Flip any one token and accept all others
                // as if any one is missing, the expression does not match
                let mut rule: HashSet<_> = Default::default();
                // produce all valid permutations of negations of the input
                // for each token, produce a flipped rule
                // for each elemental, if a token,
                // produce a path from input which flips it and otherwise is all ?
                let length = elementals_fixed_width_sequence_unit_length(&elementals);
                for (flip_index, to_flip) in elementals.iter().enumerate() {
                    if !matches!(to_flip, Elementals::Tokens(_) | Elementals::NotTokens(_)) {
                        // do not add a path
                        continue;
                    }
                    // create the path here
                    let mut new_path = vec![];
                    for (i, e) in elementals.iter().enumerate() {
                        match e {
                            Elementals::Tokens(s) if i == flip_index => {
                                new_path.push(Element::NotTokens(s.clone()))
                            }
                            Elementals::Tokens(s) if i != flip_index => {
                                new_path.extend(vec![Element::Question; s.len()])
                            }
                            Elementals::NotTokens(s) if i == flip_index => {
                                new_path.push(Element::Tokens(s.clone()))
                            }
                            Elementals::NotTokens(s) if i != flip_index => {
                                new_path.extend(vec![Element::Question; s.len()])
                            }
                            Elementals::Questions(i) => {
                                new_path.extend(vec![Element::Question; *i])
                            }
                            _ => unreachable!(),
                        }
                    }
                    rule.insert(new_path);
                }
                rule
            }
        };
        if multiple_txns {
            // This line adds the original value for combinatorics with other negations
            set.insert(txm.elements());    
        }
        rules.push(set);
    }
    // filter out empty hashsets
    rules.into_iter().filter(|h| !h.is_empty()).collect()
}

// #[tracing::instrument(ret)]
fn transforms(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let mut input = input;
    let mut working_result: Vec<Transform> = vec![];
    if let Ok((rest, front)) = front_rule(input.clone()) {
        working_result.extend(front);
        input = rest;
    }
    if input.v().is_empty() {
        return Ok((input, working_result));
    }
    // match the end of input (end_rule requires eof)
    if let Ok((rest, (unanchored, end))) = many_till(opt(unanchored_rules), end_rule)(input.clone())
    {
        if let Some(unanchored) = unanchored.into_iter().collect::<Option<Vec<_>>>() {
            working_result.extend(unanchored.into_iter().flatten());
        }
        working_result.extend(end);
        return Ok((rest, working_result));
    }
    // insist on consuming the rest of the input with unanchored_rules
    if let Ok((rest, (unanchored, _))) = many_till(unanchored_rules, eof)(input.clone()) {
        working_result.extend(unanchored.into_iter().flatten());
        return Ok((rest, working_result));
    }
    // the only place an error is returned
    fail(input)
}

fn unanchored_rules(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    // need to ordered by precedence such that no prefix matches early
    alt((
        star_a_star_rule,
        q_not_loop_q_rule,
        q_to_s_rule,
        flatten_only_questions_rule,
        tokens_rule,
        not_tokens_rule,
    ))(input)
}

// rationale: negate(?a*) -> **!a* is wrong
// need to capture any token which is anchored to the start or end by a fixed number of chars
// ^ ... ?a.. also ?a?a ?ab ?ab?ab
// should not capture the terminating ?* of ?a?*
// must end in a token/nottoken
// may need an equivalent rule for the end of the string
// produce the same anchorings of ? with the combinations of negations of the tokens,
// .. not including the input
// also needs to handle not token
// assume front of input
fn front_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, (v, w)) = pair(
        alt((questions, tokens_elementals, not_tokens_elementals)),
        alt((tokens_elementals, not_tokens_elementals)),
    )(input)?;

    let ret = Vec::from_iter(v.into_iter().chain(w));
    Ok((rest, vec![Transform::FrontAnchoredTokens(ret)]))
}

// end rule does not need to end in a token, but needs to begin with a token
fn end_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, (v, w)) = terminated(
        pair(
            alt((tokens_elementals, not_tokens_elementals)),
            alt((questions, tokens_elementals, not_tokens_elementals)),
        ),
        eof,
    )(input)?;

    let ret = Vec::from_iter(v.into_iter().chain(w));
    Ok((rest, vec![Transform::EndAnchoredTokens(ret)]))
}

fn q_not_loop_q_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, (first, pairs)) =
        tuple((questions_count, many1(pair(not_loops, questions_count))))(input)?;
    // produce a transform
    let mut ret = vec![Elementals::Questions(first)];
    for (token, last) in pairs {
        ret.push(Elementals::LoopNotTokens(token));
        ret.push(Elementals::Questions(last));
    }
    Ok((rest, vec![Transform::QNotLoopQ(ret)]))
}

fn not_tokens_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, t) = not_tokens(input)?;
    Ok((rest, vec![Transform::NotTokenSeq(t)]))
}

fn tokens_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, t) = tokens(input)?;
    Ok((rest, vec![Transform::TokenSeq(t)]))
}

// `!?` -> `**` (number of questions)??
// ?* -> **
// #[tracing::instrument(ret)]
fn q_to_s_rule(input: ElementContainer) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, s) = globulars(input)?;
    Ok((rest, vec![Transform::Globulars(s)]))
}

fn flatten_only_questions_rule(
    input: ElementContainer,
) -> IResult<ElementContainer, Vec<Transform>> {
    let (rest, q) = questions_count(input)?;
    Ok((rest, vec![Transform::Questions(q)]))
}

#[test]
fn test_tiny_star_a_star() {
    setup();

    // let input = vec![Star, Tokens(vec!['a']), Star];
    // let negative = negation_of(input.clone());
    // pretty_print_path(&input,&negative);

    use Element::*;
    let input = ElementContainer(vec![Star]);

    let r = tuple((globulars,))(input.clone());

    assert!(r.is_ok(), "{r:?}");

    let input = ElementContainer(vec![Tokens(vec!['a'])]);

    let r = tokens(input.clone());
    assert!(r.is_ok(), "{r:?}");

    let input = ElementContainer(vec![Tokens(vec!['a']), Star]);

    let r = pair(tokens, globulars)(input.clone());
    assert!(r.is_ok(), "{r:?}");

    let input = ElementContainer(vec![Star, Tokens(vec!['a']), Star]);

    let r = star_a_star_rule(input.clone());
    assert!(r.is_ok(), "{r:?}");

    let (rest, txms) = r.unwrap();
    println!("{:?}", txms);

    assert!(rest.v().is_empty(), "has stuff: {rest:?}");
}

#[test]
fn test_star_a_star() {
    setup();

    use Element::*;
    let input = ElementContainer(vec![Star, Tokens(vec!['a']), Star]);
    let (rest, txms) = transforms(input.clone()).unwrap();

    println!("{:?}", txms);
    assert!(rest.v().is_empty(), "has stuff: {rest:?}");

    let r = star_a_star_rule(input.clone());
    assert!(r.is_ok(), "{r:?}");

    let (rest, txms) = r.unwrap();
    println!("{:?}", txms);


    let stuff = negation_of(input.v().clone());
    pretty_print_path(&input.v(), &stuff);
    assert!(rest.v().is_empty(), "has stuff: {rest:?}");
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

fn tokens_elementals(input: ElementContainer) -> IResult<ElementContainer, Vec<Elementals>> {
    let (rest, tokens) = tokens(input)?;
    Ok((rest, vec![Elementals::Tokens(tokens)]))
}

// concatenate any initial sequence of Element::TokenSeq() into a single Vec and return
fn tokens(input: ElementContainer) -> IResult<ElementContainer, Vec<char>> {
    let mut chars: Vec<_> = vec![];
    let mut elements = input.v().iter().peekable();
    while let Some(Element::Tokens(c)) = elements.peek() {
        chars.extend(c);
        elements.next(); // consume
    }
    if chars.is_empty() {
        return fail(input);
    }
    Ok((ElementContainer(elements.cloned().collect()), chars))
}

// Combine any consecutive NotLoops
fn not_loops(input: ElementContainer) -> IResult<ElementContainer, Vec<char>> {
    let mut chars: Vec<_> = vec![];
    let mut elements = input.v().iter();
    while let Some(Element::LoopNotTokens(c)) = elements.next() {
        chars.extend(c);
    }
    if chars.is_empty() {
        return fail(input);
    }
    Ok((ElementContainer(elements.cloned().collect()), chars))
}

fn not_tokens_elementals(input: ElementContainer) -> IResult<ElementContainer, Vec<Elementals>> {
    let (rest, tokens) = not_tokens(input)?;
    Ok((rest, vec![Elementals::NotTokens(tokens)]))
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
// #[tracing::instrument(ret)]
fn globulars(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, (or1, stars, or2))) =
        tuple((opt(questions_count), stars, opt(stars_or_questions)))(input.clone())
    {
        let number = stars + or1.unwrap_or(0) + or2.unwrap_or(0);

        Ok((rest, number))
    } else {
        fail(input)
    }
}

/// returns the number of continuous stars and/or questions in the input
/// This does not need to require the presence of at least one star, the caller
/// should ensure that if required in context
// #[tracing::instrument(ret)]
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
// #[tracing::instrument(ret)]
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
fn questions_count(input: ElementContainer) -> IResult<ElementContainer, usize> {
    if let Ok((rest, questions)) =
        many1(our_one_of(&ElementContainer(vec![Element::Question])))(input.clone())
    {
        Ok((rest, questions.len()))
    } else {
        fail(input)
    }
}

// directly maps questions to questions
fn questions(input: ElementContainer) -> IResult<ElementContainer, Vec<Elementals>> {
    if let Ok((rest, questions)) =
        many1(our_one_of(&ElementContainer(vec![Element::Question])))(input.clone())
    {
        Ok((rest, vec![Elementals::Questions(questions.len())]))
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
        e => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Fail,
        ))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Elementals {
    Tokens(Vec<char>),
    NotTokens(Vec<char>),
    LoopNotTokens(Vec<char>),
    Questions(usize),
    Globulars(usize),
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
    // #[tracing::instrument(ret)]
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

fn pretty_print_path(original: &Vec<Element>, paths: &[Vec<Element>]) {
    let original = original.iter().map(|e| e.to_string()).collect::<String>();
    let mut paths = paths.iter().map(|p| p.iter().map(|e| e.to_string()).collect::<String>()).collect::<Vec<_>>();
    paths.sort_by(|a, b| a.len().partial_cmp(&b.len()).unwrap());
    println!("\n{}\n{}\n{}\n", original, "-".repeat(original.len()), paths.join("\n"));
}

// #[tracing::instrument(ret)]
fn element_sequence_minimum_unit_length(input: &[Element]) -> usize {
    input
        .iter()
        .map(|i| match i {
            Element::Question => 1,
            Element::Star => 1,
            Element::Tokens(s) => s.len(),
            Element::NotTokens(s) => s.len(),
            Element::LoopNotTokens(s) => s.len(),
        })
        .sum()
}

fn elementals_fixed_width_sequence_unit_length(input: &[Elementals]) -> usize {
    input
        .iter()
        .map(|i| match i {
            Elementals::Questions(i) => *i,
            Elementals::Tokens(s) | Elementals::NotTokens(s) => s.len(),
            Elementals::LoopNotTokens(_) | Elementals::Globulars(_) => unreachable!(),
        })
        .sum()
}

impl Transform {
    fn elements(&self) -> Vec<Element> {
        use Element::*;
        match self {
            Transform::StarAStar(v) => v
                .iter()
                .flat_map(|e| match e {
                    Elementals::Tokens(v) => vec![Tokens(v.clone())],
                    Elementals::NotTokens(v) => vec![NotTokens(v.clone())],
                    Elementals::LoopNotTokens(v) => vec![LoopNotTokens(v.clone())],
                    Elementals::Questions(n) => vec![Question; *n],
                    Elementals::Globulars(n) => vec![Star; *n],
                })
                .collect(),
            Transform::QNotLoopQ(v) => v
                .iter()
                .flat_map(|e| match e {
                    Elementals::LoopNotTokens(v) => vec![LoopNotTokens(v.clone())],
                    Elementals::Questions(n) => vec![Question; *n],
                    _ => unreachable!(),
                })
                .collect(),
            Transform::Globulars(n) => vec![Star; *n],
            Transform::Questions(n) => vec![Question; *n],
            Transform::TokenSeq(v) => vec![Tokens(v.clone())],
            Transform::NotTokenSeq(v) => vec![NotTokens(v.clone())],
            Transform::FrontAnchoredTokens(v) | Transform::EndAnchoredTokens(v) => v
                .iter()
                .flat_map(|e| match e {
                    Elementals::Tokens(v) => vec![Tokens(v.clone())],
                    Elementals::NotTokens(v) => vec![NotTokens(v.clone())],
                    Elementals::LoopNotTokens(v) => vec![LoopNotTokens(v.clone())],
                    Elementals::Questions(n) => vec![Question; *n],
                    Elementals::Globulars(n) => vec![Star; *n],
                })
                .collect(),
        }
    }
}
