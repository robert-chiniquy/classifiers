#![cfg(test)]
use super::*;

#[test]
fn test_accepts() {
    let d1: Nfa<NfaNode<()>, NfaEdge<Element>> = Nfa::from_language(str_to_chars("abc"), ());
    assert!(d1.accepts_string("abc"));
    assert!(!d1.accepts(&str_to_chars("ab")));
    assert!(!d1.accepts(&str_to_chars("a")));
    assert!(!d1.accepts(&str_to_chars("abcc")));
}

#[test]
fn test_graphviz_one() {
    use std::io::Write;

    test_setup();
    // let asdf: Nfa<NfaNode<()>, NfaEdge<Element>> = Nfa::from_language(str_to_chars("XYZ"), ());
    let lhs = Classifier::Any(BTreeSet::from_iter([
        Classifier::Literal(str_to_chars("ABC")),
        Classifier::Literal(str_to_chars("XYZ")),
    ]));

    // not ABC and not XYZ
    let rhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("ABC"))),
        Classifier::not(Classifier::Literal(str_to_chars("XYZ"))),
    ]));

    let c1: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile::<Element, (), char>(&lhs, ());
    let c2: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile(&rhs, ());

    let cat = c1.concatenate(&c2);

    let g = graphviz_wrap(cat.graphviz(), "c1, c2");
    let mut output = std::fs::File::create("./one.dot").unwrap();
    assert!(output.write_all(g.as_bytes()).is_ok());

    let _adsf: Vec<char> = "OOABC".chars().collect();

    assert_eq!(Relation::Disjoint, lhs.relation::<Element, char>(&rhs));
}

#[test]
fn test_intersection_of_heterogenous_states() {
    #![allow(unused)]
    let everything_but_tacos = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("tacos"))),
        Classifier::Literal(str_to_chars("*")),
    ]));

    let all_ts_but_tacos = Classifier::And(BTreeSet::from_iter([
        Classifier::Literal(str_to_chars("t*")),
        Classifier::not(Classifier::Literal(str_to_chars("tacos"))),
    ]));

    // consider all ts but tacos with this thought:
    // matt suggests that in walking an NFA if you encounter a rejection state on i.e. taco while your
    // path does NOT terminate on that rejection state, you interpret it as an accepting state
    // If my input query is "fish" and I walk the whole graph and I encounter a rejecting state on tacos,
    // i can't treat it as affirmation? fish is not t*

    let all_ts_but_q = Classifier::And(BTreeSet::from_iter([
        Classifier::Literal(str_to_chars("t*")),
        Classifier::not(Classifier::Literal(str_to_chars("*q"))),
    ]));

    let combo = Classifier::Any(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("tacos"))),
        Classifier::Literal(str_to_chars("*")),
        Classifier::And(BTreeSet::from_iter([
            Classifier::Literal(str_to_chars("t*")),
            Classifier::not(Classifier::Literal(str_to_chars("*q"))),
        ])),
    ]));

    // matt suggests that in walking an NFA if you encounter a rejection state on i.e. taco while your
    // path does NOT terminate on that rejection state, you interpret it as an accepting state

    // matt rephrases this to say that a rejection state is not a rejection state, it is an
    // acceptance state of anything which is not the path leading to the rejection

    // * and taco are intersecting paths
    // an intersection function could logically evaluate and identify the intersection of these paths
    // without regard to the type of the terminal state (accepting or rejecting) ..
    // then, logically, the remaining intersecting path (taco) must have a rejecting state in the result as it does in the input
    // - observe that this understanding of intersection here is in line with the heuristic that
    //   And(something, *) = something
    // this result for intersection relies on an interpretation of a rejecting state in an NFA as matt described above
    // how does it handle a smaller subset of * containing taco ?
    // ?

    let c = Classifier::compile::<Element, (), char>(&everything_but_tacos, ());
}

#[test]
fn test_terminal_on() {
    // if the higher combinator were And, this would include nothing
    // but since it is Any, Robert thinks it includes everything
    // Robert's original heuristic / convention was that encountering a
    // reject state on any path supersedes any accept state on that path,
    // using that rule, an And here of these two values would be an empty set
    // minimization would be nice ...
    // we don't have a single graph semantics which works with both And and Any
    // let zzz = Classifier::Any(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal(str_to_chars("AB"))),
    //     Classifier::Literal(str_to_chars("AB")),
    // ]));
    // Matt is suggesting a Element::NotToken, doesn't need to be a public interface
    let zzz = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("A*"))),
        Classifier::Literal(str_to_chars("*B")),
    ]));

    // only 1 char
    let _ = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("**"))),
        Classifier::Literal(str_to_chars("*")),
    ]));

    let c = Classifier::compile::<Element, (), char>(&zzz, ());
    write_graph(c.graphviz(), "terminal_on_zzz1.dot");
    assert!(c.accepts_string("BB"));
    // how do we ask c if it accepts any state which is not AB
    // (c could also accept AB in addition to some other state)
    // - c minus an NFA which accepts AB
    // the method to call to do this (like ... .accepts_inverse()) should:
    // - 1. c.union(nfa which accepts AB)
    // - 2. look for paths on the left but not on the right
    // or
    // walk c taking any edge which is not A and look for an accept
    // walk c taking any edge which is not AB and look for an accept
    //
}

#[test]
fn test_rejection() {
    // 2 NFAs, with all rejecting states, having 1 edge in common

    let lhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("AB"))),
        Classifier::not(Classifier::Literal(str_to_chars("XY"))),
    ]));

    let rhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal(str_to_chars("AB"))),
        Classifier::not(Classifier::Literal(str_to_chars("CD"))),
    ]));

    let c1: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile::<Element, (), char>(&lhs, ());
    let c2: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile(&rhs, ());

    let i = c1.intersection(&c2);
    let u = c1.product(&c2);

    // assert that i has 1 edge
    assert_eq!(i.edges.len(), 1);

    use std::io::Write;

    let g = graphviz_wrap(u.graphviz(), "u");
    let mut output = std::fs::File::create("./union.dot").unwrap();
    assert!(output.write_all(g.as_bytes()).is_ok());

    // assert that u has 3 edges
    assert_eq!(u.edges.len(), 3);
}

fn write_graph(data: String, filename: &str) {
    use std::io::Write;

    let g = graphviz_wrap(data, filename);
    let mut output = std::fs::File::create(filename).unwrap();
    assert!(output.write_all(g.as_bytes()).is_ok());
}

#[test]
fn test_negate() {
    assert!(*TEST_SETUP);
    // the set of P
    // - accepts P
    // - rejects not P
    let cp = Classifier::literal("P");
    let dp: Nfa<NfaNode<()>, NfaEdge<Element>> = cp.compile(());

    assert!(dp.accepts_string("P"));
}
#[test]
fn test_negate2() {
    setup();
    // // the set of all things excluding P
    // // - accepts not P
    // // - rejects P
    let c = Classifier::not(Classifier::literal("P"));
    let d: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());
    assert!(!d.accepts_string("P"));
}

#[test]
fn test_negate3() {
    setup();

    let astar = Classifier::Literal(str_to_chars("A*"));
    let stara = Classifier::Literal(str_to_chars("*b"));
    let c = Classifier::And(BTreeSet::from_iter([astar.clone(), stara.clone()]));

    let d: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());

    write_graph(d.graphviz(), "negate3.dot");

    let mut b = Classifier::compile::<Element, (), char>(&astar, ());
    let mut a = stara.compile(());
    a.set_chirality(LRSemantics::L);
    b.set_chirality(LRSemantics::R);
    let union = a.product(&b);

    write_graph(union.graphviz(), "union-negate3.dot");

    assert!(d.accepts_string("AAb"));
    // assert!(false);
}

#[test]
fn test_negate4() {
    setup();
    let c = Classifier::not(Classifier::And(BTreeSet::from_iter([
        Classifier::Literal(str_to_chars("A*")),
        Classifier::Literal(str_to_chars("*A")),
    ])));

    let d: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());

    // write_graph(d.graphviz(), "negate1.dot");

    assert!(!d.accepts_string("AA"));
}

pub fn test_setup() {
    assert!(*TEST_SETUP);
}

static TEST_SETUP: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    setup();
    true
});

pub fn setup() {
    let subscriber = tracing_subscriber::fmt()
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
        .finish();

    let _ = tracing::subscriber::set_global_default(subscriber);
}
