#![cfg(test)]
use super::*;

#[test]
fn test_intersection_of_heterogenous_states() {
    // #![allow(unused)]
    // let everything_but_tacos = Classifier::And(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("tacos"))),
    //     Classifier::Literal("*")),
    // ]));

    // let all_ts_but_tacos = Classifier::And(BTreeSet::from_iter([
    //     Classifier::Literal("t*")),
    //     Classifier::not(Classifier::Literal("tacos"))),
    // ]));

    // let all_ts_but_q = Classifier::And(BTreeSet::from_iter([
    //     Classifier::Literal("t*")),
    //     Classifier::not(Classifier::Literal("*q"))),
    // ]));

    // let combo = Classifier::Any(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("tacos"))),
    //     Classifier::Literal("*")),
    //     Classifier::And(BTreeSet::from_iter([
    //         Classifier::Literal("t*")),
    //         Classifier::not(Classifier::Literal("*q"))),
    //     ])),
    // ]));

    // let c = Classifier::compile::<Element, (), char>(&everything_but_tacos, ());
}

#[test]
fn test_terminal_on() {
    // let zzz = Classifier::Any(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("AB"))),
    //     Classifier::Literal("AB")),
    // ]));
    // Matt is suggesting a Element::NotToken, doesn't need to be a public interface
    // let zzz = Classifier::And(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("A*"))),
    //     Classifier::Literal("*B")),
    // ]));

    // // only 1 char
    // let _ = Classifier::And(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("**"))),
    //     Classifier::Literal("*")),
    // ]));

    // let c = Classifier::compile::<Element, (), char>(&zzz, ());
    // write_graph(c.graphviz(), "terminal_on_zzz1.dot");
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

    // let lhs = Classifier::And(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("AB"))),
    //     Classifier::not(Classifier::Literal("XY"))),
    // ]));

    // let rhs = Classifier::And(BTreeSet::from_iter([
    //     Classifier::not(Classifier::Literal("AB"))),
    //     Classifier::not(Classifier::Literal("CD"))),
    // ]));

    todo!()

    // let c1: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile::<Element, (), char>(&lhs, ());
    // let c2: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile(&rhs, ());

    // c1.graphviz_file("test_rejection.1.dot", "![ab] X ![xy]");
    // let i = c1.intersection(&c2);
    // let u = c1.product(&c2);

    // i.graphviz_file("test_rejection.dot", "![ab],![xy] X ![ab],![cd]");
    // // assert that i has 1 edge
    // assert_eq!(i.edges.len(), 1);

    // use std::io::Write;

    // let g = graphviz_wrap(u.graphviz(), "u");
    // let mut output = std::fs::File::create("./union.dot").unwrap();
    // assert!(output.write_all(g.as_bytes()).is_ok());

    // assert that u has 3 edges
    // assert_eq!(u.edges.len(), 3);
}

#[test]
fn test_negate() {
    assert!(*TEST_SETUP);
    // the set of P
    // - accepts P
    // - rejects not P
    let cp = Classifier::<Dfa>::Literal("P".to_string(), None);
    let dp = cp.compile(&None);

    // assert!(dp.accepts_string("P"));
    todo!()
}
#[test]
fn test_negate2() {
    setup();
    // // the set of all things excluding P
    // // - accepts not P
    // // - rejects P
    let c = Classifier::not(Classifier::Literal::<Dfa>("P".to_string(), None));
    let _d = c.compile(&None);
    // assert!(!d.accepts_string("P"));
    todo!()
}

#[test]
fn test_simpler_intersection() {
    let a = Classifier::<Dfa>::Literal("*b".to_string(), None);
    let b = Classifier::Literal("a".to_string(), None);
    let c = Classifier::Any(BTreeSet::from_iter([a, b]));
    let _d = c.compile(&None);

    todo!()
}

#[test]
fn test_intersection() {
    setup();

    let astar = Classifier::<Dfa>::Literal("A*".to_string(), None);
    let stara = Classifier::Literal("*b".to_string(), None);
    let _c = Classifier::And(BTreeSet::from_iter([astar, stara]));

    todo!()

    // let d = c.compile(());
    // assert!(d.accepts_string("AAb"));

    // let mut b: Nfa<NfaNode<()>, NfaEdge<Element>> = astar.compile(());
    // let mut a = stara.compile(());
    // let product = a.product(&b);

    // write_graph(product.graphviz(), "product-intersection.dot");
}

#[test]
fn test_negate4() {
    test_setup();
    let c = Classifier::not(Classifier::And(BTreeSet::from_iter([
        Classifier::<Dfa>::Literal("A*".to_string(), None),
        Classifier::Literal("*A".to_string(), None),
    ])));

    let _d = c.compile(&None);
    todo!()
    // assert!(!d.accepts_string("AA"));
}

#[allow(dead_code)]
pub fn test_setup() {
    assert!(*TEST_SETUP);
}

static TEST_SETUP: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    setup();
    true
});

fn setup() {
    // #[ignore]
    #[cfg(feature = "trace")]
    {
        let subscriber = tracing_subscriber::fmt()
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
            .finish();

        let _ = tracing::subscriber::set_global_default(subscriber);
    }
}
