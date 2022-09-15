#[cfg(test)]
use super::*;

#[test]
fn test_literal() {
    test_setup();
    // the set of P
    // - accepts P
    // - rejects not P
    let cp = Classifier::Literal("P".to_string(), None);
    let dp: Dfa = cp.compile(&None);
    assert!(dp.includes_string("P"));
}

#[test]
fn test_complement() {
    test_setup();
    // the set of all things excluding P
    // - accepts not P
    // - rejects P
    let c: Classifier = Classifier::Literal("P".to_string(), None);
    let d: Dfa = c.compile(&None).complement(&None);
    assert!(!d.includes_string("P"));
}

#[test]
fn test_simpler_intersection() {
    let a = Classifier::<Dfa>::Literal("*b".to_string(), None);
    let b = Classifier::Literal("*a".to_string(), None);
    let c = Classifier::Any(BTreeSet::from_iter([a, b]));
    let _d = c.compile(&None);

    // todo!()
}

#[test]
fn test_intersection() {
    test_setup();

    let astar = Classifier::<Dfa>::Literal("B*".to_string(), None);
    let stara = Classifier::Literal("*A".to_string(), None);
    let i = Classifier::And(BTreeSet::from_iter([astar, stara]));

    let d = i.compile(&None);
    assert!(!d.transitions.is_empty());
    assert!(!d.states.is_empty());
}

#[test]
fn test_negate1() {
    test_setup();

    let c = Classifier::<Dfa>::Literal("A*".to_string(), None);
    let mut d = c.compile(&None);
    d.simplify();
    d.graphviz_file("negation1.dot", "A*");

    d = d.complement(&None);
    d.simplify();
    d.graphviz_file("negation2.dot", "!A*");
    // assert!(d.includes_path(&['A'.into()]));

    d = d.complement(&None);
    d.simplify();
    d.graphviz_file("negation3.dot", "!!A*");
    assert!(!d.includes_path(&['A'.into()]));
}

#[test]
fn test_negate4() {
    test_setup();
    let c = Classifier::not(Classifier::And(BTreeSet::from_iter([
        Classifier::<Dfa>::Literal("A*".to_string(), None),
        Classifier::Literal("*A".to_string(), None),
    ])));

    let mut d = c.compile(&None);
    d.simplify();
    d.graphviz_file("negate4.dot", "negate4.dot");
    let t = Element::token;
    assert!(!d.includes_path(&[t('A'), t('A')]));
    assert!(!d.includes_path(&[t('A'), t('A'), t('A')]));
}

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

    // let c = Classifier::compile(&everything_but_tacos, ());
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

    // let c = Classifier::compile(&zzz, ());
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

    // let c1: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile(&lhs, ());
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
fn test_arithmetic() {
    // use Element::*;
    let nt = Element::not_token;
    let nts = Element::not_tokens;
    let ts = Element::tokens;
    let t = Element::token;

    // !c - a - b = !a!b!c
    let r = Element::difference(&nt('c'), &t('a'));
    let r: Element = Element::difference(&r, &t('b'));
    assert_eq!(r, Element::not_tokens(&['a', 'b', 'c']));

    // * - a - b = !a!b
    let mut r = Element::universal();
    r = Element::difference(&r, &t('a'));
    r = Element::difference(&r, &t('b'));
    assert_eq!(r, nts(&['a', 'b']));

    // ? - a - b = !a!b
    let r = Element::difference(&Element::universal(), &t('a'));
    let r = Element::difference(&r, &t('b'));
    assert_eq!(r, nts(&['a', 'b']));

    // ab - a -b = None
    let r = Element::difference(&ts(&['a', 'b']), &t('a'));
    assert_eq!(ts(&[]), Element::difference(&r, &t('b')));

    // !a - !c = c
    assert_eq!(Element::difference(&nt('a'), &nt('c')), t('c'));
}

#[test]
fn test_sum() {
    let t = Element::token;
    let nt = Element::not_token;
    let nts = Element::not_tokens;

    let e: Element = vec!['a', 'b', 'c'].iter().map(|c| t(*c)).sum();
    assert_eq!(e, Element::tokens(&['a', 'b', 'c']));

    let e: Element = vec![].iter().map(|c| t(*c)).sum();
    assert_eq!(e, Element::tokens(&[]));

    let e: Element = vec![t('a'), nt('a')].iter().sum();
    assert_eq!(e, Element::not_tokens(&[]));

    let e: Element = vec![nt('a'), nt('a')].iter().sum();
    assert_eq!(e, Element::not_tokens(&['a']));

    let e: Element = vec![nt('a'), nts(&['a', 'b'])].iter().sum();
    assert_eq!(e, Element::not_tokens(&['a']));

    let e: Element = vec![nt('a'), nt('b')].iter().sum();
    assert_eq!(e, Element::not_tokens(&[]));
}

#[test]
fn test_accepts() {
    let a = Element::not_tokens(&['a', ':']);
    let b = Element::not_tokens(&['a', 'b', ':']);
    assert!(a.accepts(&b));
    assert!(!b.accepts(&a));
}
