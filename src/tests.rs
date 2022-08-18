#![cfg(test)]
use super::*;

#[test]
fn test_accepts() {
    let d1 = Nfa::from_str("abc");
    assert!(d1.accepts("abc"));
    assert!(!d1.accepts("ab"));
    assert!(!d1.accepts("a"));
    assert!(!d1.accepts("abcc"));
}

#[test]
fn test_graphviz_one() {
    use std::io::Write;

    test_setup();

    let lhs = Classifier::Any(BTreeSet::from_iter([
        Classifier::Literal("OOABC"),
        Classifier::Literal("OOXYZ"),
    ]));

    // not ABC and not XYZ
    let rhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal("ABC")),
        Classifier::not(Classifier::Literal("XYZ")),
    ]));

    let c = lhs.compile();

    let g = graphviz_wrap(c.graphviz(), "");
    let mut output = std::fs::File::create("./one.dot").unwrap();
    assert!(output.write_all(g.as_bytes()).is_ok());

    assert_eq!(Relation::Disjoint, lhs.relation(&rhs));
}

#[test]
fn test_negate() {
    assert!(*TEST_SETUP);
    // the set of P
    // - accepts P
    // - rejects not P
    let cp = Classifier::Literal("P");
    let dp: Nfa<NfaNode<()>, NfaEdge<Element>> = cp.compile();
    assert!(dp.accepts("P"));

    // the set of all things excluding P
    // - accepts not P
    // - rejects P
    let c = Classifier::not(Classifier::Literal("P"));
    let d: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile();
    assert!(!d.accepts("P"));
}

pub fn test_setup() {
    assert!(*TEST_SETUP);
}

static TEST_SETUP: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    setup();
    true
});

fn setup() {
    let subscriber = tracing_subscriber::fmt()
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
        .finish();

    let _ = tracing::subscriber::set_global_default(subscriber);
}
