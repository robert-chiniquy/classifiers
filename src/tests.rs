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
fn test_one() {
    let lhs = Classifier::Any(BTreeSet::from_iter([
        Classifier::Literal("ABC".to_string()),
        Classifier::Literal("XYZ".to_string()),
    ]));

    // not ABC and not XYZ
    let rhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal("ABC".to_string())),
        Classifier::not(Classifier::Literal("XYZ".to_string())),
    ]));

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

pub static TEST_SETUP: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
    setup();
    true
});

fn setup() {
    let subscriber = tracing_subscriber::fmt()
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
        .finish();

    let _ = tracing::subscriber::set_global_default(subscriber);
}
