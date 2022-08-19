#![cfg(test)]
use super::*;

#[test]
fn test_accepts() {
    let d1: Nfa<NfaNode<()>, NfaEdge<Element>> =
        Nfa::from_language("abc".to_string().chars().collect(), ());
    assert!(d1.accepts_string("abc"));
    assert!(!d1.accepts("ab".to_string().chars().collect()));
    assert!(!d1.accepts("a".to_string().chars().collect()));
    assert!(!d1.accepts("abcc".to_string().chars().collect()));
}

#[test]
fn test_graphviz_one() {
    use std::io::Write;

    test_setup();
    let adsf: Vec<char> = "OOABC".chars().collect();
    let lhs = Classifier::Any(BTreeSet::from_iter([
        Classifier::Literal(adsf),
        // Classifier::Literal("OOXYZ"),
    ]));

    // not ABC and not XYZ
    let rhs = Classifier::And(BTreeSet::from_iter([
        Classifier::not(Classifier::Literal("ABC".to_string().chars().into_iter().collect())),
        Classifier::not(Classifier::Literal("XYZ".to_string().chars().into_iter().collect())),
    ]));

    // impl<C, M, E> NfaBuilder<C, M, E> for Nfa<NfaNode<M>, NfaEdge<E>>
    // where
    //     E: Eq + Clone + std::hash::Hash + std::default::Default + std::fmt::Debug,
    //     C: Into<E> + std::fmt::Debug,
    //     M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,
    // {
    //     fn build_nfa(l: Vec<C>, m: M) -> Nfa<NfaNode<M>, NfaEdge<E>> {
    //         // let l: Vec<C> = l.into_iter().collect();
    //         Nfa::from_language(l, m)
    //     }
    // }



    // E: Eq + Clone + std::hash::Hash + std::default::Default + std::fmt::Debug,
    // C: Into<E> + std::fmt::Debug,
    // M: Default + std::fmt::Debug + Clone + PartialOrd + Ord,

    //  element, 

    // IntoIterator<Item = C> + NfaBuilder<L, M, E>

    // E: std::fmt::Debug
    //     + Clone
    //     + Universal
    //     + BranchProduct<E>
    //     + Eq
    //     + std::hash::Hash
    //     + std::default::Default,
    // C: Into<E> + std::fmt::Debug + NfaBuilder<C, M, E>,
    // L: IntoIterator<Item = C>,
    // // <L as std::iter::IntoIterator>::IntoIter: nfa::Language,
    // M: std::fmt::Debug + Clone + PartialOrd + Ord + Default,

    // 66:49  error   the trait bound `char: nfa::NfaBuilder<char, (), element::Element>` is not satisfied ​rustc:
    //                the trait `nfa::NfaBuilder<E, M, C>` is implemented for `nfa::Nfa<nfa::NfaNode<M>, nfa::NfaEdge<E>>`

    let asdf: Nfa<NfaNode<()>, NfaEdge<Element>> = NfaBuilder::<char, (), Element>::build_nfa("XYZ".to_string().chars().into_iter().collect(), ());
    let c: Nfa<NfaNode<()>, NfaEdge<Element>> = Classifier::compile::<Element, (), char>(&lhs, ());

    let g = graphviz_wrap(c.graphviz(), "");
    let mut output = std::fs::File::create("./one.dot").unwrap();
    assert!(output.write_all(g.as_bytes()).is_ok());

    // assert_eq!(Relation::Disjoint, lhs.relation(&rhs));
}

#[test]
fn test_negate() {
    assert!(*TEST_SETUP);
    // the set of P
    // - accepts P
    // - rejects not P
    // let cp = Classifier::Literal("P");
    // let dp: Nfa<NfaNode<()>, NfaEdge<Element>> = cp.compile(());
    // assert!(dp.accepts("P"));

    // // the set of all things excluding P
    // // - accepts not P
    // // - rejects P
    // let c = Classifier::not(Classifier::Literal("P"));
    // let d: Nfa<NfaNode<()>, NfaEdge<Element>> = c.compile(());
    // assert!(!d.accepts("P"));
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
