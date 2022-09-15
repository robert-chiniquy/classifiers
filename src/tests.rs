#[cfg(test)]
use super::*;

#[test]
fn test_basic_classifier() {
    let c1 = Classifier::<Dfa>::Literal("a*".to_string(), None);
    let c2 = Classifier::Literal("*a".to_string(), None);
    let c3 = Classifier::and(&[c1.clone(), c2.clone()]);
    let mut d = c3.compile(&None);
    d.simplify();
    d.graphviz_file("new-test.dot", "a* & *a");
    assert_eq!(c1.relation(&c2), Relation::Intersection);
}

#[test]
fn test_relation() {
    let mut inputs = [
        ("*", "", Relation::Disjoint),
        ("", "*", Relation::Disjoint),
        ("*b", "*", Relation::Subset),
        ("*", "*", Relation::Equality),
        ("f*", "*", Relation::Subset),
        ("a*", "a", Relation::Disjoint),
        ("a", "a*", Relation::Disjoint),
        ("**", "*?*", Relation::Superset),
        ("**", "*f", Relation::Superset), // nb
        ("aab", "a?", Relation::Disjoint),
        ("?", "*", Relation::Subset),
        ("??", "*", Relation::Subset),
        ("*", "f*", Relation::Superset),
        ("**", "*f*", Relation::Superset),
        ("a", "*", Relation::Subset),
        ("*", "a", Relation::Superset),
        ("a*", "*a", Relation::Intersection),
        ("a", "a", Relation::Equality),
        ("aa", "a", Relation::Disjoint),
        ("a", "aa", Relation::Disjoint),
        ("a*b*z", "a*c*z", Relation::Intersection),
        ("a", "a*b", Relation::Disjoint),
        ("a*b", "a:b", Relation::Disjoint),
        ("*f", "f*f", Relation::Superset),
        ("f*f", "*f", Relation::Subset),
        ("*f*", "*f*", Relation::Equality),
        ("*f*", "f*f*", Relation::Superset),
        ("f*f*", "*f*", Relation::Subset),
        ("asdf*f**", "*f*", Relation::Subset),
        ("*?*", "***", Relation::Equality),
        ("f*f*", "*&f*", Relation::Intersection), // asdfafa
    ];

    inputs.reverse();

    let mut fails = 0;
    let mut results: Vec<_> = Default::default();
    for (a, b, outcome) in inputs {
        let mut da = Classifier::<Dfa>::Literal(a.to_string(), None).compile(&None);
        let mut db = Classifier::<Dfa>::Literal(b.to_string(), None).compile(&None);

        da.simplify();
        db.simplify();

        println!("\n\n🐥🐥🐥🐥🐥🐥🐥 {a} {b}\n\n");
        assert!(da.is_consistent(), "failed on: {a}");
        assert!(db.is_consistent(), "failed on: {b}");

        let (dr, dp, l, r) = da.relation(&db);
        results.push((da, db, a, b, outcome, dr, dp, l, r));
    }

    for (da, db, a, b, outcome, dr, dp, l, r) in results {
        if dr != outcome {
            println!(
                "🌵🌵🌵🌵🌵🌵 {a} v. {b} {outcome} != {dr}\nl:{l:?}\nr:{r:?}\nda: {da:?}\ndb: {db:?}"
            );
            fails += 1;
            dp.graphviz_file(
                &format!(
                    "product-{}_{}.dot",
                    a.replace('*', "_"),
                    b.replace('*', "_")
                ),
                &format!("{a} v. {b} {outcome} != {dr}"),
            );
            da.graphviz_file(&format!("single-{}.dot", a.replace('*', "_")), a);
            db.graphviz_file(&format!("single-{}.dot", b.replace('*', "_")), b);
        }
    }
    assert_eq!(fails, 0);
}

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
    assert!(d.includes_string("Pffffft"));
}

#[test]
fn test_simpler_intersection() {
    let a = Classifier::Literal("*b".to_string(), None);
    let b = Classifier::Literal("*a".to_string(), None);
    let c = Classifier::Any(BTreeSet::from_iter([a, b]));
    let d: Dfa = c.compile(&None);
    assert!(d.includes_string("bb"));
    assert!(!d.includes_string("b"));
    assert!(d.includes_string("aa"));
    assert!(!d.includes_string("a"));
    assert!(d.includes_string("ab"));
    assert!(!d.includes_string("bq"));
}

#[test]
fn test_dfa_intersect() {
    let a = Dfa::<()>::from_language("a*".to_string().chars().collect(), &None);
    let b = Dfa::from_language("*a".to_string().chars().collect(), &None);

    let mut dfa = Dfa::intersect(&a, &b);
    dfa.simplify();

    assert!(dfa.includes_string("aa"));
    assert!(dfa.includes_string("aaa"));
    assert!(!dfa.includes_string("aab"));
    assert!(!dfa.includes_string("a"));
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
    assert!(d.includes_string("BA"));
    assert!(!d.includes_string("AA"));
    assert!(d.includes_string("BAA"));
    assert!(d.includes_string("BqA"));
}

#[test]
fn test_complement1() {
    test_setup();

    let c = Classifier::<Dfa>::Literal("A*".to_string(), None);
    let mut d = c.compile(&None);
    d.simplify();
    d.graphviz_file("complement1.dot", "A*");

    d = d.complement(&None);
    d.simplify();
    d.graphviz_file("complement2.dot", "!A*");
    assert!(d.includes_string("A"));

    d = d.complement(&None);
    d.simplify();
    d.graphviz_file("complement3.dot", "!!A*");
    assert!(!d.includes_string("A"));
}

#[test]
fn test_negate() {
    test_setup();
    let c = Classifier::not(Classifier::And(BTreeSet::from_iter([
        Classifier::<Dfa>::Literal("A*".to_string(), None),
        Classifier::Literal("*A".to_string(), None),
    ])));

    let mut d = c.compile(&None);
    d.simplify();
    d.graphviz_file("negate4.dot", "negate4.dot");

    assert!(!d.includes_string("A"));
    assert!(!d.includes_string("AA"));
    assert!(!d.excludes_string("A"));
    assert!(d.excludes_string("AA"));
}

#[test]
fn test_intersection_of_heterogenous_states() {
    let combo = Classifier::any(&[
        Classifier::not(Classifier::literal("tacos")),
        Classifier::literal("*"),
        Classifier::and(&[
            Classifier::literal("t*"),
            Classifier::not(Classifier::literal("*q")),
        ]),
    ]);

    let d: Dfa = Classifier::compile(&combo, &None);
    assert!(!d.includes_string("tacos"));
    assert!(d.excludes_string("tacos"));
    assert!(d.includes_string("A"));
    assert!(d.includes_string("tZZZZq"));
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
