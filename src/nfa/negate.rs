use super::*;

#[test]
fn test_basic_classifier_negation() {

    &Nfa::from_paths(vec![])
    // tests::setup();

    // let nt1 = Nfa::from_str("***", ());
    // nt1.graphviz_file("nt1.dot", ".");
    // let nt2 = nt1.union(&Nfa::from_str("****", ()));
    // nt2.graphviz_file("nt2.dot", ".");

    // panic!();

    // let problem = [
    //     Nfa::from_str("***", ()),
    //     Nfa::from_str("?", ()),
    //     Nfa::from_symbols(&[Element::NotTokens(vec!['a', 'a'])], ()),
    // ];

    // let step1 = Nfa::from_str("***", ());
    // step1.graphviz_file("step1.dot", ".");
    // let step2 = step1.union(&Nfa::from_str("?", ()));
    // step2.graphviz_file("step2.dot", ".");
    // let step3 = step2.union(&Nfa::from_symbols(
    //     &[Element::NotTokens(vec!['a', 'a'])],
    //     (),
    // ));
    // step3.graphviz_file("step3.dot", ".");
    // let step4 = step1.intersection(&Nfa::universal(Default::default()));

    // assert!(step4.nodes.len() < 100);

    // step4.graphviz_file("step4.dot", ".");

    let c = Classifier::literal("aa");

    let n = c.compile::<Element, _, _>(());
    assert!(n.accepts_string("aa"));

    n.graphviz_file("negation-0.dot", "aa");

    let n = n.negate();

    n.graphviz_file("negation-01.dot", "just not aa");

    // // This classifier includes all things not aa due to the Universal
    // let c: Classifier<_> = Classifier::and(&[
    //     Classifier::Universal,
    //     // this Not expresses that aa is not in the set, but does not accept any path.
    //     Classifier::not(Classifier::literal("aa")),
    // ]);

    // let n = c.compile::<Element, _, _>(());
    // assert!(!n.accepts_string("aa"));

    // n.graphviz_file("negation-1.dot", "not aa");

    // // the meaning of negate is to accept all things rejected,
    // // and reject all things accepted.
    // let nn = n.negate();

    // nn.graphviz_file("negation-2.dot", "not not aa");

    // // ? the below should fail as the Universal accept is negated in the above.
    // // assert!(nn.accepts_string("aa"));

    // let n = nn.negate();
    // assert!(!n.accepts_string("aa"));

    // n.graphviz_file("negation-3.dot", "not not not aa");
}

impl<M, E> Nfa<NfaNode<M>, NfaEdge<E>>
where
    M: std::fmt::Debug + Clone + PartialOrd + Ord + PartialEq + Eq + std::default::Default,
    Vec<E>: Invertible,
    E: std::fmt::Display
        + std::fmt::Debug
        + Clone
        + Eq
        + std::hash::Hash
        + std::default::Default
        + BranchProduct<E>
        + Universal
        + Accepts<E>,
{
    #[tracing::instrument(skip(self), ret)]
    pub fn negate(&self) -> Self {
        // universal is fine as an initial value for an intersection,
        // need to rationalize for empty input?
        let mut nfa: Nfa<NfaNode<M>, NfaEdge<E>> = Nfa::universal(Default::default());

        // intersect the union of all negations of all accepting paths of self
        let the_paths = self.accepting_paths().every_path();
        for p in the_paths {
            println!("p: {:?}", p);

            // inversen is non deterministic in order!!
            // union is currently sensity to that.
            let inversen: Vec<Vec<E>> = p.inverse();
            println!("inversen: {:?}", &inversen);
            // unsafe {
            // let newNFA: &Nfa<NfaNode<()>, NfaEdge<Element>> = &Nfa::from_paths(std::mem::transmute::<_, &Vec<Vec<Element>>>(&inversen));
            // newNFA.graphviz_file("n.dot", "aa");
            // println!("newNFA: {:?}", &newNFA);
            // }

            // for every element path, produce a set of element paths which collectively are the negation
            let asdf = &Nfa::from_paths(&inversen);
            asdf.graphviz_file("asdf.dot", "asdf");
            println!("asdf: {:?}", &asdf);
            nfa = nfa.intersection(asdf);
        }
        nfa
    }
}
