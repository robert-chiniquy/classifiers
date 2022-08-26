use super::*;

#[test]
fn test_basic_classifier_negation() {
    let n = Nfa::from_str("aa", ());
    n.graphviz_file("negation-1.dot", "aa");
    assert!(n.accepts_string("aa"));

    println!("\n\nnegating n");
    let nn = n.negate();
    nn.graphviz_file("negation-2.dot", "not aa");
    assert!(!nn.accepts_string("aa"));

    println!("\n\nnegating nn");
    let nnn = nn.negate();
    nnn.graphviz_file("negation-3.dot", "not not aa");
    assert!(nnn.accepts_string("aa"));
    assert!(!nnn.accepts_string("bb"));
}

#[test]
fn test_a_v_q() {
    tests::setup();
    use Element::*;
    let n = Nfa::from_symbols(&vec![Tokens(vec!['a', 'a'])], ());
    let i = n.intersection(&Nfa::from_str("??", ()));
    i.graphviz_file("i.dot", "a_v_q");
    assert!(i.accepts_string("aa"));
    let thing = vec![vec![Tokens(vec!['a']), Tokens(vec!['a'])]];
    let e = i.accepting_paths().every_path();
    assert!(e == HashSet::from_iter(thing), "{e:?}");
    println!("accepting_paths: {:?}", i.accepting_paths().every_path());
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
        let mut nfa: Option<Nfa<_, _>> = None;

        // intersect the union of all negations of all accepting paths of self
        let the_paths = self.accepting_paths().every_path();
        println!("accepting_paths: {the_paths:?}");
        for p in the_paths {
            // inversen is non deterministic in order!!
            // union is currently sensity to that.
            let inversen: Vec<Vec<E>> = p.inverse();
            println!("\ninverses {p:?} -> {:?}", &inversen);

            // for every element path, produce a set of element paths which collectively are the negation
            let inversion = Nfa::from_paths(&inversen);
            inversion.graphviz_file("inversion.dot", "inversion");

            nfa = Some(match nfa {
                Some(n) => {
                    println!(
                        "{:?} \n\tVS\n {:?}",
                        inversion.accepting_paths().every_path(),
                        n.accepting_paths().every_path()
                    );
                    inversion.intersection(&n)
                }
                None => inversion,
            });
        }
        nfa.unwrap()
    }
}
