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
    let n = Nfa::from_symbols(&[Token('a'),Token('a')], ());
    let i = n.intersection(&Nfa::from_str("??", ()));
    assert!(!i.nodes.is_empty());
    i.graphviz_file("i.dot", "a_v_q");
    assert!(i.accepts_string("aa"));
    let thing = vec![vec![Token('a'), Token('a')]];
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

    /// These maybe mostly makes a DFA by adding edges for all words in the language.
    //  All terminal paths must end with stars.
    #[tracing::instrument(skip(self))]
    pub fn create_all_transitions(&self) -> Self {
        let complete_dfa: Self = self.clone();

        // for (node_id, _node) in &self.nodes {
        //     let edges = self.edges_from(*node_id);



        //     //    a
        //     // 1 ---> 2
        //     //    b
        //     // 1 ---> 3


        // }
        complete_dfa
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn negate(&self) -> Self {

        todo!();
    }
}
