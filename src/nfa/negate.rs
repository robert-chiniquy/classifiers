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
    let n = Nfa::from_symbols(&[Token('a'), Token('a')], ());
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
        + Accepts<E>
        + Complement<E>
        + Remaindery<E>,
{
    /// These maybe mostly makes a DFA by adding edges for all words in the language.
    //  All terminal paths must end with stars.
    #[tracing::instrument(skip(self))]
    pub fn create_all_transitions(&mut self) -> Result<Self, MatchingError> {
        let complete_dfa: Self = self.clone();

        // for (node_id, _node) in &self.nodes {
        //     let edges = self.edges_from(*node_id);

        //     //    a
        //     // 1 ---> 2
        //     //    b
        //     // 1 ---> 3

        // create dead end node and edges to it
        // (source node id, criteria)
        let mut dead_end_edges: Result<Option<Vec<(NfaIndex, E)>>, _> = (&self.nodes)
            .iter()
            .map(|(id, _node)| -> Result<_, _> {
                if let Some(edges) = self.edges_from(*id) {
                    // identify a non-empty complement of edges (use remainder for now)
                    // and create a least one edge from node to the dead end node to cover the complement
                    let es: Vec<_> = edges
                        .iter()
                        .map(|(_target, edge)| self.edge(edge).criteria.clone())
                        .collect();

                    if !es.is_empty() {
                        let remainder: Result<Option<E>, _> =
                            es[1..]
                                .iter()
                                .fold(Ok(Some(es[0].clone())), |acc, cur| match acc {
                                    Ok(None) => Ok(None),
                                    Ok(Some(acc)) => E::remainder(&acc, cur),
                                    Err(_) => acc,
                                });
                        remainder.map(|r| r.map(|r| (*id, r)))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            })
            .collect();

        // All dead end edges must go to terminal none

        if let Ok(Some(d_e_e)) = dead_end_edges {
            if !d_e_e.is_empty() {
                let dead_end = self.add_node(Default::default());
            }
        }

        Ok(complete_dfa)
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn negate(&self) -> Self {
        todo!();
    }
}
