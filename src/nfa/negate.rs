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
        let dead_end_edges: Result<Vec<Option<(NfaIndex, E)>>, _> = (&self.nodes)
            .iter()
            .map(|(id, _node)| -> Result<_, String> {
                match &self.edges_from(*id).unwrap_or(&vec![])[..] {
                    [] => {
                        if self.edges_to(*id).iter().any(|(_, e)| self.edge(e).criteria == E::universal()) {
                            Ok(None)
                        } else {
                            Ok(Some((*id, E::universal())))
                        }
                    }
                    list => list
                        .iter()
                        .map(|(_target, edge)| self.edge(edge).criteria.clone())
                        .fold(Ok(Some(E::universal())), |acc, cur| match acc {
                            Ok(None) => Ok(None),
                            Ok(Some(acc)) => E::remainder(&acc, &cur),
                            Err(_) => acc,
                        })
                        .map(|r| r.map(|r| {
                            println!("r: {r:?}");
                            return (*id, r)
                        }))
                }
            })
            .collect();

        println!("dead_end_edges: {dead_end_edges:?}");

        let stuff = dead_end_edges?.iter().flatten().cloned().collect::<Vec<(NfaIndex, E)>>();
        println!("stuff: {stuff:?}");
        for (source_node_id, criteria) in stuff {
            // TODO: M needs to be passed down to here...
            let n = NfaNode::new(Terminal::Reject(Default::default()));
            let target = self.add_node(n);
            self.add_edge(NfaEdge{ criteria: criteria.clone() }, source_node_id, target);

            if criteria != E::universal() {
                let n = NfaNode::new(Terminal::Reject(Default::default()));
                let final_target = self.add_node(n);
                self.add_edge(NfaEdge{ criteria: E::universal() }, target, final_target);
            }
        }



        return Ok(complete_dfa);
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn negate(&self) -> Self {
        // TODO: negate must take M and pass it down to create_all_transitions...
        let mut n = self.clone();
        n.create_all_transitions().unwrap();
        n.nodes.iter_mut().for_each(|(_, n)| n.state = match &n.state {
            Terminal::Not => Terminal::Accept(Default::default()),
            Terminal::Accept(_) => Terminal::Not,
            Terminal::Reject(m) => Terminal::Accept(m.clone()),
        });
        n
    }
}
