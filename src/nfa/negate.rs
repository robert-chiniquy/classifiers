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
        let mut complete_dfa: Self = self.clone();

        for (node_id, _node) in &self.nodes {
            let edges = self.edges_from(*node_id);

            //    a
            // 1 ---> 2
            //    b
            // 1 ---> 3


        }
        complete_dfa
        // // The stack loop covers subtrees which require smashing at any given point,
        // // subtrees which diverge will be copied.
        // // This is order sensitive if there are multiple entry nodes which mostly does not happen
        // // (and they are stored in a vec)
        // // Cycle detection can occur by tracking visited combinations on the stack
        // let mut visited: HashSet<_> = Default::default();
        // while let Some((union_id, other_id)) = stack.pop() {
        //     if visited.contains(&(union_id, other_id)) {
        //         // should this also validate working node id?
        //         continue;
        //     }
        //     visited.insert((union_id, other_id));
        //     // smash these nodes together
        //     // if no edges from either, consider them smashed
        //     // where edges match from other to union, push the edge targets on the stack
        //     // where edges do not match, create a new edge of the appropriate (other) kind and copy subtree to it.
        //     //
        //     let union_edges = union.edges_from(union_id);
        //     let other_edges = other.edges_from(*other_id);

        //     if other_edges == None {
        //         continue;   
        //     }

        //     if union_edges.is_some()
        //         && union_edges.as_ref().unwrap().is_empty()
        //         && other_edges.as_ref().unwrap().is_empty() {
        //         continue;
        //     }


        //     if union_edges == None || union_edges.as_ref().unwrap().is_empty() {
        //         union.copy_subtree(&union_id, other, other_id);
        //         continue;
        //     }


        //     for (other_edge_target, other_edge) in other_edges.unwrap() {
        //         let other_edge_kind = other.edge(other_edge);
        //         // is there a matching edge in union?
        //         let matching = union
        //             .edge_by_kind(union_id, &other_edge_kind.criteria)
        //             .pop();
                
        //         if let Some ((union_edge_target, _)) = matching {
        //             union.node_mut(union_id).sum_mut(other.node(*other_id));
        //             stack.push((union_edge_target, other_edge_target));
        //             continue;
        //         }

        //         let new_node = union.add_node(other.node(*other_edge_target).clone());
        //         union.add_edge(
        //             NfaEdge {
        //                 criteria: other_edge_kind.criteria.clone(),
        //             },
        //             *other_id,
        //             new_node,
        //         );
        //         union.copy_subtree(&new_node, other, other_edge_target);

        //     }
        // }
        // union
    }

    #[tracing::instrument(skip(self), ret)]
    pub fn negate(&self) -> Self {
        // universal is fine as an initial value for an intersection,
        // need to rationalize for empty input?
        // let mut nfa: Option<Nfa<_, _>> = None;

        // all nodes must have all transitions of the language explicitly implemented 
        todo!();
    }
}
