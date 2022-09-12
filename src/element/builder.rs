use std::collections::BTreeSet;

use std::collections::HashMap;
use std::collections::{BTreeMap, HashSet};

use super::*;

/*

digraph G {
    rankdir = TB;
    remincross = true;
    splines = true;
    fontsize="40";

    bgcolor = "#555555";
    node[color = "#FFFFFF"];
    node[fontcolor = "#FFFFFF"];
    edge[color = "#FFFFFF", fontcolor="#FFFFFF"];

    label = "dfa";

  node_0 -> node_1 [label="a" fontsize="20pt"];
  node_1 -> node_2 [label="b" fontsize="20pt"];
  node_0 [label="enter", shape="circle"]
  node_1 [label="1", shape="circle"]
  node_2 [label="2", shape="circle"]
}

*/

// number of rows/nodes = number of chars in input + 1
// number of columns: number of symbols in input
// abc
//   a b c
// 1 2
// 2   3
// 3     4
// 4
// a*b
// the presence of * implies the complemement of all symbols
// 2,3 is added to the table as there is a transition that goes to 2 and 3 ... ?
//       | a   | b     | !a!b |
// 1     | 2   | Ø     | Ø    |
// 2     | 2,3 | 2,3   | 2,3  |
// 3     | Ø   | 4     | Ø    |
// 4     | Ø   | Ø     | Ø    |
// 2,3   | 2,3 | 2,3,4 | 2,3  |
// 2,3,4 | 2,3 | 2,3   | 2,3  |
// where-ever you have columns such as b and ab,
// a transition of ab from a node counts as a transition of b as well
// for the purpose of unioning the resulting transitions

type CompoundId = BTreeSet<NodeId>;

#[derive(Default, Debug)]
pub struct DfaBuilder {
    entry: CompoundId,
    elements: BTreeSet<Element>,
    symbols: BTreeSet<char>,
    // a -> 1 -> [(2), (4), (2,3)]
    transitions: BTreeMap<Element, BTreeMap<CompoundId, Vec<CompoundId>>>,
    accepting_states: BTreeSet<NodeId>,
}

#[test]
fn test_product() {
    // let a = DfaBuilder::from_language("ab".to_string().chars().collect());
    // let b = DfaBuilder::from_language("ac".to_string().chars().collect());
    // let _ = DfaBuilder::product(&a, &b);

    let a = DfaBuilder::from_language("a*".to_string().chars().collect());
    let b = DfaBuilder::from_language("*a".to_string().chars().collect());

    let dfa = DfaBuilder::product(&a, &b);

    assert!(dfa.accepts(&vec!['a', 'a']).unwrap());
    assert!(dfa.accepts(&vec!['a', 'a', 'a']).unwrap());
    assert!(!dfa.accepts(&vec!['a', 'a', 'b']).unwrap());
    assert!(!dfa.accepts(&vec!['a']).unwrap());
}

impl DfaBuilder {
    pub fn new_product(a: &Self, b: &Self) -> Self {
        let symbols: BTreeSet<_> = &a.symbols | &b.symbols;

        let mut elements: BTreeSet<Element> = symbols.iter().map(|c| c.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        let accepting_states = &a.accepting_states | & b.accepting_states;

        Self {
            symbols,
            elements,
            accepting_states,
            ..Default::default()
        }


    }

    pub fn new(symbols: BTreeSet<char>) -> Self {
        let mut elements: BTreeSet<Element> = symbols.iter().map(|s| s.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        Self {
            symbols,
            elements,
            ..Default::default()
        }
    }

    fn product(a: &Self, b: &Self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let b = DfaBuilder::construct_product(a, b);
        let d = b.build_dfa();
        d.graphviz_file("product-dfa.dot", "dfa");
        d
    }

    /*

    // for every accepting symbol
    // for every state in a
    // for every state in b
    //   add transition for (a_state, b_state):  `δ(a_state, symbol) U δ(b_state, symbol)))
    
    example: 

    0 -a-> 1 -b-> (2)
    4 -a-> 5 -c-> (6)

    S  | a  | b | c
    0  | 1  | Ø | Ø
    1  | Ø  | 2 | Ø
    4  | 5  | Ø | Ø
    5  | Ø  | Ø | 6
    04 | 15 | Ø | Ø
    05 | 1  | Ø | 6
    14 | 5  | 2 | Ø
    15 | Ø  | 2 | 6

    04 -a-> 15 -b -> (2)
               -c -> (6)

    entry node is composite of A and B entry
    accepting states likewise come from A or B

    */
    fn construct_product(a: &Self, b: &Self) -> Self {
        let mut values = BTreeSet::new();
        for (_, t) in a.transitions.clone() {
            for (from, tos) in t {
                values.extend(from);
                for to in tos {
                    values.extend(to);
                }
            }
        }
        let max_a = values.iter().max().unwrap_or(&1);
        let b = b.offset_self(max_a + 1);

        let mut product = DfaBuilder::new_product(&a, &b);

        let mut stack: Vec<BTreeSet<NodeId>> = Default::default();

        // product.count = a.count + b.count + 2;
        product.entry = &a.entry | &b.entry;

        for e in &product.elements.clone() {
            let a_transitions = a.transitions.get(e);
            let b_transitions = b.transitions.get(e);

            match (a_transitions, b_transitions) {
                (None, None) => println!("nothing with symbol {e}...probably fine"),
                (Some(t), None) | (None, Some(t)) => t.iter().for_each(|(from, toos)| {
                    toos.iter()
                        .for_each(|to| product._add_transition(from, e, to))
                }),
                (Some(a_t), Some(b_t)) => {
                    a_t.iter().for_each(|(a_from, a_toos)| {
                        b_t.iter().for_each(|(b_from, b_toos)| {
                            let compound_id = a_from | b_from;
                            stack.push(compound_id.clone());
                            let mut to: CompoundId = Default::default();
                            a_toos
                                .iter()
                                .chain(b_toos.iter())
                                .for_each(|s| to.extend(s));

                            product._add_transition(&compound_id, &e.clone(), &to);
                            a_toos.iter().for_each(|to|product._add_transition(a_from, &e.clone(), to));
                            b_toos.iter().for_each(|to|product._add_transition(b_from, &e.clone(), to));
                        });
                    });
                }
            }
        }

        product.complete_transitions(stack);
        product
    }

    pub fn find_compound_ids(&self) -> Vec<CompoundId>{
        let mut stack: Vec<_> = Default::default();
        for (_, t) in &self.transitions {
            for (from, tos) in t {
                stack.push(from.clone());
                for to in tos {
                    stack.push(to.clone());
                }
            }
        }
        //  We only care about compound ids since 
        stack.iter().filter(|v| v.len() > 1).cloned().collect()

    }
    // TODO: M
    pub fn build(&mut self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let d = self.build_dfa();
        debug_assert!(d.entry == 0);
        d.graphviz_file("dfa.dot", "dfa");
        d
    }

    pub fn complete_transitions(&mut self, mut stack: Vec<CompoundId>) {
        // println!("🌮🌮🌮 the stack: {stack:?}\n🌮🌮🌮 transitions: {:?}\n\n", self.transitions);

        let mut visited: HashSet<CompoundId> = Default::default();

        while let Some(compound_state) = stack.pop() {
            if visited.contains(&compound_state) {
                continue;
            }
            visited.insert(compound_state.clone());

            for (element, transitions) in self.transitions.clone() {
                let mut unioned_transitions: CompoundId = Default::default();
                for c in compound_state.clone() {
                    // println!("looking for {c}");
                    if let Some(toos) = transitions.get(&BTreeSet::from([c])) {
                        // println!("🌮🌮🌮🌮🌮 element: {element:?} c: {c:} toos: {toos:?}");
                        unioned_transitions.extend(toos.iter().flatten());
                    }
                }

                if unioned_transitions.is_empty() {
                    continue;
                }
                self._add_transition(&compound_state, &element, &unioned_transitions.clone());
                stack.push(unioned_transitions);
            }
        }
    }

    fn build_dfa(&self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let mut dfa: Nfa<NfaNode<()>, NfaEdge<Element>> = Default::default();
        let mut node_id_map: HashMap<CompoundId, NodeId> = Default::default();
        let mut nodes: HashSet<CompoundId> = Default::default();

        for (_, transitions) in self.transitions.clone() {
            for (from, to) in transitions {
                nodes.insert(from);
                for c_id in to {
                    nodes.insert(c_id);
                }
            }
        }

        for compound_id in nodes {
            let dfa_node_id = dfa.add_node(NfaNode {
                state: match (&self.accepting_states & &compound_id).is_empty() {
                    true => Default::default(),
                    false => Terminal::Accept(()),
                },
                ..Default::default()
            });
            // println!("dfa node: {:?}", dfa.node(dfa_node_id));
            node_id_map.insert(compound_id, dfa_node_id);
        }

        println!(
            "symbols: {:?}\nnode id map {node_id_map:?}\ntransitions {:?}\nentry {:?}",
            self.symbols, self.transitions, self.entry
        );

        // add edges after nodes exist
        for (c, transitions) in self.transitions.clone() {
            for (from, tos) in transitions {
                for to in tos {
                    dfa.add_edge(
                        NfaEdge {
                            criteria: c.clone(),
                        },
                        *node_id_map.get(&from).unwrap(),
                        *node_id_map.get(&to).unwrap(),
                    );
                }
            }
        }

        dfa.entry = *node_id_map.get(&self.entry).unwrap();

        dfa.simplify();
        dfa
    }

    pub fn set_accepting_state(&mut self, node: NodeId) -> bool {
        self.accepting_states.insert(node)
    }

    fn _add_transition(&mut self, from: &CompoundId, e: &Element, to: &CompoundId) {
        let mut no_e = true;
        // println!("before: {:?}", self.transitions);
        for element in &self.elements {
            if !e.accepts(element) {
                continue;
            }
            no_e = false;
            self.transitions
                .entry(element.clone())
                .and_modify(|t| {
                    t.entry(from.clone())
                        .and_modify(|_from| _from.push(to.clone()))
                        .or_insert_with(|| vec![to.clone()]);
                })
                .or_insert_with(|| BTreeMap::from([(from.clone(), vec![to.clone()])]));
        }

        // println!("after: {:?}", self.transitions);
        if no_e {
            panic!("did not add a transition for {from:?} -{e:?}-> {to:?}");
        }
    }

    pub fn add_transition(&mut self, from: &NodeId, e: &Element, to: &NodeId) {
        self._add_transition(&BTreeSet::from([*from]), e, &BTreeSet::from([*to]));
    }

    pub(crate) fn from_language(l: Vec<char>) -> Self {
        let symbols: BTreeSet<_> = l
            .iter()
            .map(|c| if *c == '?' || *c == '*' { ':' } else { *c })
            .collect();

        let mut builder = DfaBuilder::new(symbols.clone());
        let mut prior = 0;
        builder.entry = CompoundId::from([prior]);

        let colon_free = Element::TokenSet(&symbols.clone() - &BTreeSet::from([':']));
        let not_colon_free = Element::NotTokenSet(&symbols | &BTreeSet::from([':']));

        for c in &l {
            let current = prior + 1;
            match c {
                '?' => {
                    // transition via all symbols from prior to current
                    builder.add_transition(&prior, &colon_free.clone(), &current);
                    builder.add_transition(&prior, &not_colon_free.clone(), &current);
                }
                '*' => {
                    // transition via all symbols from prior to current
                    // also have a self-loop (2 actually)
                    builder.add_transition(&prior, &colon_free.clone(), &current);
                    builder.add_transition(&prior, &not_colon_free.clone(), &current);
                    builder.add_transition(&prior, &colon_free.clone(), &prior);
                    builder.add_transition(&prior, &not_colon_free.clone(), &prior);
                }
                c => {
                    // transition from prior to current via c
                    builder.add_transition(&prior, &c.into(), &current);
                }
            }
            prior = current;
        }

        builder.set_accepting_state(prior);
        builder
    }

    /// Consume self, return a new self
    pub(crate) fn offset_self(&self, offset: u32) -> Self {
        let mut transitions: BTreeMap<Element, BTreeMap<CompoundId, Vec<CompoundId>>> =
            Default::default();
        for (element, v) in &self.transitions {
            transitions.insert(element.clone(), Default::default());
            for (from, to) in v {
                let from: CompoundId = from.iter().map(|id| *id + offset).collect();
                let to: Vec<CompoundId> = to
                    .iter()
                    .map(|vec| vec.iter().map(|id| *id + offset).collect())
                    .collect();

                transitions
                    .get_mut(&element.clone())
                    .unwrap()
                    .insert(from.clone(), to.clone());
            }
        }
        Self {
            entry: self.entry.iter().map(|id| *id + offset).collect(),
            elements: self.elements.clone(),
            symbols: self.symbols.clone(),
            transitions,
            accepting_states: self.accepting_states.iter().map(|u| u + offset).collect(),
        }
    }
}
