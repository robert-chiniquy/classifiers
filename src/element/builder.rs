use std::collections::BTreeSet;

use std::collections::btree_map::Entry::*;
use std::collections::HashMap;
use std::collections::{BTreeMap, HashSet};
use std::iter::FromIterator;

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

#[derive(Default, Debug)]
pub struct DfaBuilder {
    entry: NodeId,
    symbols: BTreeSet<char>,
    count: u32,
    // source state collection -> compound id
    // {2,3} -> 23
    states: HashMap<BTreeSet<NodeId>, NodeId>,
    // (2, b) -> {2,3}
    transitions: BTreeMap<NodeId, BTreeMap<Element, BTreeSet<NodeId>>>,
    accepting_states: BTreeSet<NodeId>,
}

#[test]
fn test_product() {
    // let a = DfaBuilder::from_language("ab".to_string().chars().collect());
    // let b = DfaBuilder::from_language("ac".to_string().chars().collect());
    // let _ = DfaBuilder::product(&a, &b);

    let a = DfaBuilder::from_language("a*".to_string().chars().collect());
    let b = DfaBuilder::from_language("*a".to_string().chars().collect());
    let _ = DfaBuilder::product(&a, &b);
}

impl DfaBuilder {
    fn product(a: &Self, b: &Self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let mut b = DfaBuilder::construct_product(a, b);
        let d = b.build_dfa();
        d.graphviz_file("product-dfa.dot", "dfa");
        d
    }

    fn construct_product(a: &Self, b: &Self) -> Self {
        let b = b.offset_self(a.count + 1);
        let mut product = DfaBuilder::new(&a.symbols | &b.symbols);

        /*
               transitions {
                 0: {
                   TokenSet({
                     'a'
                   }): {
                     1
                   }
                 },
                 1: {
                   TokenSet({
                     'b'
                   }): {
                     2
                   }
                 },
                 4: {
                   TokenSet({
                     'a'
                   }): {
                     5
                   }
                 },
                 5: {
                   TokenSet({
                     'c'
                   }): {
                     6
                   }
                 },
                 12: {
                   TokenSet({
                     'a'
                   }): {
                     13
                   }
                 },
                 13: {
                   TokenSet({
                     'b'
                   }): {
                     2
                   },
                   TokenSet({
                     'c'
                   }): {
                     6
                   }
                 },
                 14: {
                   TokenSet({
                     'a'
                   }): {
                     1
                   },
                   TokenSet({
                     'c'
                   }): {
                     6
                   }
                 },
                 15: {
                   TokenSet({
                     'a'
                   }): {
                     5
                   },
                   TokenSet({
                     'b'
                   }): {
                     2
                   }
                 }
               }
        states {{4}: 4, {1}: 1, {1, 5}: 13, {6}: 6, {0, 5}: 14, {15}: 15, {0, 4}: 12, {5}: 5, {12}: 12, {1, 4}: 15, {14}: 14, {0}: 0, {2}: 2, {13}: 13}

                12 (04), 13 (15), 14 (05), 15 (14)


                */

        /*
        0 a-> 1 b-> (2)
        4 a-> 5 c-> (6)

        S  | a  | b | c
        0  | 1  | Ø | Ø
        1  | Ø  | 2 | Ø
        4  | 5  | Ø | Ø
        5  | Ø  | Ø | 6
        04 | 15 | Ø | Ø
        05 | 1  | Ø | 6
        14 | 5  | 2 | Ø
        15 | Ø  | 2 | 6

        entry node is composite of A and B entry
        TODO: accepting states are input accepting states as well as any product of them (chirality)

        04 a-> 15 b -> (2)
                c -> (6)
        */

        // the product id space will be the a space plus the b space offset by the a highest value / count
        // we can copy a as-is, then copy b with an offset on all ids
        //

        // Ri - Li
        // Li - Ri
        // Ri - (Ri - Li)

        // a, a
        //
        // Ri - Li: a - a = Ø
        // Li - Ri: a - a = Ø
        // Ri - (Ri - Li): a - (a - a) = a

        // !a, !b
        // ab, !a!b
        // Ri - Li = !a - !b = !a!b
        // Li - Ri = !b - !a = !a!b
        // Ri - (Ri - Li) =

        // (a node id, b node id) -> product node id
        // product_nodes: HashMap<(Option<NodeId>, Option<NodeId>), NodeId>,

        // for every symbol,
        // for every state in a
        // for every state in b
        //   insert into the product table (Union(δ(a id, symbol), δ(b id, symbol))) ...
        // becomes (Some(product id for a id), Some(product id for b id)))
        // becomes product.states.get({product id for a id, product id for b id})
        // insert into product.transitions -> [(), S, SaSb] -> effectively the union of the targets
        // (target ids must also be mapped into the product id space)

        let mut stack: Vec<BTreeSet<NodeId>> = Default::default();

        product.count = a.count + b.count + 2;

        product.entry = product.compound_id(&BTreeSet::from([a.entry, b.entry]));

        for s in &product.symbols.clone() {
            for (a_id, a_tree) in &a.transitions {
                for (a_e, a_target) in a_tree {
                    let a_accepts_s = a_e.accepts(&s);

                    if a_accepts_s {
                        product.add_transitions(a_id, &s.into(), a_target);
                    }
                    for (b_id, b_tree) in &b.transitions {
                        for (b_e, b_target) in b_tree {
                            let b_accepts_s = b_e.accepts(&s);
                            if b_accepts_s {
                                product.add_transitions(b_id, &s.into(), b_target);
                            }
                            let source_id = product.compound_id(&BTreeSet::from([*a_id, *b_id]));
                            // stack.push(BTreeSet::from([source_id]));
                            println!(
                                "{a_id} {b_id} {a_accepts_s} {b_accepts_s} {a_target:?} {b_target:?}"
                            );
                            match (a_accepts_s, b_accepts_s) {
                                (true, true) => {
                                    let target_id = product.compound_id(&(a_target | b_target));
                                    stack.push(BTreeSet::from([target_id]));
                                    stack.push(a_target | b_target);

                                    product.add_transition(&source_id, &s.into(), &target_id);
                                }
                                (true, false) => {
                                    stack.push(a_target.clone());
                                    product.add_transitions(&source_id, &s.into(), a_target);
                                }
                                (false, true) => {
                                    stack.push(b_target.clone());
                                    product.add_transitions(&source_id, &s.into(), b_target);
                                }
                                (false, false) => {
                                    // product.compound_id(&(a_target | b_target));
                                }
                            }
                        }
                    }
                }
            }
        }
        // println!("lone_a: {lone_a:?}, lone_b: {lone_b:?}");
        // iterate over a transitions
        // iterate over b transitions
        // ???

        // need construction logic here
        product.construct(stack);
        product
    }

    /// Get or set
    pub(crate) fn compound_id(&mut self, compound_state: &BTreeSet<NodeId>) -> NodeId {
        let c = BTreeSet::from_iter(compound_state.to_owned());
        if let Some(id) = self.states.get(&c) {
            *id
        } else {
            // create a new state (index it in self.states) and push it on the stack
            let compound_id = self.next_id();
            self.states
                .insert(BTreeSet::from_iter(compound_state.to_owned()), compound_id);
            compound_id
        }
    }

    // TODO: M
    pub fn build(&mut self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        self.compound_construct();
        let d = self.build_dfa();
        debug_assert!(d.entry == 0);
        d.graphviz_file("dfa.dot", "dfa");
        d
    }

    fn compound_construct(&mut self) {
        // start with all compound values
        let stack: Vec<_> = self
            .transitions
            .values()
            .flat_map(|e| e.values().filter(|v| v.len() > 1).collect::<HashSet<_>>())
            .cloned()
            .collect();

        self.construct(stack);
    }

    /// Stack should include any NodeIds which require traversal to complete transitions
    fn construct(&mut self, mut stack: Vec<BTreeSet<NodeId>>) {
        // TODO: Element -> E
        let negation = Element::NotTokenSet(self.symbols.clone());
        println!("🌮🌮🌮 the stack: {stack:?}\n\n");
        while let Some(compound_state) = stack.pop() {
            // don't process the same compound state twice
            if self.states.contains_key(&compound_state) {
                continue;
            }

            let compound_id = self.compound_id(&compound_state);

            // For every symbol, and additionally also for the negation of all symbols,
            // for every compound state (as non-compound-states are already populated),
            // populate a transition value from the compound state via the symbol
            // by unioning the transitions of its component states for that column
            // where this entails creating a new compound state, push it on the stack
            for symbol in self.symbols.clone() {
                // for every transition from a component state of the compound state,
                // see if it accepts this symbol, and if it does, add it to unioned_transitions
                let mut unioned_transitions: BTreeSet<NodeId> = Default::default();
                for component_state in &compound_state {
                    if let Some(transitions) = self.transitions.get(component_state) {
                        let transitions = transitions.clone();
                        // for every component state, if it has a transition on the negation of all symbols,
                        // union that transition into that negation column for the compound state
                        if let Some(negative) = transitions.get(&negation) {
                            self.add_transitions(&compound_id, &negation, negative);
                            stack.push(negative.clone());
                            // unioned_transitions.extend(negative);
                        }
                        for (element, targets) in &transitions {
                            if element.accepts(&symbol) {
                                unioned_transitions.extend(targets);
                            }
                        }
                        self.add_transitions(
                            &compound_id,
                            &symbol.into(),
                            &unioned_transitions.clone(),
                        );
                        // if unioned_transitions is already processed, this is caught at the beginning of the loop
                        // if it is length 1, then it is already in self.states
                    }
                }
                stack.push(unioned_transitions);
            }
        }
    }

    fn build_dfa(&self) -> Nfa<NfaNode<()>, NfaEdge<Element>> {
        let mut dfa: Nfa<NfaNode<()>, NfaEdge<Element>> = Default::default();
        // builder NodeId -> dfa NodeId
        let mut node_id_map: HashMap<NodeId, NodeId> = Default::default();
        // add a node for each row
        let nodes: HashSet<_> = self.states.values().collect();
        let mut nodes: Vec<_> = nodes.iter().collect();
        nodes.sort();
        // debug_assert!(**nodes[0] == 0);

        let mut accepting_states = HashSet::new();
        for (compound, id) in self.states.clone() {
            if self.accepting_states.contains(&id) {
                accepting_states.insert(id);
                continue;
            }
            for c in compound {
                if self.accepting_states.contains(&c) {
                    accepting_states.insert(id);
                }
            }
        }
        println!(
            "the nodes: {nodes:?} {:?} {accepting_states:?}",
            self.accepting_states
        );
        for builder_node_id in nodes {
            let dfa_node_id = dfa.add_node(NfaNode {
                state: match accepting_states.contains(builder_node_id) {
                    // TODO: M
                    true => Terminal::Accept(()),
                    false => Default::default(),
                },
                ..Default::default()
            });
            node_id_map.insert(**builder_node_id, dfa_node_id);
        }
        println!(
            "symbols: {:?}\nnode id map {node_id_map:?}\ntransitions {:?}\nstates {:?}\nentry {:?}",
            self.symbols, self.transitions, self.states, self.entry
        );
        // add edges after nodes exist
        for (builder_node_id, criteria_targets) in &self.transitions {
            for (criteria, target) in criteria_targets {
                println!("builder_node_id {builder_node_id} criteria {criteria} target {target:?}");
                let t = self.states.get(target).unwrap();
                dfa.add_edge(
                    NfaEdge {
                        criteria: criteria.clone(),
                    },
                    *node_id_map.get(builder_node_id).unwrap(),
                    *node_id_map.get(t).unwrap(),
                );
            }
        }

        dfa.entry = *node_id_map.get(&self.entry).unwrap();

        dfa.simplify();
        dfa
    }

    pub fn new(symbols: BTreeSet<char>) -> Self {
        Self {
            symbols,
            ..Default::default()
        }
    }

    pub fn set_accepting_state(&mut self, node: NodeId) -> bool {
        self.accepting_states.insert(node)
    }

    // The entry node will always be 0
    pub fn next_id(&mut self) -> NodeId {
        let r = self.count;
        self.count += 1;
        r
    }

    // pub fn get_transition(&self, from: &NodeId, e: &Element) -> Option<&BTreeSet<NodeId>> {
    //     self.transitions.get(from).and_then(|t| t.get(e))
    // }

    pub fn add_transitions(&mut self, from: &NodeId, e: &Element, to: &BTreeSet<NodeId>) {
        for t in to {
            self.add_transition(from, e, t);
        }
    }

    pub fn add_transition(&mut self, from: &NodeId, e: &Element, to: &NodeId) {
        // surplus but not incorrect
        self.states.insert(BTreeSet::from([*from]), *from);
        self.states.insert(BTreeSet::from([*to]), *to);

        let entry = match self.transitions.entry(*from) {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(Default::default()),
        };
        entry
            .entry(e.clone())
            .and_modify(|e| {
                e.insert(*to);
            })
            .or_insert_with(|| BTreeSet::from([*to]));
    }

    pub(crate) fn from_language(l: Vec<char>) -> Self {
        let symbols: BTreeSet<_> = l
            .iter()
            .map(|c| if *c == '?' || *c == '*' { ':' } else { *c })
            .collect();

        let mut builder = DfaBuilder::new(symbols);
        let mut prior = builder.next_id();
        debug_assert!(prior == 0);
        builder.entry = prior;

        let colon_free = Element::TokenSet(&builder.symbols - &BTreeSet::from([':']));
        let not_colon_free = Element::NotTokenSet(&builder.symbols | &BTreeSet::from([':']));
        for c in &l {
            let current = builder.next_id();
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
        let mut states: HashMap<_, _> = Default::default();
        for (k, v) in &self.states {
            let k1 = k.iter().map(|u| *u + offset).collect();
            states.insert(k1, v + offset);
        }

        let mut transitions: BTreeMap<_, BTreeMap<_, _>> = Default::default();
        for (k, v) in &self.transitions {
            let k = k + offset;
            for (e, set) in v {
                let set: BTreeSet<_> = set.iter().map(|u| *u + offset).collect();
                transitions
                    .entry(k)
                    .and_modify(|c| {
                        c.insert(e.clone(), set.clone());
                    })
                    .or_insert_with(|| BTreeMap::from_iter([(e.clone(), set)]));
            }
        }
        Self {
            entry: self.entry + offset,
            symbols: self.symbols.clone(),
            count: self.count + offset,
            states,
            transitions,
            accepting_states: self.accepting_states.iter().map(|u| u + offset).collect(),
        }
    }
}

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
  node_1 -> node_4 [label="!:" fontsize="20pt"];
  node_4 -> node_5 [label="b" fontsize="20pt"];
  node_4 -> node_4 [label="!`{':', 'b'}`" fontsize="20pt"];
  node_5 -> node_5 [label="b" fontsize="20pt"];
  node_5 -> node_4 [label="!`{':', 'b'}`" fontsize="20pt"];
  node_0 [label="enter", shape="circle"]
  node_1 [label="1", shape="circle"]
  node_4 [label="4", shape="circle"]
  node_5 [label="5", shape="doublecircle"]
}

*/
