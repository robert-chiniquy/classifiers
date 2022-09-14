use std::collections::BTreeSet;

use std::collections::{BTreeMap, HashSet};

use super::*;

// number of rows/nodes = number of chars in input + 1
// number of columns: number of symbols in input
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

type NodeId = u32;
pub type CompoundId = BTreeSet<NodeId>;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub struct Dfa<M = ()>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    pub(super) entry: CompoundId,
    pub(super) elements: BTreeSet<Element>,
    pub(super) symbols: BTreeSet<char>,
    // for a given element and source, there is only one outbound edge to a target
    // the Vec is here to collect NodeIds during construction?
    pub(super) transitions: BTreeMap<Element, BTreeMap<CompoundId, BTreeSet<CompoundId>>>,
    // Any compound / product node may have several associated accepting states
    // propagated from the source graphs
    // example: if you have the same DFA with the same M, you have 2 copies,
    // in one copy, you change the Accept(M) to a Reject(M),
    // and then you intersect them,
    // you would get both an Accept and a Reject in this set
    pub(super) states: BTreeMap<CompoundId, BTreeSet<State<M>>>,
}

#[test]
fn test_product() {
    // let a = DfaBuilder::from_language("ab".to_string().chars().collect());
    // let b = DfaBuilder::from_language("ac".to_string().chars().collect());
    // let _ = DfaBuilder::product(&a, &b);

    let a = Dfa::<()>::from_language("a*".to_string().chars().collect(), &None);
    let mut b = Dfa::from_language("*a".to_string().chars().collect(), &None);

    let mut i = Dfa::intersect(&a, &mut b);
    let dfa = i.build();

    // assert!(dfa.accepts(&vec!['a', 'a']).unwrap());
    // assert!(dfa.accepts(&vec!['a', 'a', 'a']).unwrap());
    // assert!(!dfa.accepts(&vec!['a', 'a', 'b']).unwrap());
    // assert!(!dfa.accepts(&vec!['a']).unwrap());
}

impl<M> Dfa<M>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    pub(crate) fn from_language(l: Vec<char>, m: &Option<M>) -> Self {
        let symbols: BTreeSet<_> = l
            .iter()
            .map(|c| if *c == '?' || *c == '*' { ':' } else { *c })
            .collect();

        let mut builder = Dfa::new(symbols.clone());
        let mut prior = 0;

        builder.entry = CompoundId::from([prior]);

        let colon_free = Element::TokenSet(&symbols - &BTreeSet::from([':']));
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

        for i in 0..prior {
            builder.add_state(&CompoundId::from([i]), State::InverseInclude(m.clone()));
        }
        builder.add_state(&CompoundId::from([prior]), State::Include(m.clone()));
        builder.calculate_transitions(builder.find_compound_ids());
        builder
    }

    /// Calling this method requires setting accepting states externally
    pub fn new(symbols: BTreeSet<char>) -> Self {
        let mut elements: BTreeSet<Element> = symbols.iter().map(|s| s.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        Self {
            symbols,
            elements,
            entry: Default::default(),
            transitions: Default::default(),
            states: Default::default(),
        }
    }

    pub(crate) fn product(a: &Self, b: &Self) -> Self {
        let mut b = b.clone();
        let product = Dfa::construct_product(a, &mut b);
        let d = product.build_dfa();
        d.graphviz_file("product-dfa.dot", "dfa");
        d
    }

    // TODO: why make b mutable?
    pub(crate) fn construct_product(a: &Self, b: &mut Self) -> Self {
        let symbols: BTreeSet<_> = &a.symbols | &b.symbols;

        let mut elements: BTreeSet<Element> = symbols.iter().map(|c| c.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        let offset = a.ids().iter().flatten().max().unwrap_or(&1) + 1;
        b.offset_self(offset);

        // Combining the accepting states requires that any offset must already have occurred
        let mut accepting_states = a.states.clone();
        accepting_states.extend(b.states.clone().into_iter());

        let mut product = Self {
            symbols,
            elements,
            states: accepting_states,
            entry: &a.entry | &b.entry,
            transitions: Default::default(),
        };

        let mut stack: Vec<BTreeSet<NodeId>> = Default::default();

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
                            // Where we construct a new CompoundId,
                            // we must also compound the accepting states
                            // of the source NodeIds
                            let compound_id = a_from | b_from;
                            stack.push(compound_id.clone());
                            let states =
                                a.states.get(a_from).unwrap() | b.states.get(b_from).unwrap();
                            product.add_states(&compound_id, states);
                            let mut to: CompoundId = Default::default();
                            a_toos
                                .iter()
                                .chain(b_toos.iter())
                                .for_each(|s| to.extend(s));

                            product._add_transition(&compound_id, &e.clone(), &to);
                            a_toos
                                .iter()
                                .for_each(|to| product._add_transition(a_from, &e.clone(), to));
                            b_toos
                                .iter()
                                .for_each(|to| product._add_transition(b_from, &e.clone(), to));
                        });
                    });
                }
            }
        }

        product.calculate_transitions(stack);
        product
    }

    // Require that a and b both have accepting states
    pub fn intersect(a: &Self, b: &mut Self) -> Self {
        let mut product = Self::construct_product(a, b);

        product.states = Default::default();

        println!("a accepts: {:?}", a.states);
        println!("b accepts: {:?}", b.states);

        let both_accept = |id: &CompoundId| -> bool {
            let mut a_accepts = false;
            let mut b_accepts = false;

            for i in id {
                let c_id = CompoundId::from([*i]);
                if let Some(states) = a.states.get(&c_id) {
                    a_accepts = a_accepts || states.iter().any(|s| s.accepting());
                } else if let Some(states) = b.states.get(&c_id) {
                    b_accepts = b_accepts || states.iter().any(|s| s.accepting());
                }
            }
            a_accepts & b_accepts
        };

        for id in &product.ids() {
            if both_accept(id) {
                // cheaper to redo the logic here, probably...
                let mut all_states = Default::default();
                for i in id {
                    let c_id = CompoundId::from([*i]);
                    if let Some(states) = a.states.get(&c_id) {
                        all_states = &all_states | states;
                    } else if let Some(states) = b.states.get(&c_id) {
                        all_states = &all_states | states;
                    }
                }
                product.add_states(id, all_states);
            }
        }

        println!("intersecting states: {:?}", product.states);
        product
    }

    pub fn find_compound_ids(&self) -> Vec<CompoundId> {
        self.ids().iter().filter(|v| v.len() > 1).cloned().collect()
    }

    pub fn build(&mut self) -> Self {
        let d = self.build_dfa();
        d.graphviz_file("dfa.dot", "dfa");
        d
    }

    fn calculate_transitions(&mut self, mut stack: Vec<CompoundId>) {
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

                let mut states = Default::default();
                for id in &unioned_transitions {
                    states = &states | self.states.get(&CompoundId::from([*id])).unwrap();
                }
                // self.add_states(&unioned_transitions, unioned_transitions.iter().flat_map(|t| self.accepting_states.get(&CompoundId::from([*t])).unwrap()).collect::<BTreeSet<_>>());
                self.add_states(&unioned_transitions, states);

                stack.push(unioned_transitions);
            }
        }
    }

    pub(super) fn ids(&self) -> HashSet<CompoundId> {
        let mut ids: HashSet<CompoundId> = Default::default();

        for (_, transitions) in self.transitions.clone() {
            for (from, to) in transitions {
                ids.insert(from);
                for c_id in to {
                    ids.insert(c_id);
                }
            }
        }
        ids
    }

    fn build_dfa(&self) -> Self {
        let mut dfa = self.clone();
        dfa.simplify();
        dfa
    }

    pub fn add_state(&mut self, node: &CompoundId, state: State<M>) {
        self.states
            .entry(node.to_owned())
            .and_modify(|t| {
                t.insert(state.clone());
            })
            .or_insert_with(|| BTreeSet::from([state.clone()]));
    }

    pub fn add_states(&mut self, node: &CompoundId, states: BTreeSet<State<M>>) {
        self.states
            .entry(node.to_owned())
            .and_modify(|t| {
                t.extend(states.clone());
            })
            .or_insert_with(|| states);
    }

    pub(super) fn _add_transition(&mut self, from: &CompoundId, e: &Element, to: &CompoundId) {
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
                        .and_modify(|_from| {
                            _from.insert(to.clone());
                        })
                        .or_insert_with(|| BTreeSet::from([to.clone()]));
                })
                .or_insert_with(|| BTreeMap::from([(from.clone(), BTreeSet::from([to.clone()]))]));
        }

        // println!("after: {:?}", self.transitions);
        if no_e {
            panic!("did not add a transition for {from:?} -{e:?}-> {to:?}");
        }
    }

    pub fn add_transition(&mut self, from: &NodeId, e: &Element, to: &NodeId) {
        self._add_transition(&BTreeSet::from([*from]), e, &BTreeSet::from([*to]));
    }

    pub(crate) fn offset_self(&mut self, offset: u32) {
        let mut transitions: BTreeMap<Element, BTreeMap<CompoundId, BTreeSet<CompoundId>>> =
            Default::default();

        for (element, v) in &self.transitions {
            transitions.insert(element.clone(), Default::default());
            for (from, to) in v {
                let from: CompoundId = from.iter().map(|id| *id + offset).collect();
                let to: BTreeSet<CompoundId> = to
                    .iter()
                    .map(|vec| vec.iter().map(|id| *id + offset).collect())
                    .collect();

                transitions
                    .get_mut(&element.clone())
                    .unwrap()
                    .insert(from.clone(), to.clone());
            }
        }
        self.transitions = transitions;
        self.entry = self.entry.iter().map(|id| *id + offset).collect();
        self.states = self
            .states
            .iter()
            .map(|(k, s)| (k.iter().map(|u| u + offset).collect(), s.clone()))
            .collect();
    }

    pub(crate) fn shake(&mut self) {
        let alive = self.get_edges().accepting_branches(self);
        println!("shake: alive: {alive:?}");

        let mut dead_elements = vec![];
        for (element, edges) in self.transitions.iter_mut() {
            edges.retain(|k, _| alive.contains(k));
            for (_, targets) in edges.iter_mut() {
                targets.retain(|t| alive.contains(t));
            }
            edges.retain(|_, v| !v.is_empty());
            if edges.is_empty() {
                dead_elements.push(element.clone());
            }
        }

        for e in dead_elements {
            self.transitions.remove(&e);
        }

        self.states.retain(|k, _v| alive.contains(k));
    }

    pub(super) fn get_edges(&self) -> EdgeIndex {
        // Build a convenient map of edges indexed differently
        let mut map: BTreeMap<CompoundId, BTreeMap<Element, BTreeSet<CompoundId>>> =
            Default::default();
        for (element, edges) in &self.transitions {
            for (from, to) in edges {
                map.entry(from.clone())
                    .and_modify(|e| {
                        e.insert(element.clone(), to.clone());
                    })
                    .or_insert_with(|| BTreeMap::from([(element.clone(), to.clone())]));
            }
        }
        EdgeIndex(map)
    }

    pub(crate) fn simplify(&mut self) {
        self.shake();
        //     pub fn simplify(&mut self) {
        //         for (s, targets) in &self.transitions.clone() {
        //             if targets.len() <= 1 {
        //                 continue;
        //             }
        //             // let target_map = targets.iter().cloned().collect::<HashMap<u32, Vec<u64>>>();
        //             let mut collected_targets: HashMap<_, Vec<_>> = HashMap::new();
        //             for (t, e) in targets {
        //                 collected_targets
        //                     .entry(t)
        //                     .and_modify(|tt| tt.push(e))
        //                     .or_insert_with(|| vec![e]);
        //             }

        //             let mut positives = BTreeSet::new();
        //             let mut negatives = BTreeSet::new();
        //             for (target, ees) in collected_targets {
        //                 if ees.len() <= 1 {
        //                     continue;
        //                 }
        //                 for e in &ees {
        //                     match self.edge(e).map(|edge| &edge.criteria) {
        //                         Some(Element::TokenSet(ref s)) => {
        //                             positives = &positives | s;
        //                         }
        //                         Some(Element::NotTokenSet(ref s)) => {
        //                             negatives = &negatives | s;
        //                         }
        //                         None => unreachable!(),
        //                     }
        //                 }
        //                 // println!("positives {positives:?} negatives: {negatives:?}");
        //                 // abd !a!b!c  -> d, !c
        //                 let overlapping = &negatives - &positives;
        //                 negatives = overlapping.clone();
        //                 positives = &positives - &overlapping;
        //                 // println!("modified: positives {positives:?} negatives: {negatives:?}");
        //                 for e in ees {
        //                     self.remove_edge(*s, *e);
        //                 }

        //                 if !negatives.is_empty() {
        //                     self.add_edge(
        //                         NfaEdge {
        //                             criteria: Element::NotTokenSet(negatives.clone()),
        //                         },
        //                         *s,
        //                         *target,
        //                     );
        //                 } else if !positives.is_empty() {
        //                     self.add_edge(
        //                         NfaEdge {
        //                             criteria: Element::TokenSet(positives.clone()),
        //                         },
        //                         *s,
        //                         *target,
        //                     );
        //                 } else {
        //                     self.add_edge(
        //                         NfaEdge {
        //                             criteria: Element::NotTokenSet(negatives.clone()),
        //                         },
        //                         *s,
        //                         *target,
        //                     );
        //                 }
        //             }
        //         }
        //         self.shake();
        //     }
        // }
    }
}

pub(super) struct EdgeIndex(
    pub(crate) BTreeMap<CompoundId, BTreeMap<Element, BTreeSet<CompoundId>>>,
);

impl EdgeIndex {
    /// Return all CompoundIds which are in an accepting path
    pub fn accepting_branches<M>(&self, dfa: &Dfa<M>) -> HashSet<CompoundId>
    where
        M: std::fmt::Debug + PartialOrd + Ord + Clone,
    {
        let mut visited: HashSet<CompoundId> = Default::default();
        let mut alive: HashSet<CompoundId> = Default::default();
        self._accepting_branch(&dfa.entry, &mut visited, &mut alive, dfa);
        alive
    }

    /// Find all accepting branches under a subtree
    fn _accepting_branch<M>(
        &self,
        id: &CompoundId,
        visited: &mut HashSet<CompoundId>,
        alive: &mut HashSet<CompoundId>,
        dfa: &Dfa<M>,
    ) -> bool
    where
        M: std::fmt::Debug + PartialOrd + Ord + Clone,
    {
        if visited.contains(id) {
            return alive.contains(id);
        }
        visited.insert(id.clone());

        let mut is_alive = false;
        if let Some(states) = dfa.states.get(id) {
            if states.iter().any(|s| s.accepting()) {
                alive.insert(id.clone());
                is_alive = true;
            }
        }

        if let Some(map) = self.0.get(id) {
            map.clone().iter().for_each(|(_, targets)| {
                for t in targets {
                    if self._accepting_branch(t, visited, alive, dfa) {
                        is_alive = true;
                        alive.insert(id.clone());
                    }
                }
            });
        }
        is_alive
    }
}
