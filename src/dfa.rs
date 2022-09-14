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
// where ever you have columns such as b and ab,
// a transition of ab from a node counts as a transition of b as well
// for the purpose of unioning the resulting transitions

type NodeId = u32;
pub type CompoundId = BTreeSet<NodeId>;
// #[derive(Default, PartialOrd, Ord, PartialEq, Eq, Clone, std::hash::Hash)]
// pub struct CompoundId(BTreeSet<NodeId>);

// impl std::fmt::Debug for CompoundId {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let s =  self.0.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("_");
//         f.write_str(&s)
//     }
// }

// impl CompoundId {
//     pub fn id(&self) -> &BTreeSet<NodeId> {
//         &self.0
//     }

//     pub fn id_mut(&mut self) -> &mut BTreeSet<NodeId> {
//         &mut self.0
//     }

//     pub fn from<const N: usize>(a: [NodeId; N]) -> Self {
//         Self(BTreeSet::from(a))
//     }

//     pub fn iter(&self) -> std::collections::btree_set::Iter<'static, NodeId> {
//         self.0.iter()
//     }

//     pub fn iter_mut(&mut self) -> std::collections::btree_set::Iter<'static, NodeId> {
//         self.0.iter()
//     }
// }

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
    let _dfa = i.build();

    // assert!(dfa.accepts(&vec!['a', 'a']).unwrap());
    // assert!(dfa.accepts(&vec!['a', 'a', 'a']).unwrap());
    // assert!(!dfa.accepts(&vec!['a', 'a', 'b']).unwrap());
    // assert!(!dfa.accepts(&vec!['a']).unwrap());
}

#[test]
fn test_from_language_simple() {
    let mut starb = Dfa::<()>::from_language("*B".to_string().chars().collect(), &None);
    starb.simplify();
    starb.graphviz_file("starB.dot", "*B");

    assert!(starb.is_consistent());
    // starb.simplify();
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

        let positive_star = Element::TokenSet(&symbols - &BTreeSet::from([':']));
        let negative_star = Element::NotTokenSet(&symbols | &BTreeSet::from([':']));

        let mut stack: Vec<CompoundId> = Default::default();
        // construct an NFA (with or without epsilons is the same)
        for c in &l {
            let current = prior + 1;

            match c {
                '?' => {
                    // transition via all symbols from prior to current
                    builder.add_transition(&prior, &positive_star, &current);
                    builder.add_transition(&prior, &negative_star, &current);
                }
                '*' => {
                    // self loop/advance
                    let source = CompoundId::from([prior]);
                    let target = CompoundId::from([prior, current]);
                    stack.push(target.clone());

                    builder.add_transitions(&source, &positive_star, &target);
                    builder.add_transitions(&source, &negative_star, &target);
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
        // convert the NFA to a DFA
        builder.calculate_transitions(stack);
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

    pub fn includes_path(&self, path: &[Element]) -> bool {
        let edges = self.get_edges();
        let path_index = 0;
        let mut stack = vec![(path_index, self.entry.clone())];

        while let Some((path_index, current)) = stack.pop() {
            if path.len() >= path_index {
                // check if current is an Include?
                match self.states.get(&current) {
                    // did not find an accepting state on this path within the scope of the input
                    None => continue,
                    Some(s) => {
                        for state in s {
                            match state {
                                State::Include(_) => return true,
                                // did not find an accepting state on this path within the scope of the input
                                State::InverseExclude(_)
                                | State::InverseInclude(_)
                                | State::Exclude(_) => continue,
                            }
                        }
                    }
                }
                continue;
            }
            match edges.0.get(&current) {
                Some(edges) => {
                    for (element, targets) in edges {
                        if element.accepts(&path[path_index]) {
                            for t in targets {
                                stack.push((path_index + 1, t.clone()));
                            }
                        }
                    }
                }
                None => (),
            }
        }
        false
    }

    // pub fn excludes_path() -> bool {

    // }

    pub(crate) fn product(a: &Self, b: &Self) -> Self {
        let mut b = b.clone();
        let product = Dfa::construct_product(a, &mut b);
        let d = product.build_dfa();
        d.graphviz_file("product-dfa.dot", "dfa");
        d
    }

    fn write_into_language(&mut self, language: &BTreeSet<Element>) {
        // !a!:, !b!:, -> !a!b!:
        let mut new_transitions: BTreeMap<Element, BTreeMap<CompoundId, BTreeSet<CompoundId>>> =
            Default::default();

        for word in language {
            for (element, edges) in self.transitions.clone() {
                if element.accepts(word) {
                    new_transitions
                        .entry(word.clone())
                        .and_modify(|existing_edges| {
                            existing_edges.extend(edges.clone());
                        })
                        .or_insert(edges.clone());
                }
            }
        }
        println!(
            "transitions for {language:?}:\n{:?}\n{:?}",
            self.transitions, new_transitions
        );
        self.transitions = new_transitions;
    }

    // TODO: why make b mutable?
    pub(crate) fn construct_product(a: &Self, b: &mut Self) -> Self {
        let symbols: BTreeSet<_> = &a.symbols | &b.symbols;

        let mut elements: BTreeSet<Element> = symbols.iter().map(|c| c.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        let mut a = a.clone();
        a.write_into_language(&elements);
        b.write_into_language(&elements);
        // let a = a.rewrite_with_symbols(symbols);

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
                        .for_each(|to| product.add_transitions(from, e, to))
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

                            product.add_transitions(&compound_id, &e.clone(), &to);
                            a_toos
                                .iter()
                                .for_each(|to| product.add_transitions(a_from, &e.clone(), to));
                            b_toos
                                .iter()
                                .for_each(|to| product.add_transitions(b_from, &e.clone(), to));
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
                for i in id.iter() {
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

    /// Walk the transitions table for a stack of rows to create product states
    /// and rationalize transitions into a DFA
    fn calculate_transitions(&mut self, mut stack: Vec<CompoundId>) {
        // println!(
        //     "🌮🌮🌮 the stack: {stack:?}\n🌮🌮🌮 transitions: {:?}\n\n",
        //     self.transitions
        // );

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
                self.add_transitions(&compound_state, &element, &unioned_transitions.clone());

                let mut states = Default::default();
                for id in &unioned_transitions {
                    states = &states | self.states.get(&CompoundId::from([*id])).unwrap();
                }
                // self.add_states(&unioned_transitions, unioned_transitions.iter().flat_map(|t| self.accepting_states.get(&CompoundId::from([*t])).unwrap()).collect::<BTreeSet<_>>());
                self.add_states(&unioned_transitions, states);

                stack.push(unioned_transitions);
            }
        }
        // println!("transitions: {:#?}", self.transitions);
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

    pub(super) fn add_transitions(&mut self, from: &CompoundId, e: &Element, to: &CompoundId) {
        let mut no_e = true;
        // println!("before: {:?}", self.transitions);
        for element in &self.elements {
            if !e.accepts(element) {
                continue;
            }
            no_e = false;
            self.transitions
                .entry(element.clone())
                .or_default()
                .entry(from.clone())
                .or_default()
                .insert(to.clone());
        }

        // println!("after: {:?}", self.transitions);
        if no_e {
            panic!("did not add a transition for {from:?} -{e:?}-> {to:?}\n\n{self:?}");
        }
    }

    /// This function assumes a CompoundId of only {from} and {to} respectively!
    pub fn add_transition(&mut self, from: &NodeId, e: &Element, to: &NodeId) {
        self.add_transitions(&BTreeSet::from([*from]), e, &BTreeSet::from([*to]));
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

        let by_edge = self.get_edges().0;
        self.transitions = Default::default();

        for (source, edges) in &by_edge {
            let mut targets_to_edges: BTreeMap<CompoundId, Vec<&Element>> = Default::default();

            for (element, targets) in edges {
                for t in targets {
                    targets_to_edges
                        .entry(t.clone())
                        .and_modify(|v| v.push(element))
                        .or_insert_with(|| vec![element]);
                }
            }

            for (target, elements) in targets_to_edges {
                // source -> target -> element
                let mut positives = BTreeSet::new();
                let mut negatives = BTreeSet::new();
                for element in elements {
                    match element {
                        Element::TokenSet(ref s) => {
                            positives = &positives | s;
                        }
                        Element::NotTokenSet(ref s) => {
                            negatives = &negatives | s;
                        }
                    }
                }
                let overlapping = match (!negatives.is_empty(), !positives.is_empty()) {
                    (true, true) => Element::NotTokenSet(&negatives - &positives),
                    (true, false) => Element::NotTokenSet(negatives),
                    (false, true) => Element::TokenSet(positives),
                    (false, false) => continue,
                };

                self.transitions
                    .entry(overlapping)
                    .or_default()
                    .entry(source.clone())
                    .or_default()
                    .insert(target);
            }
        }
    }

    /// Return true if there are no overlapping edges out of a given node
    #[cfg(test)]
    fn is_consistent(&self) -> bool {
        for (s, edges) in self.get_edges().0 {
            for (i1, (e1, _)) in edges.clone().iter().enumerate() {
                for (i2, (e2, _)) in edges.clone().iter().enumerate() {
                    if i1 >= i2 {
                        continue;
                    }
                    if e1.accepts(e2) || e2.accepts(e1) {
                        print!("{s:?} {e1:?} {e2:?} {i1} v {i2}");
                        return false;
                    }
                }
            }
        }
        true
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
