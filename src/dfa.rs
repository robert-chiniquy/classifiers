use std::collections::BTreeSet;

use std::collections::{BTreeMap, HashSet};
use std::vec;

use super::*;

type NodeId = u32;
// for use in powerset construction
pub type UnionedId = BTreeSet<NodeId>;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub struct Dfa<M = ()>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    pub(super) entry: UnionedId,
    pub(super) elements: BTreeSet<Element>,
    pub(super) symbols: BTreeSet<char>,
    // FIXME: for a given element and source, there is only one outbound edge to a target
    // the BTreeSet is here to collect NodeIds during construction
    pub(super) transitions: BTreeMap<Element, BTreeMap<UnionedId, BTreeSet<UnionedId>>>,
    // Any compound / product node may have several associated accepting states
    // propagated from the source graphs
    // example: if you have the same DFA with the same M, you have 2 copies,
    // in one copy, you change the Include(M) to a Exclude(M),
    // and then you intersect them, you would get both an Include and a Exclude in this set
    pub(super) states: BTreeMap<UnionedId, BTreeSet<State<M>>>,
}

#[test]
fn test_from_language_simple() {
    let mut star_amp = Dfa::<()>::from_language("*&f".to_string().chars().collect(), &None);
    star_amp.simplify();
    star_amp.graphviz_file("star_amp.dot", "*&f");

    let accepting_states = star_amp.accepting_states();

    assert!(star_amp.is_consistent());

    assert_eq!(accepting_states.len(), 1, "too many states: {accepting_states:?}");

    assert!(star_amp.includes_path(&[
        Element::token('&'),
        Element::token('&'),
        Element::token('&'),
        Element::token('f'),
    ]));

    let mut starb = Dfa::<()>::from_language("*B".to_string().chars().collect(), &None);
    starb.simplify();
    starb.graphviz_file("starB.dot", "*B");

    assert!(starb.is_consistent());
    let accepting_states = starb.accepting_states();
    assert_eq!(accepting_states.len(), 1, "too many states: {accepting_states:?}");
    starb.graphviz_file("bbb-starb.dot", "starb");
    assert!(!starb.includes_path(&[Element::token('B')]));
    assert!(
        starb.includes_path(&[Element::token('B'), Element::token('B')]),
        "bad starb :( {starb:#?}"
    );

    // starb.simplify();

    let mut astar = Dfa::<()>::from_language("a*".to_string().chars().collect(), &None);
    astar.simplify();
    astar.graphviz_file("astar.dot", "a*");

    let accepting_states = astar.accepting_states();
    assert!(astar.is_consistent());
    assert_eq!(accepting_states.len(), 1, "too many states: {accepting_states:?}");
    assert!(!astar.includes_path(&[Element::token('a')]));
    assert!(astar.includes_path(&[Element::token('a'), Element::token('a')]));

    let mut fstarfstar = Dfa::<()>::from_language("f*f*".to_string().chars().collect(), &None);
    fstarfstar.simplify();
    fstarfstar.graphviz_file("fstarfstar.dot", "f*f*");

    let accepting_states = fstarfstar.accepting_states();
    assert!(fstarfstar.is_consistent());
    assert_eq!(accepting_states.len(), 1, "too many states: {accepting_states:?}");

    let mut abcdefg = Dfa::<()>::from_language("a*abcdefg&&".to_string().chars().collect(), &None);
    abcdefg.simplify();
    abcdefg.graphviz_file("abcdefg.dot", "a*abcdefg&&");

    let accepting_states = abcdefg.accepting_states();
    assert!(abcdefg.is_consistent());
    assert_eq!(accepting_states.len(), 1, "too many states: {accepting_states:?}");
    // assert!(!astar.includes_path(&[Element::token('a')]));
    // assert!(astar.includes_path(&[Element::token('a'), Element::token('a')]));
}

impl<M> Dfa<M>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    /// Calling this method requires setting accepting states externally
    #[tracing::instrument]
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

    #[tracing::instrument]
    pub(crate) fn from_language(l: Vec<char>, m: &Option<M>) -> Self {
        if l.is_empty() {
            println!("\n\nnones");
            return Self::none(m);
        }

        let symbols: BTreeSet<_> =
            l.iter().map(|c| if *c == '?' || *c == '*' { ':' } else { *c }).collect();

        let mut builder = Dfa::new(symbols.clone());
        let mut prior = 0;

        builder.entry = UnionedId::from([prior]);
        builder.add_state(&builder.entry.clone(), State::InverseInclude(m.clone()));

        let positives = &symbols - &BTreeSet::from([':']);
        let negative_star = Element::NotTokenSet(&symbols | &BTreeSet::from([':']));

        let mut stack: Vec<UnionedId> = Default::default();
        // construct an NFA (with or without epsilons is the same)
        for (i, c) in l.iter().enumerate() {
            let current = prior + 1;
            let s = if i == l.len() - 1 {
                State::Include(m.clone())
            } else {
                State::InverseInclude(m.clone())
            };
            match c {
                '?' => {
                    // transition via all symbols from prior to current
                    for c in &positives {
                        builder.add_transition_with_state(
                            &prior,
                            &Element::token(*c),
                            &current,
                            &s,
                        );
                    }

                    builder.add_transition_with_state(&prior, &negative_star, &current, &s);
                }
                '*' => {
                    // self loop/advance
                    let source = UnionedId::from([prior]);
                    let target = UnionedId::from([prior, current]);

                    // unioned id goes on the stack to expand...
                    stack.push(target.clone());

                    // builder.add_state(&source, State::InverseInclude(m.clone()));
                    // builder.add_state(&target, s);
                    builder.add_state(&UnionedId::from([current]), s.clone());
                    for c in &positives {
                        builder.add_transition2_with_states(
                            &source,
                            &Element::token(*c),
                            &target,
                            &BTreeSet::from([s.clone()]),
                        );
                    }
                    builder.add_transition2_with_states(
                        &source,
                        &negative_star,
                        &target,
                        &BTreeSet::from([s.clone()]),
                    );
                }
                c => {
                    builder.add_transition_with_state(&prior, &c.into(), &current, &s);
                }
            }
            prior = current;
        }

        // println!("states: {:?}", builder.states);
        builder.add_state(&UnionedId::from([prior]), State::Include(m.clone()));

        // convert the NFA to a DFA
        builder.powerset_construction(stack);
        builder
    }

    pub fn includes_string(&self, path: &str) -> bool {
        self.includes_path(&path.chars().map(|c| c.into()).collect::<Vec<Element>>())
    }

    pub fn excludes_string(&self, path: &str) -> bool {
        self.excludes_path(&path.chars().map(|c| c.into()).collect::<Vec<Element>>())
    }

    // TODO: Should return M
    #[tracing::instrument(skip(self))]
    pub fn includes_path(&self, path: &[Element]) -> bool {
        self.accepting_path(path, &mut |states| {
            for state in states {
                match state {
                    State::Include(_m) => return true,
                    State::InverseExclude(_) | State::InverseInclude(_) | State::Exclude(_) => {
                        continue;
                    }
                }
            }
            false
        })
    }

    // TODO: Should return M
    #[tracing::instrument(skip(self))]
    pub fn excludes_path(&self, path: &[Element]) -> bool {
        self.accepting_path(path, &mut |states| {
            for state in states {
                match state {
                    State::Exclude(_m) => return true,
                    State::InverseExclude(_) | State::InverseInclude(_) | State::Include(_) => {
                        continue;
                    }
                }
            }
            false
        })
    }

    #[tracing::instrument(skip(self, check_states))]
    pub fn accepting_path(
        &self,
        path: &[Element],
        check_states: &mut impl Fn(&BTreeSet<State<M>>) -> bool,
    ) -> bool {
        let edges = self.get_edges().0;
        let mut stack = vec![(0, self.entry.clone())];

        while let Some((path_index, current)) = stack.pop() {
            // println!("{} {:?} {:?}", path_index, current, path[path_index]);
            if path.len() == path_index {
                // check if current is an Include?
                match self.states.get(&current) {
                    // did not find an accepting state on this path within the scope of the input
                    None => continue,
                    Some(s) => {
                        if check_states(&s) {
                            return true;
                        }
                        // did not find an accepting state on this path within the scope of the input
                        continue;
                    }
                }
            }
            match edges.get(&current) {
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

    #[tracing::instrument(skip(self))]
    fn write_into_language(&mut self, language: &BTreeSet<Element>) {
        // !a!:, !b!:, -> !a!b!:
        let mut new_transitions: BTreeMap<Element, BTreeMap<UnionedId, BTreeSet<UnionedId>>> =
            Default::default();

        for word in language {
            for (element, edges) in self.transitions.clone() {
                if element.accepts(word) {
                    new_transitions
                        .entry(word.clone())
                        .and_modify(|existing_edges| {
                            existing_edges.extend(edges.clone());
                        })
                        .or_insert_with(|| edges.clone());
                }
            }
        }
        // println!(
        //     "transitions for {language:?}:\n{:?}\n{:?}",
        //     self.transitions, new_transitions
        // );
        self.transitions = new_transitions;
    }

    // FIXME: why make b mutable?
    #[tracing::instrument(skip_all)]
    pub(crate) fn product(a: &Self, b: &mut Self) -> Self {
        debug_assert!(a.is_consistent(), "{a:?}");
        debug_assert!(b.is_consistent(), "{b:?}");

        let symbols: BTreeSet<_> = &a.symbols | &b.symbols;

        let mut elements: BTreeSet<Element> = symbols.iter().map(|c| c.into()).collect();
        elements.insert(Element::NotTokenSet(symbols.clone()));

        let mut a = a.clone();
        a.write_into_language(&elements);
        b.write_into_language(&elements);

        let offset = a.ids().iter().flatten().max().unwrap_or(&1) + 10;
        b.offset_self(offset);

        // Combining the accepting states requires that any offset must already have occurred
        // a and b must already contain states for every extant NodeId
        let mut accepting_states = a.states.clone();
        accepting_states.extend(b.states.clone().into_iter());

        let mut transitions = a.transitions.clone();
        transitions.extend(b.transitions.clone().into_iter());
        // println!(
        //     "a states: {:?}\nb states: {:?}\nboth: {:?}",
        //     a.states,
        //     b.states,
        //     accepting_states.clone()
        // );
        // println!(
        //     "a transitions: {:?}\nb transitions: {:?}\nboth: {:?}",
        //     a.transitions,
        //     b.transitions,
        //     transitions.clone()
        // );
        // println!(
        //     "a entry: {:?}\nb entry: {:?}\nboth: {:?}",
        //     a.entry,
        //     b.entry,
        //     (&a.entry | &b.entry).clone()
        // );

        let mut product = Self {
            symbols,
            elements,
            states: accepting_states,
            entry: &a.entry | &b.entry,
            transitions: transitions,
        };

        let mut stack = vec![product.entry.clone()];

        for e in &product.elements.clone() {
            let a_transitions = a.transitions.get(e);
            let b_transitions = b.transitions.get(e);

            match (a_transitions, b_transitions) {
                (None, None) => {},
                (Some(a_t), Some(b_t)) => {
                    a_t.iter().for_each(|(a_from, a_toos)| {
                        b_t.iter().for_each(|(b_from, b_toos)| {
                            let compound_id = a_from | b_from;
                            stack.push(compound_id.clone());
                            let states =
                                a.states.get(a_from).unwrap() | b.states.get(b_from).unwrap();
                            let to = (a_toos | b_toos).into_iter().flatten().collect();
                            product.add_transition2_with_states(
                                &compound_id,
                                &e.clone(),
                                &to,
                                &states,
                            );
                            a_toos.iter().for_each(|to| {
                                product.add_transition2_with_states(a_from, &e.clone(), to, &states)
                            });
                            b_toos.iter().for_each(|to| {
                                product.add_transition2_with_states(b_from, &e.clone(), to, &states)
                            });
                        });
                    });
                }
                // NOTE:
                //  we must seed the transitions out of the product entry here and below because 
                //  the double for loop misses it above...
                (None, Some(b_t)) => {
                    if let Some(targets) = b_t.get(&b.entry) {
                        for t in targets {
                            let state = b.states.get(&t).unwrap();
                            product.add_transition2_with_states(
                                &product.entry.clone(),
                                &e.clone(),
                                &t,
                                state,
                            );
                        }
                    }
                },
                (Some(a_t), None) => {
                    if let Some(targets) = a_t.get(&a.entry) {
                        for t in targets {
                            let state = a.states.get(&t).unwrap();
                            product.add_transition2_with_states(
                                &product.entry.clone(),
                                &e.clone(),
                                &t,
                                state,
                            );
                        }
                    }
                },
            }
        }

        product.powerset_construction(stack);
        product
    }

    #[tracing::instrument(skip_all)]
    pub fn intersect(a: &Self, b: &Self) -> Self {
        Self::painting_product(a, b, &mut |_, a_states, b_states| {
            // the intersection requires that accepting states only be propagated
            // when the states are accepting on both sides
            if a_states.is_empty() || b_states.is_empty() {
                return Default::default();
            }
            &a_states | &b_states
        })
    }

    // Require that a and b both have accepting states
    /// Receives a closure which can optionally paint all states, paint only intersecting states, or whatever
    #[tracing::instrument(skip_all)]
    pub(super) fn painting_product(
        a: &Self,
        b: &Self,
        painter: &mut impl FnMut(
            &UnionedId,
            BTreeSet<State<M>>,
            BTreeSet<State<M>>,
        ) -> BTreeSet<State<M>>,
    ) -> Self {
        let mut b = b.clone();
        let mut product = Self::product(a, &mut b);
        product.simplify();
        product.states = Default::default();

        let accepting_left = a
            .states
            .iter()
            .map(|(id, states)| {
                (
                    id.clone(),
                    states.iter().filter(|s| s.accepting()).cloned().collect::<BTreeSet<_>>(),
                )
            })
            .filter(|(_, v)| !v.is_empty())
            .collect::<BTreeMap<UnionedId, BTreeSet<State<M>>>>();

        let accepting_right = b
            .states
            .iter()
            .map(|(id, states)| {
                (
                    id.clone(),
                    states.iter().filter(|s| s.accepting()).cloned().collect::<BTreeSet<_>>(),
                )
            })
            .filter(|(_, v)| !v.is_empty())
            .collect::<BTreeMap<UnionedId, BTreeSet<State<M>>>>();

        // println!("a accepts:\n{:?}\nal: {:?}", a.states, accepting_left);
        // println!("b accepts:\n{:?}\nar: {:?}", b.states, accepting_right);

        for id in &product.ids() {
            let mut left: BTreeSet<State<M>> = Default::default();
            let mut right: BTreeSet<State<M>> = Default::default();

            if let Some(states) = accepting_left.get(id) {
                left.extend(states.iter().filter(|s| s.accepting()).cloned());
            } else if let Some(states) = accepting_right.get(id) {
                right.extend(states.iter().filter(|s| s.accepting()).cloned());
            }

            for c_id in id {
                let c_id = UnionedId::from([*c_id]);
                if let Some(states) = accepting_left.get(&c_id) {
                    left.extend(states.iter().filter(|s| s.accepting()).cloned());
                } else if let Some(states) = accepting_right.get(&c_id) {
                    right.extend(states.iter().filter(|s| s.accepting()).cloned());
                }
            }
            let all_states = painter(id, left, right);
            if all_states.is_empty() {
                continue;
            }
            product.add_states(id, all_states);
        }
        product
    }

    /// Walk the transitions table for a stack of rows to create product states
    /// see eg https://en.wikipedia.org/wiki/Powerset_construction
    ///  
    ///  (1) -a-> (2) -*-> (3) -b-> ((4))
    ///           (2) -*-> (2)
    ///
    ///       | a   | b     | !a!b |
    /// 1     | 2   | Ø     | Ø    |
    /// 2     | 2,3 | 2,3   | 2,3  |
    /// 3     | Ø   | 4     | Ø    |
    /// 4     | Ø   | Ø     | Ø    |
    /// 2,3   | 2,3 | 2,3,4 | 2,3  |
    /// 2,3,4 | 2,3 | 2,3   | 2,3  |
    #[tracing::instrument(skip(self))]
    fn powerset_construction(&mut self, mut stack: Vec<UnionedId>) {
        // println!("🌮🌮🌮🌮🌮\nstack: {stack:?}\nt: {:?}", self.transitions);
        let mut visited: HashSet<UnionedId> = Default::default();
        while let Some(compound_state) = stack.pop() {
            if visited.contains(&compound_state) {
                continue;
            }
            visited.insert(compound_state.clone());

            for (element, transitions) in self.transitions.clone() {
                let mut unioned_transitions: UnionedId = Default::default();
                for c in compound_state.clone() {
                    if let Some(toos) = transitions.get(&BTreeSet::from([c])) {
                        unioned_transitions.extend(toos.iter().flatten());
                    }
                }

                if unioned_transitions.is_empty() {
                    continue;
                }
                let mut states = Default::default();
                for id in &unioned_transitions {
                    let s = self
                        .states
                        .get(&UnionedId::from([*id]))
                        .unwrap_or_else(|| panic!("missing state: {:?}\n{:?}", id, self.states));

                    states = &states | s;
                }
                self.add_transition2_with_states(
                    &compound_state,
                    &element,
                    &unioned_transitions.clone(),
                    &states,
                );
                stack.push(unioned_transitions);
            }
        }
    }

    /// Returns a map of node ids to their associated accepting states
    #[tracing::instrument(skip(self))]
    pub fn accepting_states(&self) -> BTreeMap<UnionedId, BTreeSet<State<M>>> {
        self.states
            .clone()
            .into_iter()
            .filter(|(_id, states)| states.iter().any(|s| s.accepting()))
            .collect::<BTreeMap<_, _>>()
    }

    /// Removes any node or edge which is not reachable from self.entry or is not
    /// in the path from self.entry to an accepting state
    #[tracing::instrument(skip(self))]
    pub(crate) fn shake(&mut self) {
        let alive = self.get_edges().accepting_branches(self);
        // println!("shake: alive: {alive:?}");

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

        let flat_ids = self.ids().into_iter().flatten().collect::<BTreeSet<NodeId>>();

        // removing states may invalidate future operations on this dfa:
        //  states are tracked in the single but used in the plural
        self.states.retain(|k, _v| alive.contains(k) || (k & &flat_ids).len() > 0);
    }

    /// Removes and simplifies unnecessary nodes and edges
    #[tracing::instrument(skip(self))]
    pub(crate) fn simplify(&mut self) {
        // TODO: simplify should re-map all ids to be non-compound (len 1)
        // TODO: simplify can remove any empty TokenSet edge
        self.shake();

        let by_edge = self.get_edges().0;
        self.transitions = Default::default();

        for (source, edges) in &by_edge {
            let mut targets_to_edges: BTreeMap<UnionedId, Vec<&Element>> = Default::default();

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

    #[tracing::instrument(skip(self))]
    pub(super) fn ids(&self) -> HashSet<UnionedId> {
        let mut ids: HashSet<UnionedId> = Default::default();

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

    #[tracing::instrument(skip(self))]
    pub fn add_state(&mut self, node: &UnionedId, state: State<M>) {
        self.states
            .entry(node.to_owned())
            .and_modify(|t| {
                t.insert(state.clone());
            })
            .or_insert_with(|| BTreeSet::from([state.clone()]));
    }

    #[tracing::instrument(skip(self))]
    pub fn add_states(&mut self, node: &UnionedId, states: BTreeSet<State<M>>) {
        self.states
            .entry(node.to_owned())
            .and_modify(|t| {
                t.extend(states.clone());
            })
            .or_insert_with(|| states);
    }

    #[tracing::instrument(skip(self))]
    pub(super) fn add_transition2(&mut self, from: &UnionedId, e: &Element, to: &UnionedId) {
        // let mut no_e = true;
        // println!("before: {:?}", self.transitions);
        for element in &self.elements {
            if !e.accepts(element) {
                continue;
            }
            // no_e = false;

            self.transitions
                .entry(element.clone())
                .or_default()
                .entry(from.clone())
                .or_default()
                .insert(to.clone());
        }
        // if no_e {
        //     println!("did not add a transition for {from:?} -{e:?}-> {to:?}\n\n{self:?}")
        // }
    }

    pub fn add_transition2_with_states(
        &mut self,
        from: &UnionedId,
        e: &Element,
        to: &UnionedId,
        to_state: &BTreeSet<State<M>>,
    ) {
        #[allow(deprecated)]
        self.add_transition2(from, e, to);
        self.add_states(to, to_state.clone());
    }

    #[tracing::instrument(skip(self))]
    pub fn add_transition_with_state(
        &mut self,
        from: &NodeId,
        e: &Element,
        to: &NodeId,
        to_state: &State<M>,
    ) {
        let to = &BTreeSet::from([*to]);
        #[allow(deprecated)]
        self.add_transition2(&BTreeSet::from([*from]), e, to);
        self.add_states(to, BTreeSet::from([to_state.clone()]));
    }

    /// Return true if there are no overlapping edges out of a given node
    ///  and a state for every id
    #[tracing::instrument(skip(self))]
    pub fn is_consistent(&self) -> bool {
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

        for id in self.ids() {
            if self.states.get(&id).is_none() {
                print!("missing state for id: {id:?} {self:#?}");
                return false;
            }
        }
        true
    }

    /// Applies an offset to every ID in self, so self remains consistent
    /// while being shifted into an ID range hopefully distinct from another
    #[tracing::instrument(skip(self))]
    pub(crate) fn offset_self(&mut self, offset: u32) {
        let mut transitions: BTreeMap<Element, BTreeMap<UnionedId, BTreeSet<UnionedId>>> =
            Default::default();

        for (element, v) in &self.transitions {
            transitions.insert(element.clone(), Default::default());
            for (from, to) in v {
                let from: UnionedId = from.iter().map(|id| *id + offset).collect();
                let to: BTreeSet<UnionedId> =
                    to.iter().map(|vec| vec.iter().map(|id| *id + offset).collect()).collect();

                transitions.get_mut(&element.clone()).unwrap().insert(from.clone(), to.clone());
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

    #[tracing::instrument(skip(self))]
    pub(super) fn get_edges(&self) -> EdgeIndex {
        // Build a convenient map of edges indexed differently
        let mut map: BTreeMap<UnionedId, BTreeMap<Element, BTreeSet<UnionedId>>> =
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
}

pub(super) struct EdgeIndex(pub(crate) BTreeMap<UnionedId, BTreeMap<Element, BTreeSet<UnionedId>>>);

impl EdgeIndex {
    /// Return all CompoundIds which are in an accepting path
    #[tracing::instrument(skip_all, ret)]
    pub fn accepting_branches<M>(&self, dfa: &Dfa<M>) -> HashSet<UnionedId>
    where
        M: std::fmt::Debug + PartialOrd + Ord + Clone,
    {
        let mut visited: HashSet<UnionedId> = Default::default();
        let mut alive: HashSet<UnionedId> = Default::default();
        self._accepting_branch(&dfa.entry, &mut visited, &mut alive, dfa);
        alive
    }

    /// Find all accepting branches under a subtree
    #[tracing::instrument(skip_all, ret)]
    fn _accepting_branch<M>(
        &self,
        id: &UnionedId,
        visited: &mut HashSet<UnionedId>,
        alive: &mut HashSet<UnionedId>,
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
