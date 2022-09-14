use super::*;

impl<M> Relatable for Dfa<M>
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{
    type Language = String;

    type Element = Element;

    type Metadata = M;

    fn from_language(l: &Self::Language, m: &Option<Self::Metadata>) -> Self {
        let dfa = Dfa::from_language(l.chars().collect(), m);
        dfa
    }

    fn relation(&self, other: &Self) -> (Relation, Self) {
        let _product = Dfa::product(self, other);

        todo!()
    }

    fn universal(m: &Option<Self::Metadata>) -> Self {
        // empty symbols
        let entry = CompoundId::from([1]);
        let mut dfa = Dfa::<Self::Metadata> {
            symbols: BTreeSet::new(),
            elements: BTreeSet::new(),
            entry: entry.clone(),
            transitions: Default::default(),
            states: Default::default(),
        };
        // add a self-loop with a NotTokenSet of nothing
        dfa.add_transition(&1, &Element::NotTokenSet(BTreeSet::new()), &1);
        dfa.add_state(&entry, State::Include(m.clone()));
        dfa
    }

    fn none(m: &Option<Self::Metadata>) -> Self {
        // empty symbols
        let entry = CompoundId::from([1]);
        let mut dfa = Dfa::<Self::Metadata> {
            symbols: BTreeSet::new(),
            elements: BTreeSet::new(),
            entry: entry.clone(),
            transitions: Default::default(),
            states: Default::default(),
        };
        dfa.add_state(&entry, State::InverseInclude(m.clone()));
        dfa
    }

    /// Return a "completed DFA": a DFA with a complement of all states
    /// and universally accepting self-loop edges out of any non-universally-accepting state
    fn negate(&self, m: &Option<Self::Metadata>) -> Self {
        // Start with all IDs and remove any found to have an outbound edge
        let mut dfa = self.clone();
        dfa.simplify();

        let vortex = self.ids().iter().map(|id| id).flatten().max().unwrap_or(&0) + 1;
        dfa.add_transition(&vortex, &Element::universal(), &vortex);
        dfa.add_state(
            &CompoundId::from([vortex]),
            State::InverseInclude(m.clone()),
        );

        // Find the negative space of all existing edges from each source node
        for (source, edges) in self.get_edges().0 {
            if edges.is_empty() {
                dfa.add_transitions(&source, &Element::universal(), &CompoundId::from([vortex]));
                continue;
            }
            let sum: Element = edges.iter().map(|(element, _)| element).cloned().sum();
            let d = Element::difference(&Element::universal(), &sum);

            println!("Universal - {sum:?} = {d:?}");

            match &d {
                Element::TokenSet(s) => {
                    if !s.is_empty() {
                        dfa.add_transitions(&source, &d, &CompoundId::from([vortex]));
                    }
                }
                Element::NotTokenSet(_) => {
                    dfa.add_transitions(&source, &d, &CompoundId::from([vortex]));
                }
            }
        }

        dfa.states = dfa
            .states
            .into_iter()
            .map(|(id, states)| (id, states.iter().map(|s| s.negate()).collect()))
            .collect();
        dfa
    }

    fn union(&self, other: &Self) -> Self {
        let mut p = Dfa::construct_product(self, &mut other.clone());
        p.simplify();
        p
    }

    fn intersection(&self, other: &Self) -> Self {
        let mut p = Dfa::intersect(self, &mut other.clone());
        p.simplify();
        p.graphviz_file("intersection.dot", "intersection");
        println!("p: {:?}", p);
        p
    }
}
