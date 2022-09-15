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

    #[tracing::instrument(skip_all)]
    fn relation(
        &self,
        other: &Self,
    ) -> (Relation, Self, BTreeSet<CompoundId>, BTreeSet<CompoundId>) {
        let mut left_ids: BTreeSet<_> = Default::default();
        let mut right_ids: BTreeSet<_> = Default::default();

        let by_product = Dfa::painting_product(self, other, &mut |id, l, r| {
            if !l.is_empty() {
                left_ids.insert(id.clone());
            }
            if !r.is_empty() {
                right_ids.insert(id.clone());
            }
            &l | &r
        });

        (
            if left_ids == right_ids {
                Relation::Equality
            } else if left_ids.is_disjoint(&right_ids) {
                Relation::Disjoint
            } else if left_ids.is_superset(&right_ids) {
                Relation::Superset
            } else if left_ids.is_subset(&right_ids) {
                Relation::Subset
            } else {
                Relation::Intersection
            },
            by_product,
            left_ids,
            right_ids,
        )
    }

    #[tracing::instrument]
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
        dfa.add_transition_with_state(&1, &Element::NotTokenSet(BTreeSet::new()), &1, &State::Include(m.clone()));
        dfa
    }

    #[tracing::instrument]
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

    /// Return a "completed DFA" with a complement of all states
    /// and universally accepting self-loop edges out of any non-universally-accepting state
    #[tracing::instrument(skip(self))]
    fn complement(&self, m: &Option<Self::Metadata>) -> Self {
        // Start with all IDs and remove any found to have an outbound edge
        let mut dfa = self.clone();
        dfa.simplify();

        let vortex = self.ids().iter().flatten().max().unwrap_or(&0) + 1;
        dfa.add_transition_with_state(&vortex, &Element::universal(), &vortex, &State::InverseInclude(m.clone()));
        
        // Find the negative space of all existing edges from each source node
        for (source, edges) in self.get_edges().0 {
            if edges.is_empty() {
                #[allow(deprecated)]
                dfa.add_transition2(&source, &Element::universal(), &CompoundId::from([vortex]));
                continue;
            }
            let sum: Element = edges.iter().map(|(element, _)| element).cloned().sum();
            let d = Element::difference(&Element::universal(), &sum);

            println!("Universal - {sum:?} = {d:?}");

            match &d {
                Element::TokenSet(s) => {
                    if !s.is_empty() {
                        #[allow(deprecated)]
                        dfa.add_transition2(&source, &d, &CompoundId::from([vortex]));
                    }
                }
                Element::NotTokenSet(_) => {
                    #[allow(deprecated)]
                    dfa.add_transition2(&source, &d, &CompoundId::from([vortex]));
                }
            }
        }

        dfa.states = dfa
            .states
            .into_iter()
            .map(|(id, states)| (id, states.iter().map(|s| s.complement()).collect()))
            .collect();
        dfa
    }

    /// Return a DFA with all states negated, that is, flipped to their opposite Include/Exclude state
    fn negate(&self) -> Self {
        let mut ret = self.clone();
        ret.states.iter_mut().for_each(|(_id, states)| {
            *states = states.iter().map(|state| state.negate()).collect();
        });
        ret
    }

    #[tracing::instrument(skip_all)]
    fn union(&self, other: &Self) -> Self {
        let mut p = Dfa::construct_product(self, &mut other.clone());
        p.simplify();
        p
    }

    #[tracing::instrument(skip_all)]
    fn intersection(&self, other: &Self) -> Self {
        let mut p = Dfa::intersect(self, other);
        p.simplify();
        p.graphviz_file("intersection.dot", "intersection");
        println!("p: {:?}", p);
        p
    }
}
