use super::*;


impl<M> Dfa<M> 
where
    M: std::fmt::Debug + PartialOrd + Ord + PartialEq + Eq + Clone,
{

    #[tracing::instrument(skip_all)]
    pub(crate) fn compute_relation(
        &self,
        other: &Self,
    ) -> (Relation, Self, BTreeSet<UnionedId>, BTreeSet<UnionedId>) {
        let mut left_ids: BTreeSet<_> = Default::default();
        let mut right_ids: BTreeSet<_> = Default::default();

        let by_product = Dfa::painting_product(self, other, &mut |id, l, r| {
            if l.iter().any(|s|s.accepting()) {
                left_ids.insert(id.clone());
            }
            if r.iter().any(|s|s.accepting()) {
                right_ids.insert(id.clone());
            }
            l | r
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


}

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

    fn relation(&self, other: &Self) -> Relation {
        let (relation, _, _, _) = self.compute_relation(other);
        relation
    }   

    #[tracing::instrument]
    fn universal(m: &Option<Self::Metadata>) -> Self {
        // empty symbols
        let entry = UnionedId::from([1]);
        let mut dfa = Dfa::<Self::Metadata> {
            symbols: BTreeSet::new(),
            elements: BTreeSet::new(),
            entry: entry.clone(),
            transitions: Default::default(),
            states: Default::default(),
        };
        // add a self-loop with a NotTokenSet of nothing
        dfa.add_transition_with_state(&1, &Element::not_tokens(&[]), &1, &State::Include(m.clone()));
        dfa
    }

    #[tracing::instrument]
    fn none(m: &Option<Self::Metadata>) -> Self {
        // empty symbols
        let entry = UnionedId::from([1]);
        let mut dfa = Dfa::<Self::Metadata> {
            symbols: BTreeSet::new(),
            elements: BTreeSet::new(),
            entry: entry.clone(),
            transitions: Default::default(),
            states: Default::default(),
        };
        dfa.add_transition_with_state(&1, &Element::tokens(&[]), &1, &State::InverseInclude(m.clone()));
        dfa
    }

    /// Return a "completed DFA" with a complement of all states
    /// and universally accepting self-loop edges out of any non-universally-accepting state
    #[tracing::instrument(skip(self))]
    fn complement(&self, m: &Option<Self::Metadata>) -> Self {
        // Start with all IDs and remove any found to have an outbound edge
        let mut dfa = self.clone();
        dfa.simplify();

        let vortex_id = self.ids().iter().flatten().max().unwrap_or(&0) + 1;
        dfa.add_transition_with_state(&vortex_id, &Element::universal(), &vortex_id, &State::InverseInclude(m.clone()));
        let vortex = &UnionedId::from([vortex_id]);
        
        // Find the negative space of all existing edges from each source node
        let mut no_edges = self.ids();
        for (source, edges) in self.get_edges().0 {
            no_edges.remove(&source);

            if edges.is_empty() {
                #[allow(deprecated)]
                dfa.add_transition2(&source, &Element::universal(), vortex);
                continue;
            }
            let sum: Element = edges.iter().map(|(element, _)| element).cloned().sum();
            let d = Element::difference(&Element::universal(), &sum);

            // println!("Universal - {sum:?} = {d:?}");

            match &d {
                Element::TokenSet(s) => {
                    if !s.is_empty() {
                        #[allow(deprecated)]
                        dfa.add_transition2(&source, &d, vortex);
                    }
                }
                Element::NotTokenSet(_) => {
                    #[allow(deprecated)]
                    dfa.add_transition2(&source, &d, vortex);
                }
            }
        }

        for id in no_edges {
            dfa.add_transition2(&id, &Element::universal(), vortex);
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
        let mut p = Dfa::product(self, &mut other.clone());
        p.simplify();
        p
    }

    #[tracing::instrument(skip_all)]
    fn intersection(&self, other: &Self) -> Self {
        let mut p = Dfa::intersect(self, other);
        p.simplify();
        p.graphviz_file("intersection.dot", "intersection");
        // println!("p: {:?}", p);
        p
    }
}
