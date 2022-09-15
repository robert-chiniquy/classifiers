use super::*;

#[test]
fn test_relation() {
    let mut inputs = [
        ("*", "", Relation::Disjoint),
        ("*b", "*", Relation::Subset),
        ("*", "*", Relation::Equality),
        ("f*", "*", Relation::Subset),
        ("a*", "a", Relation::Disjoint),
        ("a", "a*", Relation::Disjoint),
        ("**", "*?*", Relation::Superset),
        ("**", "*f", Relation::Superset), // nb
        ("aab", "a?", Relation::Disjoint),
        ("?", "*", Relation::Subset),
        ("??", "*", Relation::Subset),
        ("*", "f*", Relation::Superset),
        ("**", "*f*", Relation::Superset),
        ("a", "*", Relation::Subset),
        ("*", "a", Relation::Superset),
        ("a*", "*a", Relation::Intersection),
        ("a", "a", Relation::Equality),
        ("aa", "a", Relation::Disjoint),
        ("a", "aa", Relation::Disjoint),
        ("a*b*z", "a*c*z", Relation::Intersection),
        ("a", "a*b", Relation::Disjoint),
        ("a*b", "a:b", Relation::Disjoint),
        ("*f", "f*f", Relation::Superset),
        ("f*f", "*f", Relation::Subset),
        ("*f*", "*f*", Relation::Equality),
        ("*f*", "f*f*", Relation::Superset),
        ("f*f*", "*f*", Relation::Subset),
        ("asdf*f**", "*f*", Relation::Subset),
        ("*?*", "***", Relation::Equality),
        ("f*f*", "*&f*", Relation::Intersection), // asdfafa
    ];

    inputs.reverse();

    let mut fails = 0;
    let mut results: Vec<_> = Default::default();
    for (a, b, outcome) in inputs {
        let mut da = Classifier::<Dfa>::Literal(a.to_string(), None).compile(&None);
        let mut db = Classifier::<Dfa>::Literal(b.to_string(), None).compile(&None);

        da.simplify();
        db.simplify();

        // println!("\n\n🐥🐥🐥🐥🐥🐥🐥 {a} {b}\n\n");
        // assert!(da.is_consistent(), "failed on: {a}");
        // assert!(db.is_consistent(), "failed on: {b}");

        let (dr, dp, l, r) = da.relation(&db);
        results.push((da, db, a, b, outcome, dr, dp, l, r));
    }

    for (da, db, a, b, outcome, dr, dp, l, r) in results {
        if dr != outcome {
            println!(
                "🌵🌵🌵🌵🌵🌵 {a} v. {b} {outcome} != {dr}\nl:{l:?}\nr:{r:?}\nda: {da:?}\ndb: {db:?}"
            );
            fails += 1;
            dp.graphviz_file(
                &format!(
                    "product-{}_{}.dot",
                    a.replace('*', "_"),
                    b.replace('*', "_")
                ),
                &format!("{a} v. {b} {outcome} != {dr}"),
            );
            da.graphviz_file(&format!("single-{}.dot", a.replace('*', "_")), a);
            db.graphviz_file(&format!("single-{}.dot", b.replace('*', "_")), b);
        }
    }
    assert_eq!(fails, 0);
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

        let vortex = self.ids().iter().flatten().max().unwrap_or(&0) + 1;
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
            .map(|(id, states)| (id, states.iter().map(|s| s.complement()).collect()))
            .collect();
        dfa
    }

    fn union(&self, other: &Self) -> Self {
        let mut p = Dfa::construct_product(self, &mut other.clone());
        p.simplify();
        p
    }

    fn intersection(&self, other: &Self) -> Self {
        let mut p = Dfa::intersect(self, other);
        p.simplify();
        p.graphviz_file("intersection.dot", "intersection");
        println!("p: {:?}", p);
        p
    }
}
