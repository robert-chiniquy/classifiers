#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum State<M>
where
    M: std::fmt::Debug,
{
    /// The inverse / complement of Include
    InverseInclude(Option<M>),
    /// The inverse / complement of Exclude
    InverseExclude(Option<M>),
    /// An accepting state which indicates the path is included in the defined set
    Include(Option<M>),
    /// An accepting state which indicates the path is excluded from the defined set
    Exclude(Option<M>),
}

impl<M> State<M>
where
    M: std::fmt::Debug + Clone,
{
    /// Flips states between themselves and their inverses
    pub fn complement(&self) -> Self {
        use State::*;
        match self {
            Include(m) => InverseInclude(m.clone()),
            Exclude(m) => InverseExclude(m.clone()),
            InverseInclude(m) => Include(m.clone()),
            InverseExclude(m) => Exclude(m.clone()),
        }
    }

    pub fn negate(&self) -> Self {
        self.complement()
    }

    /// Returns true if self is an 'accepting' state in the DFA
    pub fn accepting(&self) -> bool {
        use State::*;
        match self {
            Include(_) => true,
            Exclude(_) => true,
            InverseInclude(_) => false,
            InverseExclude(_) => false,
        }
    }
}

impl<M> std::fmt::Debug for State<M>
where
    M: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use State::*;
        match self {
            Include(_) => f.write_str(&format!("A")),
            Exclude(_) => f.write_str(&format!("Ae")),
            InverseInclude(_) => f.write_str(&format!("_")),
            InverseExclude(_) => f.write_str(&format!("_e")),
        }
    }
}
