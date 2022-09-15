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
        match self {
            State::InverseInclude(m) => State::Include(m.clone()),
            State::InverseExclude(m) => State::Exclude(m.clone()),
            State::Include(m) => State::InverseInclude(m.clone()),
            State::Exclude(m) => State::Exclude(m.clone()),
        }
    }

    pub fn negate(&self) -> Self {
        self.complement()
    }

    /// Returns true if self is an 'accepting' state in the DFA
    pub fn accepting(&self) -> bool {
        match self {
            State::InverseInclude(_) => false,
            State::InverseExclude(_) => false,
            State::Include(_) => true,
            State::Exclude(_) => true,
        }
    }
}

impl<M> std::fmt::Debug for State<M>
where
    M: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            State::InverseInclude(_) => f.write_str(&format!("_")),
            State::InverseExclude(_) => f.write_str(&format!("_e")),
            Self::Include(_) => f.write_str(&format!("A")),
            Self::Exclude(_) => f.write_str(&format!("Ae")),
        }
    }
}
