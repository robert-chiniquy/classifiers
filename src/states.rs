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

    /// Flips states between themselves and their opposites
    pub fn negate(&self) -> Self {
        match self {
            State::InverseInclude(m) => State::InverseExclude(m.clone()),
            State::InverseExclude(m) => State::InverseInclude(m.clone()),
            State::Include(m) => State::Exclude(m.clone()),
            State::Exclude(m) => State::Include(m.clone()),
        }
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
            State::InverseInclude(m) => f.write_str(&format!("v-in: {m:?}")),
            State::InverseExclude(m) => f.write_str(&format!("v-ex: {m:?}")),
            Self::Include(m) => f.write_str(&format!("In: {m:?}")),
            Self::Exclude(m) => f.write_str(&format!("Ex: {m:?}")),
        }
    }
}
