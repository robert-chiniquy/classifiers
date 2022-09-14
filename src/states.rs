
// Define complementation ... 
// a -> b -> c(accept)
// complement:
// a(accept) -> b(accept) -> c
// for homogenous trees with only 1 accepting state or multiple identical accepting states, this is fine
// we need to also define complementation for trees with heterogenous accepting states
// matt suggests the default state is like ... InverseInclude(M), so when complementation occurs,
// you get the right match ... however ... 
// so then, any operation which transforms a tree by changing the accepting Include(M) 
// states to Exclude(M), (as in Classifier::Not) must also change the InverseInclude(M) to InverseExclude(M)
#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum State<M: std::fmt::Debug> {
    InverseInclude(Option<M>),
    InverseExclude(Option<M>),
    Include(Option<M>),
    Exclude(Option<M>),
}


// TODO: validate
impl <M> State<M> 
where 
    M: std::fmt::Debug + Clone
{
    pub fn negate(&self) -> Self {
        match self {
            State::InverseInclude(m) =>State::Include(m.clone()),
            State::InverseExclude(m) => State::Exclude(m.clone()),
            State::Include(m) => State::InverseInclude(m.clone()),
            State::Exclude(m) => State::Exclude(m.clone()),
        }
    }

    pub fn accepting(&self) -> bool {
        match self {
            State::InverseInclude(_) => false,
            State::InverseExclude(_) => false,
            State::Include(_) => true,
            State::Exclude(_) => true,
        }
    }
}

impl<M> std::fmt::Debug for State<M> where M: std::fmt::Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            State::InverseInclude(m) => f.write_str(&format!("In: {m:?}")),
            State::InverseExclude(m) => f.write_str(&format!("Ex: {m:?}")),
            Self::Include(m) => f.write_str(&format!("((In: {m:?}))")),
            Self::Exclude(m) => f.write_str(&format!("((Ex: {m:?}))")),
        }
    }
}