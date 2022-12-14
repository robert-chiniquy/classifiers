mod classifiers;
mod dfa;
mod element;
mod graphviz;
mod relatable;
mod states;
mod tests;
mod traits;

pub use crate::classifiers::*;
pub use dfa::*;
pub use element::*;
pub use relatable::*;
pub use states::*;
pub use traits::*;

#[cfg(test)]
pub use utilities::*;

use std::collections::BTreeSet;


mod utilities {
    #![allow(dead_code)]
    pub fn test_setup() {
        assert!(*TEST_SETUP);
    }

    static TEST_SETUP: once_cell::sync::Lazy<bool> = once_cell::sync::Lazy::new(|| {
        setup();
        true
    });

    fn setup() {
        #[cfg(feature = "trace")]
        {
            let subscriber = tracing_subscriber::fmt()
                .with_span_events(tracing_subscriber::fmt::format::FmtSpan::CLOSE)
                .finish();

            let _ = tracing::subscriber::set_global_default(subscriber);
        }
    }
}
