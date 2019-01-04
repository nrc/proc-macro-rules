#![feature(proc_macro_hygiene)]

extern crate proc_macro2;
extern crate proc_macro_rules_macros as macros;
extern crate syn;

pub use crate::match_set::{Fork, MatchSet};
pub use macros::rules;

mod match_set;

#[cfg(test)]
mod tests {
    use super::*;
    use crate as proc_macro_rules;

    #[test]
    fn smoke() {
        let tokens: proc_macro2::TokenStream = "hi (a b c) # [there] the - rest".parse().unwrap();
        rules!(tokens => {
            ($finish:ident ($($found:ident)+) # [ $($inner:tt)? ] $($rest:expr)*) => {
                assert_eq!(finish.to_string(), "hi");
                assert_eq!(found.len(), 3);
                assert!(inner.is_some());
                assert_eq!(rest.len(), 1);
                return;
            }
        });
        panic!();
    }
}
