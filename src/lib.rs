//! `macro_rules`-style syntax matching for procedural macros.
//! 
//! This crate is work-in-progress, incomplete, and probably buggy!
//! 
//! Example:
//! 
//! ```rust
//! use proc_macro_rules::rules;
//! use proc_macro2::TokenStream;
//!
//! fn main() {
//!     # use quote::quote;
//!     # test_rules(quote! { A (B C) #[inner...] ... });
//!     # test_rules(quote! { foo 1 + 1 });
//!     # test_rules(quote! { foo });
//! # }
//! #
//! # fn test_rules(tokens: TokenStream) {
//!     # const IGNORE: &str = stringify! {
//!     let tokens: TokenStream = /* ... */;
//!     # };
//!
//!     rules!(tokens => {
//!         ($finish:ident ($($found:ident)*) # [ $($inner:tt)* ] $($rest:tt)*) => {
//!             for f in found {
//!                 do_something(&finish, f, &inner, &rest[0]);
//!             }
//!         }
//!         (foo $($bar:expr)?) => {
//!             match bar {
//!                 Some(e) => foo_with_expr(e),
//!                 None => foo_no_expr(),
//!             }
//!         }
//!     });
//! }
//! #
//! # use syn::{Expr, Ident};
//! # use proc_macro2::TokenTree;
//! #
//! # fn do_something(
//! #     finish: &Ident,
//! #     f: Ident,
//! #     inner: &[TokenTree],
//! #     rest: &TokenTree,
//! # ) {}
//! # fn foo_with_expr(e: Expr) {}
//! # fn foo_no_expr() {}
//! ```
//! 
//! Import the `rules` macro with `use proc_macro_rules::rules`, then use with 
//! `rules!(tokens => { branches });` where `tokens` is an expression which
//! evaluates to a `TokenStream` (such as the argument in the definition of a
//! procedural macro).
//! 
//! Each branch in `branches` should have the form `( pattern ) => { body }` where
//! `pattern` is a macro-rules-style pattern (using all the same syntax for
//! meta-variables, AST nodes, repetition, etc.) and `body` is rust code executed
//! when the pattern is matched. Within `body`, any meta-variables in the pattern
//! are bound to variables of an appropriate type from either the
//! [proc_macro2](https://github.com/alexcrichton/proc-macro2) or
//! [syn](https://github.com/dtolnay/syn) crates. Where a meta-variable is
//! inside a repetition or option clause, it will be wrapped in a `Vec` or
//! `Option`, respectively.
//! 
//! For example, in the first branch in the above example `ident` has type
//! `syn::Ident` and `inner` has type `Vec<proc_macro2::TokenTree>`.


extern crate proc_macro2;
extern crate proc_macro_hack;
extern crate proc_macro_rules_macros;
#[doc(hidden)]
pub extern crate syn;

pub use crate::match_set::{Fork, MatchSet};
#[proc_macro_hack::proc_macro_hack]
pub use proc_macro_rules_macros::rules;

mod match_set;

// Regression tests
#[cfg(test)]
mod tests {
    use crate as proc_macro_rules;

    #[test]
    fn test_smoke() {
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

    #[test]
    fn test_empty() {
        let tokens: proc_macro2::TokenStream = "".parse().unwrap();
        rules!(tokens => {
            () => {
                return;
            }
        });
        panic!();
    }

    #[test]
    #[should_panic]
    fn test_no_match() {
        let tokens: proc_macro2::TokenStream = "foo".parse().unwrap();
        rules!(tokens => {
            (bar) => {}
        });
    }

    #[test]
    fn test_branches() {
        fn apply(tokens: proc_macro2::TokenStream, expected_branch: usize) {
            rules!(tokens => {
                (foo) => {
                    if expected_branch == 0 {
                        return;
                    } else {
                        panic!("branch: 0, expected: {}", expected_branch);
                    }
                }
                ($x:ident) => {
                    if expected_branch == 1 {
                        assert_eq!(x.to_string(), "bar");
                        return;
                    } else {
                        // TODO failing here!
                        panic!("branch: 1, expected: {}", expected_branch);
                    }
                }
                ($($x:ident)*) => {
                    if expected_branch == 2 {
                        assert_eq!(x.len(), 3);
                        assert_eq!(x[0].to_string(), "a");
                        assert_eq!(x[1].to_string(), "b");
                        assert_eq!(x[2].to_string(), "c");
                        return;
                    } else {
                        panic!("branch: 2, expected: {}", expected_branch);
                    }
                }
            });
            panic!("Hit no branches, expected: {}", expected_branch);
        }

        apply("foo".parse().unwrap(), 0);
        apply("bar".parse().unwrap(), 1);
        apply("a b c".parse().unwrap(), 2);
    }

    #[test]
    fn test_opt() {
        fn apply(tokens: proc_macro2::TokenStream) {
            rules!(tokens => {
                (foo $(bar),? baz) => {
                    return;
                }
            });
            panic!();
        }

        apply("foo baz".parse().unwrap());
        apply("foo bar baz".parse().unwrap());
        apply("foo bar, baz".parse().unwrap());
    }

    #[test]
    fn test_plus() {
        fn apply(tokens: proc_macro2::TokenStream) {
            rules!(tokens => {
                (foo $(bar),+ baz) => {
                    return;
                }
            });
            panic!();
        }

        apply("foo bar baz".parse().unwrap());
        apply("foo bar, baz".parse().unwrap());
        apply("foo bar, bar baz".parse().unwrap());
        apply("foo bar, bar, baz".parse().unwrap());
        apply("foo bar, bar, bar baz".parse().unwrap());
        apply("foo bar, bar, bar, baz".parse().unwrap());
    }

    #[test]
    fn test_star() {
        fn apply(tokens: proc_macro2::TokenStream) {
            rules!(tokens => {
                (foo $(bar),* baz) => {
                    return;
                }
            });
            panic!();
        }

        apply("foo baz".parse().unwrap());
        apply("foo bar baz".parse().unwrap());
        apply("foo bar, baz".parse().unwrap());
        apply("foo bar, bar baz".parse().unwrap());
        apply("foo bar, bar, baz".parse().unwrap());
        apply("foo bar, bar, bar baz".parse().unwrap());
        apply("foo bar, bar, bar, baz".parse().unwrap());
    }
}
