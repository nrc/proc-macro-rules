# proc-macro-rules

`macro_rules`-style syntax matching for procedural macros.

This crate is work-in-progress, incomplete, and probably buggy!

Example:

```rust
rules!(tokens => {
    ($finish:ident ($($found:ident)*) # [ $($inner:tt)* ] $($rest:tt)*) => {
        for f in found {
            do_something(finish, f, inner, rest[0]);
        }
    }
    (foo $($bar:expr)?) => {
        match bar {
            Some(e) => foo_with_expr(e),
            None => foo_no_expr(),
        }
    }
});
```

## Using proc-macro-rules

Add `proc-macro-rules = "0.2.1"` to your Cargo.toml.

Import the `rules` macro with `use proc_macro_rules::rules`, then use with `rules!(tokens => { branches });` where `tokens` is an expression which evaluates to a `TokenStream` (such as the argument in the definition of a procedural macro).

Each branch in `branches` should have the form `( pattern ) => { body }` where `pattern` is a macro-rules-style pattern (using all the same syntax for meta-variables, AST nodes, repetition, etc.) and `body` is rust code executed when the pattern is matched. Within `body`, any meta-variables in the pattern are bound to variables of an appropriate type from either the [proc_macro2](https://github.com/alexcrichton/proc-macro2) or [syn](https://github.com/dtolnay/syn) crates. Where a meta-variable is inside a repetition or option clause, it will be wrapped in a `Vec` or `Option`, respectively.

For example, in the first branch in the above example `ident` has type `syn::Ident` and `inner` has type `Vec<proc_macro2::TokenTree>`.


## Building and testing

Use `cargo build` to build, `cargo test --all` to test.


## Contributing

Contributions are very welcome! It would be great to know things which are missing or incorrect (in general we should have the same behaviour as `macro_rules`, so anything different is incorrect). Issues, code, docs, tests, and corrections are all welcome.
