// This is an example of using the proc_macro_rules crate to convert a macro_rules
// macro to a procedural one. The two macros should work in exactly the same way.

// The main differences are:
//   * we must manually handle the token streams and their type conversion
//   * we use the rules! macro
//   * we use the quote! macro for each body (which requires using `#` instead of `$`)

#![allow(unused_macros)]

extern crate proc_macro;

use quote::quote;
use proc_macro::TokenStream;
use proc_macro_rules::rules;

// Declarative version using macro_rules.
macro_rules! vec {
    () => {
        Vec::new()
    };
    ( $( $x:expr ),+ ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

// Procedural version.
#[proc_macro]
pub fn vec(input: TokenStream) -> TokenStream {
    rules!(input.into() => {
        () => { quote! {
            Vec::new()
        }}
        ( $( $x:expr ),+ ) => { quote! {
            let mut temp_vec = Vec::new();
            #(
                temp_vec.push(#x);
            )*
            temp_vec
        }}
    }).into()
}
