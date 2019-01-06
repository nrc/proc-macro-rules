// This is an example of using the proc_macro_rules crate to convert a macro_rules
// macro to a procedural one.

#![allow(unused_macros)]

extern crate proc_macro;

use quote::quote;
use proc_macro::TokenStream;
use proc_macro_rules::rules;

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
