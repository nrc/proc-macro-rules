#![recursion_limit = "256"]
#![feature(crate_visibility_modifier)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{Ident, Token};

#[proc_macro]
pub fn rules(input: TokenStream) -> TokenStream {
    let rules: parse::Rules = syn::parse(input).expect("Parsing error");
    let result = expand_rules(rules).into();
    // println!("{}", result);
    result
}

mod parse;

// TODO
// tests!
// docs!
// other AST nodes
// repeat separators

fn expand_rules(rules: parse::Rules) -> TokenStream2 {
    let clause = rules.clause;
    let rules: Vec<_> = rules.rules.into_iter().map(|r| expand_rule(r)).collect();
    quote! {
        {
            fn match_rules(tts: proc_macro2::TokenStream) {
                #(#rules)*
                panic!("No rule matched input");
            }
            match_rules(#clause)
        }
    }
}

fn expand_rule(rule: parse::Rule) -> TokenStream2 {
    verify_rule(&rule.lhs);

    let body = rule.rhs;

    let rule = rule.lhs;
    let mut variables = vec![];
    collect_vars(&rule, &mut variables);
    let rule = rule.to_builder(None);
    let vars = var_names(&variables);
    let builder_name = &rule.name;
    let matches = matches(&builder_name, &variables);

    quote! {
        {
            #matches

            impl syn::parse::Parse for Matches {
                fn parse(ps: syn::parse::ParseStream) -> syn::parse::Result<Matches> {
                    let mut ms: MatchSet<#builder_name> = proc_macro_rules::MatchSet::new(ps.fork());
                    // parse the whole initial ParseStream to avoid 'unexpected token' errors
                    let _: Result<proc_macro2::TokenStream, _> = ps.parse();

                    #rule

                    let result = ms.finalise()?;
                    if result.len() > 1 {
                        // TODO pick best match
                    }
                    Ok(result.into_iter().next().unwrap().matches.finalise())
                }
            }

            match syn::parse2(tts.clone()) {
                Ok(Matches { #(#vars,)* }) => {
                    #body;
                    return;
                }
                // It can be useful to debug here.
                Err(e) => {}
            }
        }
    }
}

fn verify_rule(_rule: &parse::SubRule) {
    // FIXME pattern rule verification
}

// FIXME we could save some computation by storing intermediate results on the SubRules.
fn collect_vars(rule: &parse::SubRule, vars: &mut Vec<MetaVar>) {
    for m in &rule.matchers {
        match m {
            parse::Fragment::Var(id, ty) => vars.push(MetaVar {
                name: id.clone(),
                ty: MetaVarType::T(*ty),
            }),
            parse::Fragment::Repeat(sub_rule, rkind) => {
                let mut sub = vec![];
                collect_vars(sub_rule, &mut sub);
                for s in sub {
                    vars.push(match rkind {
                        parse::RepeatKind::ZeroOrMore | parse::RepeatKind::OneOrMore => MetaVar {
                            name: s.name,
                            ty: MetaVarType::Vec(Box::new(s.ty)),
                        },
                        parse::RepeatKind::ZeroOrOne => MetaVar {
                            name: s.name,
                            ty: MetaVarType::Option(Box::new(s.ty)),
                        },
                    })
                }
            }
            parse::Fragment::Group(sub_rule, _) => {
                collect_vars(sub_rule, vars);
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
struct MetaVar {
    name: Ident,
    ty: MetaVarType,
}

impl ToTokens for MetaVar {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.name.to_tokens(tokens);
        Token!(:)(Span::call_site()).to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
enum MetaVarType {
    Vec(Box<MetaVarType>),
    Option(Box<MetaVarType>),
    T(parse::Type),
}

impl ToTokens for MetaVarType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            MetaVarType::Vec(ty) => {
                tokens.append(Ident::new("Vec", Span::call_site()));
                Token!(<)(Span::call_site()).to_tokens(tokens);
                ty.to_tokens(tokens);
                Token!(>)(Span::call_site()).to_tokens(tokens);
            }
            MetaVarType::Option(ty) => {
                tokens.append(Ident::new("Option", Span::call_site()));
                Token!(<)(Span::call_site()).to_tokens(tokens);
                ty.to_tokens(tokens);
                Token!(>)(Span::call_site()).to_tokens(tokens);
            }
            MetaVarType::T(ty) => ty.to_tokens(tokens),
        }
    }
}

fn var_names(variables: &[MetaVar]) -> Vec<Ident> {
    variables.iter().map(|v| v.name.clone()).collect()
}

fn matches(builder_name: &Ident, variables: &[MetaVar]) -> TokenStream2 {
    let decls = variables;
    let builder = builder(builder_name, variables);
    let unwraps = variables.iter().map(|v| {
        let name = &v.name;
        match v.ty {
            MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { #name: self.#name },
            MetaVarType::T(_) => quote! { #name: self.#name.unwrap() },
        }
    });        
    let fork_impl = matches_fork(builder_name, variables);
    quote! {
        #builder

        struct Matches {
            #(#decls,)*
        }

        impl #builder_name {
            fn finalise(self) -> Matches {
                Matches {
                    #(#unwraps,)*
                }
            }
        }

        impl proc_macro_rules::Fork for #builder_name {
            type Parent = ();
            fn hoist(&self, outer: &mut ()) {}
            #fork_impl
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum HoistRepeat {
    None,
    Repeat,
    Option,
}

// repeat will have to do opt toof
fn sub_matches(builder_name: &Ident, outer_builder_name: &Ident, variables: &[MetaVar], repeat: HoistRepeat) -> TokenStream2 {
    let builder = builder(builder_name, variables);
    // TODO types should be based on outer types, not inners
    let hoists: Vec<_> = match repeat {
        HoistRepeat::Repeat => {
            variables.iter().map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                    MetaVarType::T(_) => quote! { self.#name.as_ref().expect("hoist failed (a)").clone() },
                };
                quote! {
                    outer.#name.push(#unpack);
                }
            }).collect()
        }
        HoistRepeat::Option => {
            variables.iter().map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                    MetaVarType::T(_) => quote! { self.#name.as_ref().expect("hoist failed (b)").clone() },
                };
                quote! {
                    outer.#name = #unpack;
                }
            }).collect()
        }
        HoistRepeat::None => {
            variables.iter().map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                    MetaVarType::T(_) => quote! { Some(self.#name.as_ref().expect("hoist failed (c)").clone()) },
                };
                quote! {
                    outer.#name = #unpack;
                }
            }).collect()
        }
    };
    let fork_impl = matches_fork(builder_name, variables);
    quote! {
        #builder

        impl proc_macro_rules::Fork for #builder_name {
            type Parent = #outer_builder_name;
            fn hoist(&self, outer: &mut #outer_builder_name) {
                #(#hoists)*
            }

            #fork_impl
        }

    }
}

fn builder(builder_name: &Ident, variables: &[MetaVar]) -> TokenStream2 {
    let opt_decls = variables.iter().map(|v| {
        let name = &v.name;
        let ty = &v.ty;
        match &v.ty {
            MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { #name: #ty },
            MetaVarType::T(_) => quote! { #name: Option<#ty> },
        }
    });
    quote! {
        #[derive(Clone)]
        struct #builder_name {
            #(#opt_decls,)*
        }
    }
}

fn matches_fork(builder_name: &Ident, variables: &[MetaVar]) -> TokenStream2 {
    let names = &var_names(variables);
    let values = variables.iter().map(|v| {
        match v.ty {
            MetaVarType::Vec(_) => quote! { Vec::new() },
            _ => quote! { None },
        }
    });
    quote! {
        fn new() -> #builder_name {
            #builder_name { #(#names: #values,)* }
        }

        // TODO this is inefficient and requires the matched types to
        // be Clone. We could do better by using an immutable data structure.
        fn fork(&self) -> #builder_name {
            self.clone()
        }
    }
}
