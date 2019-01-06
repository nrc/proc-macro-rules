use crate::ast::*;
use crate::{collect_vars, verify_rule};

use proc_macro2::{Delimiter, Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{Ident, Token};

impl ToTokens for Rules {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let clause = &self.clause;
        let rules = &self.rules;
        tokens.append_all(quote!({
            let tts = &#clause;
            #(
                if let Some(value) = #rules {
                    value
                } else
            )* {
                panic!("No rule matched input");
            }
        }));
    }
}

impl ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        verify_rule(&self.lhs);

        let body = &self.rhs;

        let rule = &self.lhs;
        let mut variables = vec![];
        collect_vars(&rule, &mut variables);
        let rule = rule.clone().to_builder(None);
        let vars = var_names(&variables);
        let builder_name = &rule.name;
        let matches = matches(&builder_name, &variables);

        tokens.append_all(quote!({
            #matches

            impl proc_macro_rules::syn::parse::Parse for Matches {
                fn parse(ps: proc_macro_rules::syn::parse::ParseStream) -> proc_macro_rules::syn::parse::Result<Matches> {
                    let mut ms: proc_macro_rules::MatchSet<#builder_name> = proc_macro_rules::MatchSet::new(ps.fork());
                    // parse the whole initial ParseStream to avoid 'unexpected token' errors
                    let _: Result<proc_macro2::TokenStream, _> = ps.parse();

                    #rule

                    let result = ms.finalise()?;
                    // FIXME(#8) pick best match
                    result.into_iter().filter_map(|p| if p.input.is_empty() {
                        Some(p.matches.finalise())
                    } else {
                        None
                    }).next().ok_or_else(|| proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "pattern could not be parsed"))
                }
            }

            match proc_macro_rules::syn::parse2(tts.clone()) {
                Ok(Matches { #(#vars,)* }) => {
                    let value = { #body };

                    // This is needed because the body may have ended in a return
                    // statement which makes the `Some` construction unreachable.
                    #[allow(unreachable_code)]
                    {
                        Some(value)
                    }
                }

                // It can be useful to debug here.
                Err(e) => None,
            }
        }));
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
fn sub_matches(
    builder_name: &Ident,
    outer_builder_name: &Ident,
    variables: &[MetaVar],
    repeat: HoistRepeat,
) -> TokenStream2 {
    let builder = builder(builder_name, variables);
    let hoists: Vec<_> = match repeat {
        HoistRepeat::Repeat => variables
            .iter()
            .map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                    MetaVarType::T(_) => {
                        quote! { self.#name.as_ref().expect("hoist failed (a)").clone() }
                    }
                };
                quote! {
                    outer.#name.push(#unpack);
                }
            })
            .collect(),
        HoistRepeat::Option => variables
            .iter()
            .map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) => quote! {
                        if self.#name.is_empty() {
                            None
                        } else {
                            Some(self.#name.clone())
                        }
                    },
                    MetaVarType::T(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                };
                quote! {
                    outer.#name = #unpack;
                }
            })
            .collect(),
        HoistRepeat::None => variables
            .iter()
            .map(|v| {
                let name = &v.name;
                let unpack = match &v.ty {
                    MetaVarType::Vec(_) | MetaVarType::Option(_) => quote! { self.#name.clone() },
                    MetaVarType::T(_) => {
                        quote! { Some(self.#name.as_ref().expect("hoist failed (c)").clone()) }
                    }
                };
                quote! {
                    outer.#name = #unpack;
                }
            })
            .collect(),
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
    let values = variables.iter().map(|v| match v.ty {
        MetaVarType::Vec(_) => quote! { Vec::new() },
        _ => quote! { None },
    });
    quote! {
        fn new() -> #builder_name {
            #builder_name { #(#names: #values,)* }
        }

        // FIXME(#9) this is inefficient and requires the matched types to
        // be Clone. We could do better by using an immutable data structure.
        fn fork(&self) -> #builder_name {
            self.clone()
        }
    }
}

impl ToTokens for Rhs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Rhs::Expr(e) => e.to_tokens(tokens),
            Rhs::Block(b) => b.to_tokens(tokens),
        }
    }
}

// This `ToTokens` impl produces a matcher for the sub-rule.
impl ToTokens for RuleBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        for m in &self.matchers {
            m.to_tokens(tokens)
        }
    }
}

impl ToTokens for FragmentBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            FragmentBuilder::Var(id, _) => tokens.append_all(quote! {
                ms.expect(|ps, matches| {
                    matches.#id = Some(ps.parse()?);
                    Ok(())
                })?;
            }),
            FragmentBuilder::Repeat(rule, RepeatKind::ZeroOrMore, sep) => {
                let sub_builder_name = &rule.name;
                let match_builder = sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::Repeat,
                );

                let sep = match sep {
                    Some(sep) => quote! {
                        ms.expect(|ps, _| {
                            if ps.peek(proc_macro_rules::syn::Token!(#sep)) {
                                let _: proc_macro2::TokenTree = ps.parse().unwrap();
                            } else {
                                terminate = true;
                            }
                            Ok(())
                        })?;
                    },
                    None => TokenStream2::new(),
                };

                tokens.append_all(quote! {
                    let mut terminate = false;
                    while !terminate && ms.fork(|ps, match_handler| {
                        #match_builder

                        let mut ms: proc_macro_rules::MatchSet<#sub_builder_name> =
                            proc_macro_rules::MatchSet::new(ps);

                        #rule
                        #sep

                        let mb = ms.finalise()?;
                        match_handler.hoist(&mb);

                        Ok(())
                    }) {}
                    ms.reset_states();
                });
            }
            FragmentBuilder::Repeat(rule, RepeatKind::OneOrMore, sep) => {
                let sub_builder_name = &rule.name;
                let match_builder = sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::Repeat,
                );

                let sep = match sep {
                    Some(sep) => quote! {
                        ms.expect(|ps, _| {
                            if ps.peek(proc_macro_rules::syn::Token!(#sep)) {
                                let _: proc_macro2::TokenTree = ps.parse().unwrap();
                            } else {
                                terminate = true;
                            }
                            Ok(())
                        })?;
                    },
                    None => TokenStream2::new(),
                };

                tokens.append_all(quote! {
                    let mut count = 0;
                    let mut terminate = false;
                    while !terminate && ms.fork(|ps, match_handler| {
                        #match_builder

                        let mut ms: proc_macro_rules::MatchSet<#sub_builder_name> =
                            proc_macro_rules::MatchSet::new(ps);

                        #rule
                        #sep

                        let mb = ms.finalise()?;
                        match_handler.hoist(&mb);

                        Ok(())
                    }) {
                        count += 1;
                    }
                    if count == 0 {
                        return Err(proc_macro_rules::syn::Error::new(
                            proc_macro2::Span::call_site(),
                            "At least one iteration required",
                        ));
                    }
                    ms.reset_states();
                });
            }
            FragmentBuilder::Repeat(rule, RepeatKind::ZeroOrOne, sep) => {
                let sub_builder_name = &rule.name;
                let match_builder = sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::Option,
                );
                let sep = match sep {
                    Some(sep) => quote! {
                        ms.expect(|ps, _| {
                            if ps.peek(proc_macro_rules::syn::Token!(#sep)) {
                                let _: proc_macro2::TokenTree = ps.parse().unwrap();
                            }
                            Ok(())
                        })?;
                    },
                    None => TokenStream2::new(),
                };

                tokens.append_all(quote! {
                    ms.fork(|ps, match_handler| {
                        #match_builder

                        let mut ms: proc_macro_rules::MatchSet<#sub_builder_name> =
                            proc_macro_rules::MatchSet::new(ps);

                        #rule
                        #sep

                        let mb = ms.finalise()?;
                        match_handler.hoist(&mb);

                        Ok(())
                    });
                    ms.reset_states();
                });
            }
            FragmentBuilder::Ident(i) => {
                let i_str = &i.to_string();
                tokens.append_all(quote! {
                    ms.expect(|ps, _| {
                        let i: proc_macro2::Ident = ps.parse()?;
                        if i.to_string() == #i_str {
                            Ok(())
                        } else {
                            Err(proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "bad ident"))
                        }
                    })?;
                });
            }
            FragmentBuilder::Punct(p) => {
                let p_str = &p.to_string();
                tokens.append_all(quote! {
                    ms.expect(|ps, _| {
                        let p: proc_macro2::Punct = ps.parse()?;
                        if p.to_string() == #p_str {
                            Ok(())
                        } else {
                            Err(proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "bad punct"))
                        }
                    })?;
                });
            }
            FragmentBuilder::Literal(l) => {
                let l_str = &l.to_string();
                tokens.append_all(quote! {
                    ms.expect(|ps, _| {
                        let l: proc_macro2::Literal = ps.parse()?;
                        if l.to_string() == #l_str {
                            Ok(())
                        } else {
                            Err(proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "bad literal"))
                        }
                    })?;
                });
            }
            FragmentBuilder::Group(rule, delimiter) => {
                let d_toks = match delimiter {
                    Delimiter::Parenthesis => quote!(proc_macro2::Delimiter::Parenthesis),
                    Delimiter::Brace => quote!(proc_macro2::Delimiter::Brace),
                    Delimiter::Bracket => quote!(proc_macro2::Delimiter::Bracket),
                    Delimiter::None => quote!(proc_macro2::Delimiter::None),
                };

                let sub_builder_name = &rule.name;
                let match_builder = sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::None,
                );

                tokens.append_all(quote! {
                    ms.expect(|ps, matches| {
                        let tok: proc_macro2::TokenTree = ps.parse()?;
                        match tok {
                            proc_macro2::TokenTree::Group(g) => {
                                if g.delimiter() != #d_toks {
                                    return Err(
                                        proc_macro_rules::syn::Error::new(
                                            proc_macro2::Span::call_site(),
                                            "bad delimiter",
                                        ));
                                }
                                {
                                    #match_builder

                                    struct MatchParser(#sub_builder_name);

                                    impl proc_macro_rules::syn::parse::Parse for MatchParser {
                                        fn parse(ps: proc_macro_rules::syn::parse::ParseStream) -> proc_macro_rules::syn::parse::Result<MatchParser> {
                                            let mut ms: proc_macro_rules::MatchSet<#sub_builder_name> =
                                                proc_macro_rules::MatchSet::new(ps.fork());
                                            // parse the whole initial ParseStream to avoid 'unexpected token' errors
                                            let _: Result<proc_macro2::TokenStream, _> = ps.parse();

                                            #rule

                                            let result = ms.finalise()?;
                                            result.into_iter().filter_map(|p| if p.input.is_empty() {
                                                Some(MatchParser(p.matches))
                                            } else {
                                                None
                                            }).next().ok_or_else(|| proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "group could not be parsed"))
                                        }
                                    }

                                    let mp: MatchParser = proc_macro_rules::syn::parse2(g.stream())?;
                                    proc_macro_rules::Fork::hoist(&mp.0, matches);
                                }
                                Ok(())
                            }
                            _ => Err(proc_macro_rules::syn::Error::new(proc_macro2::Span::call_site(), "expected group")),
                        }
                    })?;
                });
            }
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Type::Item => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Item", Span::call_site()));
            }
            Type::Block => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Block", Span::call_site()));
            }
            Type::Stmt => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Stmt", Span::call_site()));
            }
            Type::Expr => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Expr", Span::call_site()));
            }
            Type::Pat => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Pat", Span::call_site()));
            }
            Type::Lifetime => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Lifetime", Span::call_site()));
            }
            Type::Path => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Path", Span::call_site()));
            }
            Type::Ty => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Type", Span::call_site()));
            }
            Type::Ident => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Ident", Span::call_site()));
            }
            Type::Meta => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Meta", Span::call_site()));
            }
            Type::Tt => {
                tokens.append(Ident::new("proc_macro2", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("TokenTree", Span::call_site()));
            }
            Type::Vis => {
                tokens.append(Ident::new("syn", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Visibility", Span::call_site()));
            }
            Type::Literal => {
                tokens.append(Ident::new("proc_macro2", Span::call_site()));
                Token!(::)(Span::call_site()).to_tokens(tokens);
                tokens.append(Ident::new("Literal", Span::call_site()));
            }
        }
    }
}

impl ToTokens for MetaVar {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.name.to_tokens(tokens);
        Token!(:)(Span::call_site()).to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
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
