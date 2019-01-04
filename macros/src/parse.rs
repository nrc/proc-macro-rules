use crate::{collect_vars, HoistRepeat, MetaVar};
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, Literal, Punct, Span, TokenTree as TokenTree2};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Result as ParseResult};
use syn::{
    braced, parenthesized, parse2,
    token::{Brace, Comma, Dollar, FatArrow, Paren},
    Block, Expr, Ident, Token,
};

#[derive(Debug)]
crate struct Rules {
    crate clause: Expr,
    crate rules: Vec<Rule>,
}

impl Parse for Rules {
    fn parse(input: ParseStream) -> ParseResult<Rules> {
        let clause = input.parse()?;
        input.parse::<FatArrow>()?;
        let content;
        braced!(content in input);
        let mut rules = vec![];
        while !content.is_empty() {
            rules.push(content.parse()?);
        }

        Ok(Rules { clause, rules })
    }
}

#[derive(Debug)]
crate struct Rule {
    crate lhs: SubRule,
    crate rhs: Rhs,
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> ParseResult<Rule> {
        let content;
        parenthesized!(content in input);
        let lhs = content.parse()?;

        input.parse::<FatArrow>()?;
        let rhs = input.parse()?;
        Ok(Rule { lhs, rhs })
    }
}

#[derive(Debug)]
crate enum Rhs {
    Expr(Expr),
    Block(Block),
}

impl ToTokens for Rhs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Rhs::Expr(e) => e.to_tokens(tokens),
            Rhs::Block(b) => b.to_tokens(tokens),
        }
    }
}

impl Parse for Rhs {
    fn parse(input: ParseStream) -> ParseResult<Rhs> {
        if input.peek(Brace) {
            Ok(Rhs::Block(input.parse()?))
        } else {
            let e = input.parse()?;
            input.parse::<Comma>()?;
            Ok(Rhs::Expr(e))
        }
    }
}

#[derive(Debug)]
crate struct SubRule {
    crate matchers: Vec<Fragment>,
}

impl Parse for SubRule {
    fn parse(input: ParseStream) -> ParseResult<SubRule> {
        let mut matchers = vec![];
        while !input.is_empty() {
            matchers.push(input.parse()?);
        }
        Ok(SubRule { matchers })
    }
}

impl SubRule {
    crate fn to_builder(self, parent_name: Option<Ident>) -> RuleBuilder {
        let mut variables = vec![];
        collect_vars(&self, &mut variables);
        let name = Ident::new(&next_builder_name(), Span::call_site());
        RuleBuilder {
            matchers: self
                .matchers
                .into_iter()
                .map(|m| m.to_builder(Some(name.clone())))
                .collect(),
            variables,
            name,
            parent_name,
        }
    }
}

crate struct RuleBuilder {
    matchers: Vec<FragmentBuilder>,
    variables: Vec<MetaVar>,
    parent_name: Option<Ident>,
    crate name: Ident,
}

// FIXME(#10) WARNING: VERY THREAD-UNSAFE
fn next_builder_name() -> String {
    static mut NEXT_ID: u32 = 0;
    unsafe {
        NEXT_ID += 1;
        format!("MatchesBuilder{}", NEXT_ID)
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

#[derive(Debug)]
crate enum Fragment {
    Var(Ident, Type),
    // FIXME(#1) separators
    Repeat(SubRule, RepeatKind),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(SubRule, Delimiter),
}

impl Parse for Fragment {
    fn parse(input: ParseStream) -> ParseResult<Fragment> {
        if input.peek(Dollar) {
            let _: Dollar = input.parse()?;
            if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                let rule = content.parse()?;
                let kind = input.parse()?;
                Ok(Fragment::Repeat(rule, kind))
            } else {
                let ident = input.parse()?;
                let _: Token!(:) = input.parse()?;
                let ty = input.parse()?;
                Ok(Fragment::Var(ident, ty))
            }
        } else {
            let tok: TokenTree2 = input.parse()?;
            Ok(match tok {
                TokenTree2::Literal(l) => Fragment::Literal(l),
                TokenTree2::Punct(p) => Fragment::Punct(p),
                TokenTree2::Ident(i) => Fragment::Ident(i),
                TokenTree2::Group(g) => {
                    let rule = parse2(g.stream())?;
                    Fragment::Group(rule, g.delimiter())
                }
            })
        }
    }
}

impl Fragment {
    crate fn to_builder(self, parent_name: Option<Ident>) -> FragmentBuilder {
        match self {
            Fragment::Var(i, t) => FragmentBuilder::Var(i, t),
            Fragment::Repeat(r, rep) => FragmentBuilder::Repeat(r.to_builder(parent_name), rep),
            Fragment::Ident(i) => FragmentBuilder::Ident(i),
            Fragment::Punct(p) => FragmentBuilder::Punct(p),
            Fragment::Literal(l) => FragmentBuilder::Literal(l),
            Fragment::Group(r, d) => FragmentBuilder::Group(r.to_builder(parent_name), d),
        }
    }
}

crate enum FragmentBuilder {
    Var(Ident, Type),
    // FIXME(#1) separators
    Repeat(RuleBuilder, RepeatKind),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(RuleBuilder, Delimiter),
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
            FragmentBuilder::Repeat(rule, RepeatKind::ZeroOrMore) => {
                let sub_builder_name = &rule.name;
                let match_builder = crate::sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::Repeat,
                );

                tokens.append_all(quote! {
                    while ms.fork(|ps, match_handler| {
                        #match_builder

                        let mut ms: MatchSet<#sub_builder_name> =
                            proc_macro_rules::MatchSet::new(ps);

                        #rule

                        let mb = ms.finalise()?;
                        match_handler.hoist(&mb);

                        Ok(())
                    }) {}
                    ms.reset_states();
                });
            }
            // FIXME (#7)
            FragmentBuilder::Repeat(rule, RepeatKind::OneOrMore) => {}
            FragmentBuilder::Repeat(rule, RepeatKind::ZeroOrOne) => {
                let sub_builder_name = &rule.name;
                let match_builder = crate::sub_matches(
                    sub_builder_name,
                    rule.parent_name.as_ref().unwrap(),
                    &rule.variables,
                    HoistRepeat::Option,
                );

                tokens.append_all(quote! {
                    ms.fork(|ps, match_handler| {
                        #match_builder

                        let mut ms: MatchSet<#sub_builder_name> =
                            proc_macro_rules::MatchSet::new(ps);

                        #rule

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
                            Err(syn::Error::new(proc_macro2::Span::call_site(), "bad ident"))
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
                            Err(syn::Error::new(proc_macro2::Span::call_site(), "bad punct"))
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
                            Err(syn::Error::new(proc_macro2::Span::call_site(), "bad literal"))
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
                let match_builder = crate::sub_matches(
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
                                    return Err(syn::Error::new(proc_macro2::Span::call_site(), "bad delimiter"));
                                }
                                {
                                    #match_builder

                                    struct MatchParser(#sub_builder_name);

                                    impl syn::parse::Parse for MatchParser {
                                        fn parse(ps: syn::parse::ParseStream) -> syn::parse::Result<MatchParser> {
                                            let mut ms: MatchSet<#sub_builder_name> = proc_macro_rules::MatchSet::new(ps.fork());
                                            // parse the whole initial ParseStream to avoid 'unexpected token' errors
                                            let _: Result<proc_macro2::TokenStream, _> = ps.parse();

                                            #rule

                                            let result = ms.finalise()?;
                                            result.into_iter().filter_map(|p| if p.input.is_empty() {
                                                Some(MatchParser(p.matches))
                                            } else {
                                                None
                                            }).next().ok_or_else(|| syn::Error::new(proc_macro2::Span::call_site(), "group could not be parsed"))
                                        }
                                    }

                                    let mb: MatchParser = syn::parse2(g.stream())?;
                                    mb.0.hoist(matches);
                                }
                                Ok(())
                            }
                            _ => Err(syn::Error::new(proc_macro2::Span::call_site(), "expected group")),
                        }
                    })?;
                });
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
crate enum Type {
    Item,
    Block,
    Stmt,
    Expr,
    Pat,
    Lifetime,
    Path,
    Ty,
    Ident,
    Meta,
    Tt,
    Vis,
    Literal,
}

impl Parse for Type {
    fn parse(input: ParseStream) -> ParseResult<Type> {
        let ident: Ident = input.parse()?;
        match &*ident.to_string() {
            "item" => Ok(Type::Item),
            "block" => Ok(Type::Block),
            "stmt" => Ok(Type::Stmt),
            "expr" => Ok(Type::Expr),
            "pat" => Ok(Type::Pat),
            "lifetime" => Ok(Type::Lifetime),
            "path" => Ok(Type::Path),
            "ty" => Ok(Type::Ty),
            "ident" => Ok(Type::Ident),
            "meta" => Ok(Type::Meta),
            "tt" => Ok(Type::Tt),
            "vis" => Ok(Type::Vis),
            "literal" => Ok(Type::Literal),
            s => panic!("Bad fragment type: {}", s),
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

#[derive(Debug)]
crate enum RepeatKind {
    // `*`
    ZeroOrMore,
    // `+`
    OneOrMore,
    // `?`
    ZeroOrOne,
}

impl Parse for RepeatKind {
    fn parse(input: ParseStream) -> ParseResult<RepeatKind> {
        if input.peek(Token!(*)) {
            let _: Token!(*) = input.parse()?;
            Ok(RepeatKind::ZeroOrMore)
        } else if input.peek(Token!(+)) {
            let _: Token!(+) = input.parse()?;
            Ok(RepeatKind::OneOrMore)
        } else if input.peek(Token!(?)) {
            let _: Token!(?) = input.parse()?;
            Ok(RepeatKind::ZeroOrOne)
        } else {
            Err(input.error("Unexpected repeat operator"))
        }
    }
}
