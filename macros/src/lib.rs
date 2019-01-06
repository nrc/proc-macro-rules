#![recursion_limit = "256"]
#![feature(crate_visibility_modifier)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

crate mod ast;
crate mod expand;
crate mod parse;

#[proc_macro]
pub fn rules(input: TokenStream) -> TokenStream {
    let rules = parse_macro_input!(input as ast::Rules);
    let result = rules.into_token_stream().into();
    // println!("{}", result);
    result
}

fn verify_rule(_rule: &ast::SubRule) {
    // FIXME(#11) pattern rule verification
}

// FIXME(#12) we could save some computation by using intermediate results from the SubRules.
fn collect_vars(rule: &ast::SubRule, vars: &mut Vec<ast::MetaVar>) {
    for m in &rule.matchers {
        match m {
            ast::Fragment::Var(id, ty) => vars.push(ast::MetaVar {
                name: id.clone(),
                ty: ast::MetaVarType::T(*ty),
            }),
            ast::Fragment::Repeat(sub_rule, rkind, _) => {
                let mut sub = vec![];
                collect_vars(sub_rule, &mut sub);
                for s in sub {
                    vars.push(match rkind {
                        ast::RepeatKind::ZeroOrMore | ast::RepeatKind::OneOrMore => ast::MetaVar {
                            name: s.name,
                            ty: ast::MetaVarType::Vec(Box::new(s.ty)),
                        },
                        ast::RepeatKind::ZeroOrOne => ast::MetaVar {
                            name: s.name,
                            ty: ast::MetaVarType::Option(Box::new(s.ty)),
                        },
                    })
                }
            }
            ast::Fragment::Group(sub_rule, _) => {
                collect_vars(sub_rule, vars);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;
    use syn::Ident;
    use proc_macro2::Span;

    #[test]
    fn test_collect_vars() {
        fn run_test(rule: ast::SubRule, expected: Vec<MetaVar>) {
            let mut result = vec![];
            collect_vars(&rule, &mut result);
            assert_eq!(result, expected);
        }

        // ``
        let ast = ast::SubRule {
            matchers: vec![],
        };
        run_test(ast, vec![]);

        // `$foo:vis`
        let ast = ast::SubRule {
            matchers: vec![Fragment::Var(Ident::new("foo", Span::call_site()), Type::Vis)],
        };
        run_test(ast, vec![MetaVar {
            name: Ident::new("foo", Span::call_site()),
            ty: MetaVarType::T(Type::Vis),
        }]);

        // `foo`
        let ast = ast::SubRule {
            matchers: vec![Fragment::Ident(Ident::new("foo", Span::call_site()))],
        };
        run_test(ast, vec![]);

        // `foo $bar:Tt $($foo:expr)*`
        let ast = ast::SubRule {
            matchers: vec![
                Fragment::Ident(Ident::new("foo", Span::call_site())),
                Fragment::Var(Ident::new("bar", Span::call_site()), Type::Tt),
                Fragment::Repeat(SubRule {
                    matchers: vec![Fragment::Var(Ident::new("foo", Span::call_site()), Type::Expr)],
                }, RepeatKind::OneOrMore),
            ],
        };
        run_test(ast, vec![
            MetaVar {
                name: Ident::new("bar", Span::call_site()),
                ty: MetaVarType::T(Type::Tt),
            },
            MetaVar {
                name: Ident::new("foo", Span::call_site()),
                ty: MetaVarType::Vec(Box::new(MetaVarType::T(Type::Expr))),
            },
        ]);
    }
}
