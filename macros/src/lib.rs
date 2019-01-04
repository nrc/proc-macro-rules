#![recursion_limit = "256"]
#![feature(crate_visibility_modifier)]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;

crate mod ast;
crate mod expand;
crate mod parse;

#[proc_macro]
pub fn rules(input: TokenStream) -> TokenStream {
    let rules: ast::Rules = syn::parse(input).expect("Parsing error");
    let result = expand::expand_rules(rules).into();
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
            ast::Fragment::Repeat(sub_rule, rkind) => {
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
