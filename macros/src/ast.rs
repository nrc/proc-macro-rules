use crate::collect_vars;

use proc_macro2::{Delimiter, Literal, Punct, Span};
use syn::{Block, Expr, Ident};

#[derive(Debug)]
crate struct Rules {
    crate clause: Expr,
    crate rules: Vec<Rule>,
}

#[derive(Debug)]
crate struct Rule {
    crate lhs: SubRule,
    crate rhs: Rhs,
}

#[derive(Debug)]
crate enum Rhs {
    Expr(Expr),
    Block(Block),
}

#[derive(Debug)]
crate struct SubRule {
    crate matchers: Vec<Fragment>,
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

#[derive(Debug)]
crate enum RepeatKind {
    // `*`
    ZeroOrMore,
    // `+`
    OneOrMore,
    // `?`
    ZeroOrOne,
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

#[derive(Debug, Clone)]
crate struct MetaVar {
    crate name: Ident,
    crate ty: MetaVarType,
}

#[derive(Debug, Clone)]
crate enum MetaVarType {
    Vec(Box<MetaVarType>),
    Option(Box<MetaVarType>),
    T(Type),
}

crate struct RuleBuilder {
    crate matchers: Vec<FragmentBuilder>,
    crate variables: Vec<MetaVar>,
    crate parent_name: Option<Ident>,
    crate name: Ident,
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

// FIXME(#10) WARNING: VERY THREAD-UNSAFE
fn next_builder_name() -> String {
    static mut NEXT_ID: u32 = 0;
    unsafe {
        NEXT_ID += 1;
        format!("MatchesBuilder{}", NEXT_ID)
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
