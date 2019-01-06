use crate::collect_vars;

use proc_macro2::{Delimiter, Literal, Punct, Span};
use syn::{Block, Expr, Ident};

#[derive(Debug)]
pub(crate) struct Rules {
    pub(crate) clause: Expr,
    pub(crate) rules: Vec<Rule>,
}

#[derive(Debug)]
pub(crate) struct Rule {
    pub(crate) lhs: SubRule,
    pub(crate) rhs: Rhs,
}

#[derive(Debug, Clone)]
pub(crate) enum Rhs {
    Expr(Expr),
    Block(Block),
}

#[derive(Debug, Clone)]
pub(crate) struct SubRule {
    pub(crate) matchers: Vec<Fragment>,
}

#[derive(Debug, Clone)]
pub(crate) enum Fragment {
    Var(Ident, Type),
    Repeat(SubRule, RepeatKind, Option<Punct>),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(SubRule, Delimiter),
}

#[derive(Debug, Clone)]
pub(crate) enum RepeatKind {
    // `*`
    ZeroOrMore,
    // `+`
    OneOrMore,
    // `?`
    ZeroOrOne,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Type {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct MetaVar {
    pub(crate) name: Ident,
    pub(crate) ty: MetaVarType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum MetaVarType {
    Vec(Box<MetaVarType>),
    Option(Box<MetaVarType>),
    T(Type),
}

pub(crate) struct RuleBuilder {
    pub(crate) matchers: Vec<FragmentBuilder>,
    pub(crate) variables: Vec<MetaVar>,
    pub(crate) parent_name: Option<Ident>,
    pub(crate) name: Ident,
}

pub(crate) enum FragmentBuilder {
    Var(Ident, Type),
    Repeat(RuleBuilder, RepeatKind, Option<Punct>),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(RuleBuilder, Delimiter),
}

impl SubRule {
    pub(crate) fn to_builder(self, parent_name: Option<Ident>) -> RuleBuilder {
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
    pub(crate) fn to_builder(self, parent_name: Option<Ident>) -> FragmentBuilder {
        match self {
            Fragment::Var(i, t) => FragmentBuilder::Var(i, t),
            Fragment::Repeat(r, rep, sep) => FragmentBuilder::Repeat(r.to_builder(parent_name), rep, sep),
            Fragment::Ident(i) => FragmentBuilder::Ident(i),
            Fragment::Punct(p) => FragmentBuilder::Punct(p),
            Fragment::Literal(l) => FragmentBuilder::Literal(l),
            Fragment::Group(r, d) => FragmentBuilder::Group(r.to_builder(parent_name), d),
        }
    }
}
