use crate::ast::*;

use proc_macro2::TokenTree as TokenTree2;
use syn::parse::{Parse, ParseStream, Result as ParseResult};
use syn::{
    braced, parenthesized, parse2,
    token::{Brace, Comma, Dollar, FatArrow, Paren},
    Ident, Token,
};

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

impl Parse for SubRule {
    fn parse(input: ParseStream) -> ParseResult<SubRule> {
        let mut matchers = vec![];
        while !input.is_empty() {
            matchers.push(input.parse()?);
        }
        Ok(SubRule { matchers })
    }
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
