use proc_macro2::Span;
use std::collections::HashSet;
use syn::parse::{Error, ParseBuffer, ParseStream};

pub struct MatchSet<'a, M: Fork> {
    positions: Vec<Position<'a, M>>,
    garbage: HashSet<usize>,
}

pub trait Fork {
    type Parent;

    fn fork(&self) -> Self;
    fn new() -> Self;
    fn hoist(&self, outer: &mut Self::Parent);
}

pub struct Position<'a, M: Fork> {
    pub input: ParseBuffer<'a>,
    pub matches: M,
    state: FragmentState,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum FragmentState {
    Ready,
    Exhausted,
}

pub struct MatchHandler<'a, 'b: 'a, M: Fork> {
    outer_position: &'a mut Position<'b, M>,
    forked: Vec<Position<'b, M>>,
}

impl<'a, M: Fork> MatchSet<'a, M> {
    pub fn new(initial: ParseBuffer<'a>) -> MatchSet<'a, M> {
        let result = MatchSet {
            positions: vec![Position {
                input: initial,
                matches: M::new(),
                state: FragmentState::Ready,
            }],
            garbage: HashSet::new(),
        };

        result
    }

    pub fn finalise(self) -> syn::parse::Result<Vec<Position<'a, M>>> {
        if self.positions.is_empty() {
            return Err(Error::new(Span::call_site(), "No match"));
        }
        Ok(self.positions)
    }

    // return value = if any positions were forked
    pub fn fork<F>(&mut self, mut f: F) -> bool
    where
        for<'b> F: FnMut(ParseBuffer<'a>, &mut MatchHandler<'b, 'a, M>) -> Result<(), Error>,
    {
        debug_assert!(self.garbage.is_empty());

        let mut forked = vec![];
        for p in self.positions.iter_mut() {
            if p.state != FragmentState::Ready {
                continue;
            }
            p.state = FragmentState::Exhausted;
            let forked_input = p.input.fork();
            let mut match_handler = MatchHandler {
                outer_position: p,
                forked: vec![],
            };
            match f(forked_input, &mut match_handler) {
                Ok(_) => forked.append(&mut match_handler.forked),
                Err(_) => {}
            }
        }

        self.positions
            .iter()
            .filter(|p| p.state == FragmentState::Ready)
            .count()
            > 0
    }

    pub fn reset_states(&mut self) {
        for p in self.positions.iter_mut() {
            p.state = FragmentState::Ready;
        }
    }

    // returns err if the set is non-empty
    pub fn expect<F>(&mut self, mut f: F) -> Result<(), Error>
    where
        for<'b> F: FnMut(ParseStream<'b>, &'b mut M) -> Result<(), Error>,
    {
        debug_assert!(self.garbage.is_empty());

        for (i, p) in self.positions.iter_mut().enumerate() {
            match f(&p.input, &mut p.matches) {
                Ok(_) => {}
                Err(_) => {
                    self.garbage.insert(i);
                }
            }
        }

        self.compact();

        if self.positions.is_empty() {
            Err(Error::new(Span::call_site(), "no positions passed expect"))
        } else {
            Ok(())
        }
    }

    fn compact(&mut self) {
        if self.garbage.is_empty() {
            return;
        }

        let mut new = Vec::with_capacity(self.positions.len() - self.garbage.len());
        for (i, p) in self.positions.drain(..).enumerate() {
            if !self.garbage.contains(&i) {
                new.push(p);
            }
        }

        self.positions = new;
        self.garbage = HashSet::new();
    }
}

impl<'a, M: Fork> Position<'a, M> {
    pub fn stream(&'a self) -> ParseStream<'a> {
        &self.input
    }

    pub fn fork(&self) -> Position<'a, M> {
        Position {
            input: self.input.fork(),
            matches: self.matches.fork(),
            state: FragmentState::Ready,
        }
    }
}

impl<'a, 'b: 'a, M: Fork> MatchHandler<'a, 'b, M> {
    pub fn hoist<MM: Fork<Parent = M>>(&mut self, positions: &[Position<'b, MM>]) {
        assert!(!positions.is_empty());

        for p in &positions[1..] {
            let mut new = Position {
                input: p.input.fork(),
                matches: self.outer_position.matches.fork(),
                state: FragmentState::Ready,
            };
            p.matches.hoist(&mut new.matches);
            self.forked.push(new);
        }

        positions[0].matches.hoist(&mut self.outer_position.matches);
        let _: Result<proc_macro2::TokenStream, _> = self.outer_position.input.parse();
        self.outer_position.state = FragmentState::Ready;
        self.outer_position.input = positions[0].input.fork();
    }

    pub fn matches(&mut self) -> &mut M {
        &mut self.outer_position.matches
    }
}
