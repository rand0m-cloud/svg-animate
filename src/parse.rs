use std::marker::PhantomData;

use derive_more::*;
pub use derive_parse::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1},
    combinator::recognize,
    multi::many0,
    sequence::pair,
    Finish,
};

use crate::{lex::lex, tokens::*};

pub trait Parse: Sized {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)>;
    fn parse_from_str(input: &str) -> Option<Self> {
        let lexed = lex(input);
        let (remaining, this) = Self::parse(&lexed)?;
        assert!(
            remaining.is_empty(),
            "not all input lexed into tokens\n\n{remaining:?}"
        );
        Some(this)
    }
}

#[derive(Clone)]
pub struct Token(String);

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.write_str(self.value())
    }
}

impl Token {
    pub(crate) fn new(value: impl ToString) -> Self {
        Self(value.to_string())
    }

    pub fn value(&self) -> &str {
        &self.0
    }
}

impl Parse for Token {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let token = input.get(0)?.clone();
        Some((&input[1..], token))
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, _) = OpenSquareBracket::parse(input)?;
        let (input, values) = Punctated0::<_, Comma>::parse(input)?;
        let (input, _) = CloseSquareBracket::parse(input)?;
        Some((input, values.0))
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        if let Some((input, t)) = T::parse(input) {
            Some((input, Some(t)))
        } else {
            Some((input, None))
        }
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, t) = T::parse(input)?;
        Some((input, Box::new(t)))
    }
}
impl<T1: Parse, T2: Parse> Parse for (T1, T2) {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, t1) = Parse::parse(input)?;
        let (input, t2) = Parse::parse(input)?;
        Some((input, (t1, t2)))
    }
}

impl Token {
    pub fn parse_token<'a>(token: &'a str, input: &'a [Token]) -> Option<(&'a [Token], Self)> {
        let (input, t) = Self::parse(input)?;
        (t.value() == token).then_some(())?;
        Some((input, t))
    }
}

#[derive(Debug, Clone, Deref, PartialEq)]
pub struct Ident(pub Token);

impl Parse for Ident {
    fn parse<'a>(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, t) = Token::parse(input)?;

        let res: Result<_, nom::error::Error<&str>> = recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(t.value())
        .finish();
        let (_, val) = res.ok()?;

        if [
            "fork",
            "animate",
            "return",
            "null",
            "delay",
            "animation",
            "play",
            "use"
        ]
        .contains(&val)
        {
            return None;
        }

        Some((input, Ident(t)))
    }
}

impl Ident {
    pub fn as_str(&self) -> String {
        self.0.value().to_string()
    }
}

#[derive(Debug, Clone, Deref)]
pub struct StrLiteral(pub Token);

impl Parse for StrLiteral {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, t) = Token::parse(input)?;
        if !t.value().starts_with('"') {
            return None;
        }

        Some((input, StrLiteral(t)))
    }
}

impl StrLiteral {
    pub fn get_inner_str(&self) -> &str {
        self.0
            .value()
            .strip_prefix('"')
            .unwrap()
            .strip_suffix('"')
            .unwrap()
    }
}

#[derive(Debug, Clone, Deref)]
pub struct Integer(pub Token);

impl Parse for Integer {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, t) = Token::parse(input)?;

        let res: Result<_, nom::error::Error<&str>> = digit1(t.value()).finish();
        res.ok()?;

        Some((input, Integer(t)))
    }
}

impl Integer {
    pub fn as_str(&self) -> String {
        self.0.value().to_string()
    }
}

#[derive(Debug, Parse, Clone)]
pub struct NumberLiteral(Option<Minus>, Integer, Option<(Dot, Integer)>);

impl NumberLiteral {
    pub fn as_f32(&self) -> f32 {
        self.as_str().parse().unwrap()
    }

    pub fn as_str(&self) -> String {
        let negative = self.0.as_ref().map(|_| "-").unwrap_or_else(|| "");
        if let Some((_, int)) = &self.2 {
            format!("{}{}.{}", negative, self.1.as_str(), int.as_str())
        } else {
            format!("{}{}", negative, self.1.as_str())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Many0<T>(pub Vec<T>);

impl<T: Parse> Parse for Many0<T> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let mut output = vec![];
        let mut input = input;

        while let Some((i, v)) = T::parse(input) {
            input = i;
            output.push(v);
        }

        Some((input, Many0(output)))
    }
}

#[derive(Debug, Clone)]
pub struct Many1<T>(pub Vec<T>);

impl<T: Parse> Parse for Many1<T> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let mut output = vec![];
        let mut input = input;
        while let Some((i, v)) = T::parse(input) {
            input = i;
            output.push(v);
        }

        if output.is_empty() {
            None
        } else {
            Some((input, Many1(output)))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Punctated0<T, P>(pub Vec<T>, pub PhantomData<P>);

impl<T: Parse, P: Parse> Parse for Punctated0<T, P> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let mut output = vec![];
        let mut input = input;
        while let Some((i, v)) = T::parse(input) {
            input = i;
            output.push(v);

            if let Some((i, _p)) = P::parse(input) {
                input = i;
            } else {
                break;
            }
        }
        Some((input, Punctated0(output, PhantomData)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctated1<T, P>(pub Vec<T>, pub PhantomData<P>);

impl<T: Parse, P: Parse> Parse for Punctated1<T, P> {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        let (input, punctated_0) = Punctated0::<T, P>::parse(input)?;

        if punctated_0.0.is_empty() {
            return None;
        }

        Some((input, Punctated1(punctated_0.0, PhantomData)))
    }
}
