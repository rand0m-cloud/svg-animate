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
use nom_locate::LocatedSpan;

use crate::{lex::lex, tokens::*};

pub type Span<'a> = LocatedSpan<&'a str>;
pub type OwnedSpan = LocatedSpan<String>;

pub trait Parse<'a>: Sized {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b;
}

pub trait ParseOwned: Sized {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b;

    fn parse_from_str(input: &str) -> Option<Self> {
        let tokens = lex(input);
        let (input, out) = Self::parse_owned(&tokens)?;
        if input.is_empty() {
            Some(out)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct Token<'a> {
    borrowed_input: Option<&'a str>,
    owned_input: Option<String>,
    span: Option<Span<'a>>,
    owned_span: Option<OwnedSpan>,
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.write_str(self.value())
    }
}

impl<'a> Token<'a> {
    pub(crate) fn new(value: &'a str) -> Self {
        Self {
            borrowed_input: Some(value),
            owned_input: None,
            span: None,
            owned_span: None,
        }
    }

    pub fn with_span(mut self, span: Span<'a>) -> Self {
        self.span = Some(span);
        self
    }

    pub fn value(&self) -> &str {
        if let Some(b) = self.borrowed_input {
            b
        } else {
            self.owned_input.as_ref().unwrap()
        }
    }

    pub fn as_owned_token(&self) -> Token<'static> {
        let owned_span = if self.span.is_some() || self.owned_span.is_some() {
            Some(self.span())
        } else {
            None
        };
        Token {
            borrowed_input: None,
            owned_input: Some(self.value().to_string()),
            span: None,
            owned_span,
        }
    }

    pub fn span(&self) -> OwnedSpan {
        if let Some(span) = self.owned_span.as_ref() {
            span.clone()
        } else if let Some(span) = self.span {
            unsafe {
                OwnedSpan::new_from_raw_offset(
                    span.location_offset(),
                    span.location_line(),
                    span.to_string(),
                    (),
                )
            }
        } else {
            OwnedSpan::new("".to_string())
        }
    }

    pub fn set_span(&mut self, span: OwnedSpan) {
        self.span = None;
        self.owned_span = Some(span);
    }
}

impl<'a> Parse<'a> for Token<'a> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let token = input.get(0)?.clone();
        Some((&input[1..], token))
    }
}

impl ParseOwned for Token<'static> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse(input)?;
        Some((input, t.as_owned_token()))
    }
}

impl ParseOwned for String {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse(input)?;
        Some((input, t.value().to_string()))
    }
}

impl ParseOwned for usize {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse(input)?;
        Some((input, t.value().parse().ok()?))
    }
}

impl<T: ParseOwned> ParseOwned for Vec<T> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, _) = OpenSquareBracket::parse_owned(input)?;
        let (input, values) = Punctated0::<_, Comma>::parse_owned(input)?;
        let (input, _) = CloseSquareBracket::parse_owned(input)?;
        Some((input, values.0))
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Option<T> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        if let Some((input, t)) = T::parse(input) {
            Some((input, Some(t)))
        } else {
            Some((input, None))
        }
    }
}

impl<T: ParseOwned> ParseOwned for Option<T> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        if let Some((input, t)) = T::parse_owned(input) {
            Some((input, Some(t)))
        } else {
            Some((input, None))
        }
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Box<T> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = T::parse(input)?;
        Some((input, Box::new(t)))
    }
}

impl<T: ParseOwned> ParseOwned for Box<T> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = T::parse_owned(input)?;
        Some((input, Box::new(t)))
    }
}

impl<'a, T1: Parse<'a>, T2: Parse<'a>> Parse<'a> for (T1, T2) {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t1) = Parse::parse(input)?;
        let (input, t2) = Parse::parse(input)?;
        Some((input, (t1, t2)))
    }
}

impl<T1: ParseOwned, T2: ParseOwned> ParseOwned for (T1, T2) {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t1) = ParseOwned::parse_owned(input)?;
        let (input, t2) = ParseOwned::parse_owned(input)?;
        Some((input, (t1, t2)))
    }
}

impl<'a> Token<'a> {
    pub fn parse_token<'b>(token: &str, input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)> {
        let (input, t) = Self::parse(input)?;
        (t.value() == token).then_some(())?;
        Some((input, t))
    }
}
impl Token<'static> {
    pub fn parse_token_owned<'a, 'b>(
        token: &str,
        input: &'b [Token<'a>],
    ) -> Option<(&'b [Token<'a>], Self)> {
        let (input, t) = Token::parse_token(token, input)?;
        Some((input, t.as_owned_token()))
    }
}

#[derive(Debug, Clone, Deref, PartialEq)]
pub struct Ident(pub Token<'static>);

impl<'a> Parse<'a> for Ident {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse_owned(input)?;

        let res: Result<_, nom::error::Error<&str>> = recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(t.value())
        .finish();
        let (_, val) = res.ok()?;

        if ["fork", "animate", "return"].contains(&val) {
            return None;
        }

        Some((input, Ident(t)))
    }
}

impl ParseOwned for Ident {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, ident) = Ident::parse(input)?;
        Some((input, Ident(ident.0.as_owned_token())))
    }
}

impl Ident {
    pub fn as_str(&self) -> String {
        self.0.value().to_string()
    }
}

#[derive(Debug, Clone, Deref)]
pub struct StrLiteral(pub Token<'static>);

impl<'a> Parse<'a> for StrLiteral {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse_owned(input)?;
        if !t.value().starts_with('"') {
            return None;
        }

        Some((input, StrLiteral(t)))
    }
}

impl ParseOwned for StrLiteral {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, lit) = StrLiteral::parse(input)?;
        Some((input, StrLiteral(lit.0.as_owned_token())))
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
pub struct IntLiteral(pub Token<'static>);

impl<'a> Parse<'a> for IntLiteral {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, t) = Token::parse_owned(input)?;

        let res: Result<_, nom::error::Error<&str>> = digit1(t.value()).finish();
        res.ok()?;

        Some((input, IntLiteral(t)))
    }
}

impl ParseOwned for IntLiteral {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, lit) = IntLiteral::parse(input)?;
        Some((input, IntLiteral(lit.0.as_owned_token())))
    }
}

impl IntLiteral {
    pub fn as_usize(&self) -> usize {
        self.0.value().parse().unwrap()
    }

    pub fn as_str(&self) -> String {
        self.0.value().to_string()
    }
}

#[derive(Debug, Clone, From, Deref, DerefMut)]
#[deref(forward)]
#[deref_mut(forward)]
pub struct Many0<T>(pub Vec<T>);

impl<'a, T: Parse<'a>> Parse<'a> for Many0<T> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let mut output = vec![];
        let mut input = input;

        while let Some((i, v)) = T::parse(input) {
            input = i;
            output.push(v);
        }

        Some((input, Many0(output)))
    }
}

impl<T: ParseOwned> ParseOwned for Many0<T> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let mut output = vec![];
        let mut input = input;

        while let Some((i, v)) = T::parse_owned(input) {
            input = i;
            output.push(v);
        }

        Some((input, Many0(output)))
    }
}

#[derive(Debug, Clone, From, Deref, DerefMut)]
#[deref(forward)]
#[deref_mut(forward)]
pub struct Many1<T>(pub Vec<T>);

impl<'a, T: Parse<'a>> Parse<'a> for Many1<T> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
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

impl<T: ParseOwned> ParseOwned for Many1<T> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let mut output = vec![];
        let mut input = input;
        while let Some((i, v)) = T::parse_owned(input) {
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

#[derive(Debug, Clone, Deref, DerefMut)]
#[deref(forward)]
#[deref_mut(forward)]
pub struct Punctated0<T, P>(
    pub Vec<T>,
    #[deref(ignore)]
    #[deref_mut(ignore)]
    PhantomData<P>,
);

impl<'a, T: Parse<'a>, P: Parse<'a>> Parse<'a> for Punctated0<T, P> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
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

impl<T: ParseOwned, P: ParseOwned> ParseOwned for Punctated0<T, P> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let mut output = vec![];
        let mut input = input;
        while let Some((i, v)) = T::parse_owned(input) {
            input = i;
            output.push(v);

            if let Some((i, _p)) = P::parse_owned(input) {
                input = i;
            } else {
                break;
            }
        }
        Some((input, Punctated0(output, PhantomData)))
    }
}

#[derive(Debug, Clone, Deref, DerefMut, PartialEq, Eq)]
#[deref(forward)]
#[deref_mut(forward)]
pub struct Punctated1<T, P>(
    pub Vec<T>,
    #[deref(ignore)]
    #[deref_mut(ignore)]
    PhantomData<P>,
);

impl<'a, T: Parse<'a>, P: Parse<'a>> Parse<'a> for Punctated1<T, P> {
    fn parse<'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, punctated_0) = Punctated0::<T, P>::parse(input)?;

        if punctated_0.0.is_empty() {
            return None;
        }

        Some((input, Punctated1(punctated_0.0, PhantomData)))
    }
}
impl<T: ParseOwned, P: ParseOwned> ParseOwned for Punctated1<T, P> {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        let (input, punctated_0) = Punctated0::<T, P>::parse_owned(input)?;

        if punctated_0.0.is_empty() {
            return None;
        }

        Some((input, Punctated1(punctated_0.0, PhantomData)))
    }
}
