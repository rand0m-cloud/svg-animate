use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{digit1, multispace0, one_of},
    combinator::{all_consuming, map, recognize, value},
    multi::many1,
    sequence::{delimited, pair},
    Finish, IResult,
};

use crate::parse::{Span, Token};

pub fn lex<'a>(input: &'a str) -> Vec<Token<'a>> {
    fn parser(input: Span) -> IResult<Span, Vec<Option<Token>>> {
        let punctation = "()<>:-+*={}&;|/![].,";
        all_consuming(many1(delimited(
            multispace0,
            alt((
                value(None, pair(tag("#"), is_not("\r\n"))),
                map(
                    alt((
                        recognize(delimited(tag("\""), is_not("\""), tag("\""))),
                        recognize(digit1),
                        recognize(one_of(punctation)),
                        recognize(many1(is_not(&*format!(" \t\n\r{}", punctation)))),
                    )),
                    |s: Span| Some(Token::new(s.fragment()).with_span(s)),
                ),
            )),
            multispace0,
        )))(input)
    }

    parser(Span::new(input))
        .finish()
        .unwrap()
        .1
        .into_iter()
        .flatten()
        .collect()
}
