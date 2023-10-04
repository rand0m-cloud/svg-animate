use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till},
    character::complete::{digit1, multispace0, one_of},
    combinator::{all_consuming, map, recognize, value},
    multi::many1,
    sequence::{delimited, pair},
    Finish, IResult,
};

use crate::parse::Token;

pub fn lex(input: &str) -> Vec<Token> {
    let punctation = "()<>:-+*={}&;|/![].,";
    let res: IResult<&str, Vec<Option<Token>>> = all_consuming(many1(delimited(
        multispace0,
        alt((
            value(None, pair(tag("#"), take_till(|c| c == '\r' || c == '\n'))),
            map(
                alt((
                    recognize(delimited(tag("\""), is_not("\""), tag("\""))),
                    recognize(digit1),
                    recognize(one_of(punctation)),
                    recognize(many1(is_not(&*format!(" \t\n\r{}", punctation)))),
                )),
                |s: &str| Some(Token::new(s)),
            ),
        )),
        multispace0,
    )))(input);

    res.finish().unwrap().1.into_iter().flatten().collect()
}
