use std::{time::Duration, collections::HashMap};

use svg::node::element::Element;

use crate::{ast::DurationLiteral, parse::*, tokens::*};

#[derive(Debug, Parse)]
pub struct IR {
    statements: Many1<IRStatement>,
}

#[derive(Debug, Parse)]
pub enum IRStatement {
    SvgAssign(SvgAssign, Ident, Ident),
    SvgAppend(SvgAppend, Ident, Ident),
    Abstraction(Ident, OpenBrace, IR, CloseBrace),
    Assign(Ident, Equals, IRValue),
    Return(Return, Ident),
    Play(Play, Ident, Ident, Option<Ident>),
}

#[derive(Debug, Parse)]
pub enum IRValue {
    Arg(Arg, NumberLiteral),
    SvgNew(SvgNew, Ident),
    Call(
        Call,
        Ident,
        OpenSquareBracket,
        Punctated0<Ident, Comma>,
        CloseSquareBracket,
    ),
    Duration(DurationLiteral),
    Number(NumberLiteral),
    Str(StrLiteral),
}

#[derive(Debug)]
pub struct IRContext {
    ir: IR,
    vars: HashMap<String, IRContextValue>,
}

impl IRContext {
    pub fn new(ir: IR) -> Self {
        Self {
            ir,
            vars: HashMap::default(),
        }
    }
}

#[derive(Debug)]
pub enum IRContextValue {
    Duration(Duration),
    Number(f32),
    Svg(Element),
}
