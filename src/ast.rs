use std::time::Duration;

use crate::analysis::AnimationContext;
use crate::analysis::AnimationContextValue;
use crate::parse::*;
use crate::tokens;
use crate::tokens::*;
use derive_parse::*;
use svg::node::element::Element;
use svg::Node;

#[derive(Parse, Debug, Clone)]
pub struct Animation {
    directives: Many1<Directive>,
}

impl Animation {
    pub fn directives(&self) -> impl Iterator<Item = &Directive> {
        self.directives.0.iter()
    }
}

#[derive(Parse, Debug, Clone)]
pub enum Directive {
    Use(Use,Ident),
    Assign(Ident, Equals, Value),
    Func(
        Ident,
        OpenParen,
        Punctated0<Ident, Comma>,
        CloseParen,
        OpenBrace,
        Many1<Directive>,
        CloseBrace,
    ),
    Value(Return, Value),
    Animate(AnimationDef),
    Delay(Delay, NumberLiteral, DurationType),
    Animation(tokens::Animation, Ident, OpenBrace, Animation, CloseBrace),
    Play(Option<Fork>, tokens::Play, Ident, Option<Value>),
}

#[derive(Parse, Debug, Clone)]
pub enum DurationType {
    Seconds(S),
}
impl DurationType {
    pub fn duration(&self, qty: f32) -> Duration {
        match self {
            DurationType::Seconds(_) => Duration::from_secs_f32(qty),
        }
    }
}

#[derive(Parse, Debug, Clone)]
pub struct AnimationDef {
    pub fork: Option<Fork>,
    pub animate: Animate,
    pub duration: NumberLiteral,
    pub duration_type: DurationType,
    pub sprite: Ident,
    pub func: Option<Value>,
}

impl AnimationDef {
    pub fn duration(&self) -> Duration {
        self.duration_type.duration(self.duration.as_f32())
    }

    pub fn is_forked(&self) -> bool {
        self.fork.is_some()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Null(Null),
    Number(NumberLiteral),
    Str(StrLiteral),
    Svg(SvgLiteral),
    FuncCall(Box<Value>, OpenParen, Punctated0<Value, Comma>, CloseParen),
    BinaryOp(Box<Value>, BinaryOperator, Box<Value>),
    Variable(Ident),
    DotField(Box<Value>, Dot, Ident),
    Object(OpenBrace, Punctated0<ObjectField, Comma>, CloseBrace),
}

#[derive(Parse, Debug, Clone)]
pub struct ObjectField(pub Ident, pub Colon, pub Value);

#[derive(Parse, Debug, Clone)]
pub enum BinaryOperator {
    Multiply(Star),
    Subtract(Minus),
    Add(Plus),
    Divide(Slash),
}

impl Value {
    pub fn as_variable(&self) -> Option<&Ident> {
        if let Self::Variable(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Parse for Value {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        #[derive(Parse)]
        enum Simple {
            Null(Null),
            Number(NumberLiteral),
            Str(StrLiteral),
            Svg(SvgLiteral),
            Variable(Ident),
            Parenthesis(OpenParen, Box<Value>, CloseParen),
            Object(OpenBrace, Punctated0<ObjectField, Comma>, CloseBrace),
        }

        let (mut input, simple) = Simple::parse(input)?;
        let mut output = match simple {
            Simple::Null(n) => Value::Null(n),
            Simple::Number(i) => Value::Number(i),
            Simple::Str(s) => Value::Str(s),
            Simple::Variable(i) => Value::Variable(i),
            Simple::Svg(s) => Value::Svg(s),
            Simple::Parenthesis(_, s, _) => *s,
            Simple::Object(open, fields, close) => Value::Object(open, fields, close),
        };

        #[derive(Parse)]
        enum PostFix {
            Call(Call),
            BinaryOp(BinaryOp),
            DotField(Dot, Ident),
        }

        #[derive(Parse)]
        struct Call {
            open_paren: OpenParen,
            args: Punctated0<Value, Comma>,
            close_paren: CloseParen,
        }

        #[derive(Parse)]
        struct BinaryOp(BinaryOperator, Value);

        while let Some((new_input, postfix)) = PostFix::parse(input) {
            match postfix {
                PostFix::Call(c) => {
                    output = Value::FuncCall(Box::new(output), c.open_paren, c.args, c.close_paren);
                }
                PostFix::BinaryOp(op) => {
                    output = Value::BinaryOp(Box::new(output), op.0, Box::new(op.1));
                }
                PostFix::DotField(dot, ident) => {
                    output = Value::DotField(Box::new(output), dot, ident);
                }
            }

            input = new_input;
        }

        Some((input, output))
    }
}

#[derive(Parse, Debug, Clone)]
pub struct SvgLiteral {
    _open: OpenAngle,
    tag: Ident,
    attrs: Many0<SvgAttr>,
    body: SvgLiteralBody,
}

impl SvgLiteral {
    pub fn format(&self, ctx: &mut AnimationContext) -> Element {
        let mut root = Element::new(self.tag.as_str());

        let make_string = |value: AnimationContextValue| match value {
            AnimationContextValue::Number(n) => n.to_string(),
            AnimationContextValue::String(s) => s,
            _ => panic!("failed to convert value to string, {:?}", value),
        };

        for attr in &self.attrs.0 {
            match attr {
                SvgAttr::Formatted(_, name, _, code) => {
                    let value = Value::parse_from_str(code.get_inner_str()).unwrap_or_else(|| {
                        panic!("svg attr failed to parse value: {}", name.as_str())
                    });

                    let value = match value {
                        Value::FuncCall(name, _, args, _) => {
                            let args = args.0
                                .iter()
                                .map(|arg| {
                                    let arg = match arg {
                                        Value::Variable(ident) => ident.as_str(),
                                        Value::Number(i)=> i.as_str(),
                                        Value::Str(s)=>s.get_inner_str().to_string(),
                                            _=> panic!("svg formatting can only handle vars, numbers, and strings")
                                    };
                                    make_string(ctx.get_var(&arg).unwrap())
                                })
                                .collect::<Vec<_>>();
                            format!(
                                "{}({})",
                                name.as_variable().unwrap().as_str(),
                                args.join(", ")
                            )
                        }
                        Value::Variable(ident) => {
                            let value = ctx.get_var(&ident.as_str()).unwrap_or_else(|| {
                                panic!("failed to find variable: {}", ident.as_str())
                            });

                            make_string(value)
                        }
                        x => panic!("{x:?}"),
                    };

                    root.assign(name.as_str(), value);
                }
                SvgAttr::Normal(name, _, literal) => {
                    root.assign(name.as_str(), literal.get_inner_str());
                }
            }
        }
        if let SvgLiteralBody::Open(_, items, _, _, tag, _) = &self.body {
            assert_eq!(self.tag.as_str(), tag.as_str());
            for item in &items.0 {
                let svg = match item {
                    SvgLiteralBodyItem::Svg(svg) => svg.format(ctx),
                    SvgLiteralBodyItem::Formatted(_, _, value, _, _) => {
                        ctx.evaluate(value).as_svg().unwrap().clone()
                    }
                };
                root.append(svg);
            }
        }
        root
    }
}

#[derive(Parse, Debug, Clone)]
pub enum SvgLiteralBody {
    Empty(Slash, CloseAngle),
    Open(
        CloseAngle,
        Many0<SvgLiteralBodyItem>,
        OpenAngle,
        Slash,
        Ident,
        CloseAngle,
    ),
}

#[derive(Parse, Debug, Clone)]
pub enum SvgLiteralBodyItem {
    Svg(SvgLiteral),
    Formatted(OpenBrace, OpenBrace, Value, CloseBrace, CloseBrace),
}

#[derive(Parse, Debug, Clone)]
pub enum SvgAttr {
    Formatted(Colon, SvgAttrName, Equals, StrLiteral),
    Normal(SvgAttrName, Equals, StrLiteral),
}

#[derive(Parse, Debug, Clone)]
pub struct SvgAttrName {
    name: Punctated1<Ident, Minus>,
}

impl SvgAttrName {
    pub fn as_str(&self) -> String {
        self.name
            .0
            .iter()
            .map(|x| x.as_str())
            .collect::<Vec<_>>()
            .join("-")
    }
}
