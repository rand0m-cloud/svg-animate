use std::time::Duration;

use crate::analysis::AnimationContext;
use crate::analysis::AnimationContextValue;
use crate::parse::*;
use crate::tokens::*;
use derive_parse::*;
use svg::node::element::Element;
use svg::Node;

#[derive(ParseOwned, Debug, Clone)]
pub struct Animation {
    directives: Many1<Directive>,
}

impl Animation {
    pub fn animations(&self) -> impl Iterator<Item = &AnimationDef> {
        self.directives.iter().filter_map(|x| match x {
            Directive::Animate(def) => Some(def),
            _ => None,
        })
    }

    pub fn directives(&self) -> impl Iterator<Item = &Directive> {
        self.directives.iter()
    }
}

#[derive(ParseOwned, Debug, Clone)]
pub enum Directive {
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
}

#[derive(ParseOwned, Debug, Clone)]
pub enum DurationType {
    Seconds(S),
}

#[derive(ParseOwned, Debug, Clone)]
pub struct AnimationDef {
    pub fork: Option<Fork>,
    pub animate: Animate,
    pub duration: IntLiteral,
    pub duration_type: DurationType,
    pub sprite: Ident,
    pub func: Option<Value>,
}

impl AnimationDef {
    pub fn duration(&self) -> Duration {
        match self.duration_type {
            DurationType::Seconds(_) => Duration::from_secs(self.duration.as_usize() as u64),
        }
    }

    pub fn is_forked(&self) -> bool {
        self.fork.is_some()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Null(Null),
    Int(IntLiteral),
    Str(StrLiteral),
    Svg(SvgLiteral),
    FuncCall(Box<Value>, OpenParen, Punctated0<Value, Comma>, CloseParen),
    BinaryOp(Box<Value>, BinaryOperator, Box<Value>),
    Variable(Ident),
}

#[derive(ParseOwned, Debug, Clone)]
pub enum BinaryOperator {
    Multiply(Star),
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

impl ParseOwned for Value {
    fn parse_owned<'a, 'b>(input: &'b [Token<'a>]) -> Option<(&'b [Token<'a>], Self)>
    where
        'a: 'b,
    {
        #[derive(ParseOwned)]
        enum Simple {
            Null(Null),
            Int(IntLiteral),
            Str(StrLiteral),
            Svg(SvgLiteral),
            Variable(Ident),
        }

        let (input, simple) = Simple::parse_owned(input)?;

        let mut input = input;
        let mut output = match simple {
            Simple::Null(n) => Value::Null(n),
            Simple::Int(i) => Value::Int(i),
            Simple::Str(s) => Value::Str(s),
            Simple::Variable(i) => Value::Variable(i),
            Simple::Svg(s) => Value::Svg(s),
        };

        #[derive(ParseOwned)]
        enum PostFix {
            Call(Call),
            BinaryOp(BinaryOp),
        }

        #[derive(ParseOwned)]
        struct Call {
            open_paren: OpenParen,
            args: Punctated0<Value, Comma>,
            close_paren: CloseParen,
        }

        #[derive(ParseOwned)]
        struct BinaryOp(BinaryOperator, Value);

        while let Some((new_input, postfix)) = PostFix::parse_owned(input) {
            match postfix {
                PostFix::Call(c) => {
                    output = Value::FuncCall(Box::new(output), c.open_paren, c.args, c.close_paren);
                }
                PostFix::BinaryOp(op) => {
                    output = Value::BinaryOp(Box::new(output), op.0, Box::new(op.1));
                }
            }

            input = new_input;
        }

        Some((input, output))
    }
}

#[derive(ParseOwned, Debug, Clone)]
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
                            let args = args
                                .into_iter()
                                .map(|arg| {
                                    let arg = match arg {
                                        Value::Variable(ident) => ident.as_str(),
                                        Value::Int(i)=> i.as_str(),
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
                        _ => panic!(),
                    };

                    root.assign(name.as_str(), value);
                }
                SvgAttr::Normal(name, _, literal) => {
                    root.assign(name.as_str(), literal.get_inner_str());
                }
            }
        }
        match &self.body {
            SvgLiteralBody::Open(_, items, _, _, tag, _) => {
                assert_eq!(self.tag.as_str(), tag.as_str());
                for item in &items.0 {
                    let svg = match item {
                        SvgLiteralBodyItem::Svg(svg) => svg.format(ctx),
                        SvgLiteralBodyItem::Formatted(_, _, value, _, _) => {
                            ctx.evaluate(&value).as_svg().unwrap().clone()
                        }
                    };
                    root.append(svg);
                }
            }
            _ => {}
        }
        root
    }
}

#[derive(ParseOwned, Debug, Clone)]
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

#[derive(ParseOwned, Debug, Clone)]
pub enum SvgLiteralBodyItem {
    Svg(SvgLiteral),
    Formatted(OpenBrace, OpenBrace, Value, CloseBrace, CloseBrace),
}

#[derive(ParseOwned, Debug, Clone)]
pub enum SvgAttr {
    Formatted(Colon, Ident, Equals, StrLiteral),
    Normal(Ident, Equals, StrLiteral),
}
