use std::{collections::HashMap, mem, time::Duration};

use resvg::{
    tiny_skia::{Color, Pixmap, PixmapRef},
    usvg::{self, Options, Transform, TreeParsing},
};
use svg::{node::element::Element, Node};

use crate::{parse::*, tokens::*};

#[derive(Debug, Parse)]
pub struct IR(Many1<IRBlock>);

#[derive(Debug, Parse, Clone)]
pub struct IRBlock {
    name: Ident,
    _open: OpenBrace,
    body: Many1<IRStatement>,
    _close: CloseBrace,
}

#[derive(Debug, Parse, Clone)]
pub enum IRStatement {
    SvgAssign(SvgAssign, Ident, StrLiteral, Ident),
    SvgAppend(SvgAppend, Ident, Ident),
    Play(
        Play,
        Ident,
        Ident,
        Option<(OpenSquareBracket, Ident, CloseSquareBracket)>,
    ),
    Assign(Ident, Equals, IRValue),
    Return(Return, Ident),
}

#[derive(Debug, Parse, Clone)]
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
    BinaryOp(Ident, BinaryOperator, Ident),
}

#[derive(Parse, Debug, Clone)]
pub enum BinaryOperator {
    Multiply(Star),
    Subtract(Minus),
    Add(Plus),
    Divide(Slash),
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
pub struct DurationLiteral(NumberLiteral, DurationType);

impl DurationLiteral {
    pub fn duration(&self) -> Duration {
        self.1.duration(self.0.as_f32())
    }
}

pub struct IRContext {
    ir: IR,
    vars: HashMap<String, IRContextValue>,
    framerate: f32,
    size: (u32, u32),
    pixmap: Pixmap,
    layers: Vec<Layer>,
    render: Box<dyn FnMut(PixmapRef)>,
}

pub struct Layer {
    base: Option<Element>,
    modifier: Option<Box<dyn FnMut(&mut IRContext, Element) -> Element>>,
}

impl Layer {
    pub fn new<F: FnMut(&mut IRContext, Element) -> Element + 'static>(
        element: Element,
        modifier: Option<F>,
    ) -> Self {
        Self {
            base: Some(element),
            modifier: match modifier {
                Some(x) => Some(Box::new(x)),
                None => None,
            },
        }
    }

    pub fn disable(&mut self) {
        self.base.take();
    }

    pub fn get(&mut self, ctx: &mut IRContext) -> Option<Element> {
        let mut element = self.base.as_ref()?.clone();
        if let Some(x) = self.modifier.as_mut() {
            element = x(ctx, element);
        };
        Some(element)
    }
}

impl IRContext {
    pub fn new<F: FnMut(PixmapRef) + 'static>(
        ir: IR,
        framerate: f32,
        size: (u32, u32),
        render: F,
    ) -> Self {
        Self {
            ir,
            vars: HashMap::default(),
            framerate,
            size,
            layers: vec![],
            pixmap: Pixmap::new(size.0, size.1).unwrap(),
            render: Box::new(render),
        }
    }

    pub fn render(&mut self) {
        self.pixmap.fill(Color::TRANSPARENT);

        let mut layers = mem::take(&mut self.layers);
        let layers_to_render = layers
            .iter_mut()
            .map(|x| x.get(self))
            .flatten()
            .collect::<Vec<_>>();
        self.layers = layers;

        for tree in layers_to_render {
            let mut svg = Element::new("svg");
            svg.assign("width", self.size.0);
            svg.assign("height", self.size.1);
            svg.assign("xmlns", "http://www.w3.org/2000/svg");
            svg.append(tree.clone());

            let usvg_tree = usvg::Tree::from_str(&svg.to_string(), &Options::default()).unwrap();
            let resvg_tree = resvg::Tree::from_usvg(&usvg_tree);

            resvg_tree.render(Transform::default(), &mut self.pixmap.as_mut());
        }

        (self.render)(self.pixmap.as_ref());
    }

    pub fn new_layer(&mut self, layer: Layer) -> usize {
        let i = self.layers.len();
        self.layers.push(layer);
        i
    }

    pub fn disable_layer(&mut self, i: usize) {
        self.layers[i].disable();
    }

    pub fn evaluate(&mut self, name: &str, args: &[IRContextValue]) -> Option<IRContextValue> {
        let block = self
            .ir
            .0
             .0
            .iter()
            .find(|block| block.name.as_str() == name)
            .cloned()
            .unwrap();

        for stmt in block.body.0.clone() {
            match stmt {
                IRStatement::Assign(name, _, val) => {
                    let val = self.evaulate_val(&val, args);
                    self.vars.insert(name.as_str(), val);
                }
                IRStatement::Return(_, var) => {
                    let val = self.vars.get(&var.as_str()).cloned().unwrap();
                    return Some(val);
                }
                IRStatement::SvgAssign(_, var, name, val) => {
                    let val_as_val = self.vars.get(&val.as_str()).cloned().unwrap();
                    let var_as_val = self.vars.get_mut(&var.as_str()).unwrap();
                    var_as_val
                        .as_svg_mut()
                        .unwrap_or_else(|| panic!("{} wasn't a svg", var.as_str()))
                        .assign(
                            name.get_inner_str(),
                            val_as_val
                                .as_svg_val()
                                .unwrap_or_else(|| panic!("{} wasn't a svg value", val.as_str()))
                                .clone(),
                        );
                }
                IRStatement::SvgAppend(_, tree, element) => {
                    let element_as_val = self
                        .vars
                        .get(&element.as_str())
                        .unwrap()
                        .as_svg()
                        .unwrap()
                        .clone();
                    let tree_as_val = self
                        .vars
                        .get_mut(&tree.as_str())
                        .unwrap()
                        .as_svg_mut()
                        .unwrap();
                    tree_as_val.append(element_as_val);
                }
                IRStatement::Play(_, time, tree, modifier) => {
                    let time_as_val = self.vars.get(&time.as_str()).cloned().unwrap();
                    let tree_as_val = self.vars.get(&tree.as_str()).cloned().unwrap();

                    let time = time_as_val
                        .as_duration()
                        .cloned()
                        .unwrap_or_else(|| panic!("{} was not a duration", time.as_str()));
                    let tree = tree_as_val
                        .as_svg()
                        .cloned()
                        .unwrap_or_else(|| panic!("{} was not a svg tree", tree.as_str()));

                    let layer = Layer::new(
                        tree,
                        modifier.clone().map(move |modifier| {
                            let mut local_time = 0.0;
                            move |ctx: &mut IRContext, element| {
                                let ident = &modifier.1;
                                let res = ctx
                                    .evaluate(
                                        &ident.as_str(),
                                        &[
                                            IRContextValue::Number(local_time),
                                            IRContextValue::Svg(element),
                                        ],
                                    )
                                    .unwrap();
                                let res = res.as_svg().unwrap().clone();
                                local_time += (1.0 / ctx.framerate) / time.as_secs_f32();
                                res
                            }
                        }),
                    );

                    let layer_id = self.new_layer(layer);

                    let mut i = 0.0;
                    while i < time.as_secs_f32() {
                        self.render();
                        i += 1.0 / self.framerate;
                    }

                    self.disable_layer(layer_id);
                }
            }
        }
        None
    }

    pub fn evaulate_val(&mut self, val: &IRValue, args: &[IRContextValue]) -> IRContextValue {
        match val {
            IRValue::SvgNew(_, tag) => IRContextValue::Svg(Element::new(tag.as_str())),
            IRValue::Duration(dur) => IRContextValue::Duration(dur.duration()),
            IRValue::Number(n) => IRContextValue::Number(n.as_f32()),
            IRValue::Arg(_, n) => {
                let n = n.as_f32() as usize;
                args[n].clone()
            }
            IRValue::Str(s) => IRContextValue::Str(s.get_inner_str().to_string()),
            IRValue::Call(_, name, _, args, _) => {
                let args = args
                    .0
                    .iter()
                    .map(|x| self.vars.get(&x.as_str()).unwrap().clone())
                    .collect::<Vec<_>>();
                self.evaluate(&name.as_str(), &args)
                    .unwrap_or(IRContextValue::Null)
            }
            IRValue::BinaryOp(left, op, right) => {
                let left = *self.vars.get(&left.as_str()).unwrap().as_number().unwrap();
                let right = *self.vars.get(&right.as_str()).unwrap().as_number().unwrap();
                IRContextValue::Number(match op {
                    BinaryOperator::Add(_) => left + right,
                    BinaryOperator::Divide(_) => left / right,
                    BinaryOperator::Subtract(_) => left - right,
                    BinaryOperator::Multiply(_) => left * right,
                })
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRContextValue {
    Duration(Duration),
    Number(f32),
    Svg(Element),
    Str(String),
    Null,
}

impl IRContextValue {
    pub fn as_svg(&self) -> Option<&Element> {
        if let Self::Svg(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_svg_mut(&mut self) -> Option<&mut Element> {
        if let Self::Svg(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_svg_val(&self) -> Option<String> {
        match self {
            Self::Number(n) => Some(n.to_string()),
            Self::Str(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn as_duration(&self) -> Option<&Duration> {
        if let Self::Duration(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_number(&self) -> Option<&f32> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
