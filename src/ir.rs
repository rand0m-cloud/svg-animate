use std::{collections::HashMap, mem, time::Duration};

use resvg::{
    tiny_skia::{Color, Pixmap, PixmapRef},
    usvg::{self, Options, Transform, TreeParsing},
};
use svg::{node::element::Element, Node};

use crate::{parse::*, tokens::*};

#[derive(Debug, Parse)]
pub struct IR(Many0<IRBlock>);

#[derive(Debug, Parse, Clone)]
pub struct IRBlock {
    name: Ident,
    _open: OpenBrace,
    body: Many0<IRStatement>,
    _close: CloseBrace,
}

#[derive(Debug, Parse, Clone)]
pub enum IRStatement {
    SvgAssign(SvgAssign, Ident, StrLiteral, Ident),
    SvgAppend(SvgAppend, Ident, Ident),
    Fork(Fork, Ident, Ident),
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
    Intrinsic(
        Intrinsic,
        Ident,
        OpenSquareBracket,
        Punctated0<Ident, Comma>,
        CloseSquareBracket,
    ),
    GetField(GetField, Ident, Ident),
    Variable(Ident),
    Null(Null),
}

#[derive(Parse, Debug, Clone)]
pub enum BinaryOperator {
    Multiply(Star),
    Subtract(Minus),
    Add(Plus),
    Divide(Slash),
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Multiply(_) => f.write_str("*"),
            Self::Subtract(_) => f.write_str("-"),
            Self::Add(_) => f.write_str("+"),
            Self::Divide(_) => f.write_str("/"),
        }
    }
}

#[derive(Parse, Debug, Clone)]
pub enum DurationType {
    Seconds(S),
}

impl std::fmt::Display for DurationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Seconds(_) => f.write_str("s"),
        }
    }
}

impl DurationType {
    pub fn duration(&self, qty: f32) -> Duration {
        match self {
            DurationType::Seconds(_) => Duration::from_secs_f32(qty),
        }
    }
}

#[derive(Parse, Debug, Clone)]
pub struct DurationLiteral(pub NumberLiteral, pub DurationType);

impl DurationLiteral {
    pub fn duration(&self) -> Duration {
        self.1.duration(self.0.as_f32())
    }
}

#[derive(Debug, Default)]
pub struct Variables(Vec<HashMap<String, IRContextValue>>);

impl Variables {
    pub fn get_var(&self, name: &str) -> Option<IRContextValue> {
        for i in self.0.iter().rev() {
            if let Some(val) = i.get(name) {
                return Some(val.clone());
            }
        }
        None
    }

    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut IRContextValue> {
        for i in self.0.iter_mut().rev() {
            if let Some(val) = i.get_mut(name) {
                return Some(val);
            }
        }
        None
    }

    pub fn set_var(&mut self, name: &str, val: impl Into<IRContextValue>) {
        self.0
            .last_mut()
            .unwrap()
            .insert(name.to_string(), val.into());
    }

    pub fn enter_scope(&mut self) {
        self.0.push(Default::default());
    }

    pub fn exit_scope(&mut self) {
        self.0.pop().unwrap();
    }
}

pub struct IRContext {
    ir: IR,
    pub vars: Variables,
    framerate: f32,
    size: (u32, u32),
    pixmap: Pixmap,
    layers: Vec<Layer>,
    render: Box<dyn FnMut(PixmapRef)>,
}

pub struct Layer {
    base: Option<Element>,
    #[allow(clippy::type_complexity)]
    modifier: Option<Box<dyn FnMut(&mut IRContext, Element) -> Option<Element>>>,
}

impl Layer {
    pub fn new<F: FnMut(&mut IRContext, Element) -> Option<Element> + 'static>(
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
        self.modifier.take();
    }

    pub fn get(&mut self, ctx: &mut IRContext) -> Option<Element> {
        let mut element = self.base.as_ref()?.clone();
        if let Some(x) = self.modifier.as_mut() {
            element = match x(ctx, element) {
                Some(x) => x,
                None => {
                    self.disable();
                    return None;
                }
            };
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
            vars: Variables::default(),
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
            .flat_map(|x| x.get(self))
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
            .unwrap_or_else(|| panic!("failed to find ir block {name}"));

        for stmt in block.body.0 {
            match stmt {
                IRStatement::Assign(name, _, val) => {
                    let val = self.evaulate_val(&val, args);
                    self.vars.set_var(&name.as_str(), val);
                }
                IRStatement::Return(_, var) => {
                    let val = self.vars.get_var(&var.as_str()).unwrap();
                    return Some(val);
                }
                IRStatement::SvgAssign(_, var, name, val) => {
                    let val_as_val = self.vars.get_var(&val.as_str()).unwrap();
                    let var_as_val = self.vars.get_var_mut(&var.as_str()).unwrap();
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
                        .get_var(&element.as_str())
                        .unwrap()
                        .as_svg()
                        .unwrap()
                        .clone();
                    let tree_as_val = self
                        .vars
                        .get_var_mut(&tree.as_str())
                        .unwrap()
                        .as_svg_mut()
                        .unwrap();
                    tree_as_val.append(element_as_val);
                }
                IRStatement::Play(_, time, tree, modifier) => {
                    let time_as_val = self.vars.get_var(&time.as_str()).unwrap();
                    let tree_as_val = self.vars.get_var_mut(&tree.as_str()).unwrap();

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
                                Some(res)
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
                IRStatement::Fork(_, time, tree) => {
                    let time_as_val = self.vars.get_var(&time.as_str()).unwrap();
                    let tree_as_val = self.vars.get_var(&tree.as_str()).unwrap();

                    let time = time_as_val
                        .as_duration()
                        .cloned()
                        .unwrap_or_else(|| panic!("{} was not a duration", time.as_str()));
                    let tree = tree_as_val
                        .as_svg()
                        .cloned()
                        .unwrap_or_else(|| panic!("{} was not a svg tree", tree.as_str()));

                    let mut local_time = 0.0;

                    let layer = Layer::new(
                        tree,
                        Some(move |ctx: &mut IRContext, element| {
                            if local_time > 1.0 {
                                return None;
                            }

                            local_time += (1.0 / ctx.framerate) / time.as_secs_f32();
                            Some(element)
                        }),
                    );
                    self.new_layer(layer);
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
                args.get(n).unwrap_or_else(||panic!("tried getting argument {n} that didn't exist, args passed were: {args:?}")).clone()
            }
            IRValue::Str(s) => IRContextValue::Str(s.get_inner_str().to_string()),
            IRValue::Call(_, name, _, args, _) => {
                let args = args
                    .0
                    .iter()
                    .map(|x| self.vars.get_var(&x.as_str()).unwrap())
                    .collect::<Vec<_>>();

                self.vars.enter_scope();

                let res = self
                    .evaluate(&name.as_str(), &args)
                    .unwrap_or(IRContextValue::Null);

                self.vars.exit_scope();

                res
            }
            IRValue::BinaryOp(left, op, right) => {
                let left = self.vars.get_var(&left.as_str()).unwrap();
                let right = self.vars.get_var(&right.as_str()).unwrap();
                match (left, right) {
                    (IRContextValue::Number(left), IRContextValue::Number(right)) => {
                        IRContextValue::Number(match op {
                            BinaryOperator::Add(_) => left + right,
                            BinaryOperator::Divide(_) => left / right,
                            BinaryOperator::Subtract(_) => left - right,
                            BinaryOperator::Multiply(_) => left * right,
                        })
                    }
                    (IRContextValue::Str(left), right) => {
                        IRContextValue::Str(left + &right.as_svg_val().unwrap())
                    }
                    (left, right) => {
                        todo!("unknown binary operation on {:?} and {:?}", left, right)
                    }
                }
            }
            IRValue::Intrinsic(_, name, _, args, _) => {
                let args = args
                    .0
                    .iter()
                    .map(|x| self.vars.get_var(&x.as_str()).unwrap())
                    .collect::<Vec<_>>();
                self.run_intrinsic(&name.as_str(), &args)
            }
            IRValue::GetField(_, obj, field) => {
                let obj_as_val = self.vars.get_var(&obj.as_str()).unwrap();
                let obj = obj_as_val.as_object().unwrap_or_else(|| {
                    panic!("{} wasn't an object ({:?})", obj.as_str(), obj_as_val)
                });
                obj.get(&field.as_str())
                    .unwrap_or_else(|| {
                        panic!("{:?} didn't have field {}", obj_as_val, field.as_str())
                    })
                    .clone()
            }
            IRValue::Variable(ident) => self
                .vars
                .get_var(&ident.as_str())
                .unwrap_or_else(|| panic!("unknown variable: {}", ident.as_str())),
            IRValue::Null(_) => IRContextValue::Null,
        }
    }

    fn run_intrinsic(&self, name: &str, args: &[IRContextValue]) -> IRContextValue {
        match name {
            "boundingBox" => {
                let svg = args[0].as_svg().unwrap();

                let mut svg_tree = Element::new("svg");
                svg_tree.append(svg.clone());
                svg_tree.assign("xmlns", "http://www.w3.org/2000/svg");

                let tree =
                    usvg::Tree::from_str(&svg_tree.to_string(), &Options::default()).unwrap();
                let width = tree.size.width();
                let height = tree.size.height();
                IRContextValue::Object(
                    [
                        ("width".to_string(), IRContextValue::Number(width)),
                        ("height".to_string(), IRContextValue::Number(height)),
                    ]
                    .into_iter()
                    .collect(),
                )
            }
            "debug" => {
                println!("{:?}", args);
                IRContextValue::Null
            }
            "screenSize" => IRContextValue::Object(
                [
                    (
                        "width".to_string(),
                        IRContextValue::Number(self.size.0 as f32),
                    ),
                    (
                        "height".to_string(),
                        IRContextValue::Number(self.size.1 as f32),
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            x => todo!("unknown intrinsic {x}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRContextValue {
    Duration(Duration),
    Number(f32),
    Svg(Element),
    Str(String),
    Object(HashMap<String, IRContextValue>),
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

    pub fn as_object(&self) -> Option<&HashMap<String, IRContextValue>> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
