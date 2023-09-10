use std::{collections::HashMap, rc::Rc, time::Duration};

use resvg::usvg::{self, TreeParsing};
use svg::{node::element::Element, Node};

use crate::{
    ast::{self, BinaryOperator, Directive, Value},
    parse::{Ident, Parse},
};

fn find_breakpoint(breakpoints: &[Duration], time: Duration) -> Option<Duration> {
    breakpoints.iter().find(|x| time < **x).cloned()
}

fn find_breakpoint_rev(breakpoints: &[Duration], time: Duration) -> Option<Duration> {
    breakpoints.iter().rev().find(|x| time > **x).cloned()
}

#[derive(Clone, Debug)]
pub struct Animation {
    pub root: ast::Animation,
    pub duration: Duration,
    pub breakpoints: Vec<Duration>,
}

impl Animation {
    pub fn new(root: ast::Animation) -> Self {
        let mut linear_end = Duration::default();
        let mut forked_end = Duration::default();
        let mut animations = HashMap::new();
        let mut breakpoints = vec![];

        for directive in root.directives() {
            if let Directive::Animation(_, name, _, anim, _) = directive {
                animations.insert(name.as_str(), Animation::new(anim.clone()));
            }
            if let Directive::Break(_, literal) = directive {
                breakpoints.push(literal.duration());
            }
        }

        for directive in root.directives() {
            match directive {
                Directive::Animate(anim) => {
                    if let Some(dur) = anim.duration() {
                        if anim.fork.is_some() {
                            forked_end = forked_end.max(linear_end + dur);
                        } else {
                            linear_end += dur;
                        }
                    }
                }
                Directive::Delay(_, duration) => {
                    linear_end += duration.duration();
                }
                Directive::Play(fork, _, name, _) => {
                    let anim = animations.get(&name.as_str()).unwrap();
                    if fork.is_some() {
                        forked_end = forked_end.max(linear_end + anim.duration);
                    } else {
                        linear_end += anim.duration
                    }
                }
                Directive::Break(_, duration) => {
                    assert!(
                        duration.duration() >= linear_end,
                        "invalid breakpoint {:?}, check the timeline ({:?})",
                        duration.duration(),
                        linear_end
                    );
                    linear_end = duration.duration();
                }
                _ => {}
            }
        }

        Animation {
            root,
            duration: linear_end.max(forked_end),
            breakpoints,
        }
    }

    pub fn render(&self, time: f32, config: AnimationConfig) -> Element {
        AnimationContext::render(time, Animation::new(self.root.clone()), config).root
    }
}

#[derive(Clone)]
pub enum AnimationContextValue {
    Svg(Element),
    Number(f32),
    String(String),
    Null,
    Native(
        #[allow(clippy::type_complexity)]
        Rc<
            Box<dyn Fn(&mut AnimationContext, Vec<AnimationContextValue>) -> AnimationContextValue>,
        >,
    ),
    Abstraction(
        #[allow(clippy::type_complexity)]
        Rc<
            Box<dyn Fn(&mut AnimationContext, Vec<AnimationContextValue>) -> AnimationContextValue>,
        >,
    ),
    Animation(Animation),
    Object(HashMap<String, AnimationContextValue>),
    Bool(bool),
}

impl Eq for AnimationContextValue {}

impl PartialEq for AnimationContextValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Object(l0), Self::Object(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl std::fmt::Debug for AnimationContextValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Svg(arg0) => f.write_str(&arg0.to_string()),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
            Self::Native(_) => f.debug_tuple("Native").finish(),
            Self::Abstraction(_) => f.debug_tuple("Abstraction").finish(),
            Self::Animation(anim) => f.debug_tuple("Animation").field(anim).finish(),
            Self::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
        }
    }
}

impl AnimationContextValue {
    pub fn native<
        F: Fn(&mut AnimationContext, Vec<AnimationContextValue>) -> AnimationContextValue + 'static,
    >(
        f: F,
    ) -> Self {
        Self::Native(Rc::new(Box::new(f)))
    }

    pub fn abstraction<
        F: Fn(&mut AnimationContext, Vec<AnimationContextValue>) -> AnimationContextValue + 'static,
    >(
        f: F,
    ) -> Self {
        Self::Abstraction(Rc::new(Box::new(f)))
    }

    pub fn null() -> Self {
        Self::Null
    }

    pub fn as_animation(&self) -> Option<&Animation> {
        if let Self::Animation(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_object(&self) -> Option<&HashMap<String, AnimationContextValue>> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl From<String> for AnimationContextValue {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<f32> for AnimationContextValue {
    fn from(v: f32) -> Self {
        Self::Number(v)
    }
}

impl From<Element> for AnimationContextValue {
    fn from(v: Element) -> Self {
        Self::Svg(v)
    }
}

impl From<bool> for AnimationContextValue {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl AnimationContextValue {
    pub fn as_svg(&self) -> Option<&Element> {
        if let Self::Svg(v) = self {
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

#[derive(Debug, Clone)]
pub struct AnimationConfig {
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Default)]
pub struct Variables(Vec<HashMap<String, AnimationContextValue>>);

impl Variables {
    pub fn get_var(&self, name: &str) -> Option<AnimationContextValue> {
        for i in self.0.iter().rev() {
            if let Some(val) = i.get(name) {
                return Some(val.clone());
            }
        }
        None
    }

    pub fn set_var(&mut self, name: &str, val: impl Into<AnimationContextValue>) {
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

#[derive(Debug)]
pub struct AnimationContext {
    pub vars: Variables,
    pub root: Element,
    pub animation: Animation,
    current_time: Duration,
    tracked_time: Duration,
    duration: Duration,
    pub config: AnimationConfig,
}

impl AnimationContext {
    pub fn render(time: f32, anim: Animation, config: AnimationConfig) -> Self {
        let mut vars = Variables::default();
        vars.enter_scope();
        vars.set_var("duration", anim.duration.as_secs_f32());
        vars.set_var("time", time);
        vars.set_var("percent", time / anim.duration.as_secs_f32());
        vars.set_var(
            "boundingBox",
            AnimationContextValue::native(|_ctx, args| {
                let svg_arg = args[0].as_svg().unwrap();

                let mut svg = Element::new("svg");
                svg.append(svg_arg.clone());
                svg.assign("xmlns", "http://www.w3.org/2000/svg");

                let usvg =
                    usvg::Tree::from_str(&svg.to_string(), &usvg::Options::default()).unwrap();
                let rect = usvg.size;

                let fields = [("width", rect.width()), ("height", rect.height())]
                    .iter()
                    .map(|(k, v)| (k.to_string(), (*v).into()))
                    .collect::<HashMap<_, AnimationContextValue>>();

                AnimationContextValue::Object(fields)
            }),
        );

        vars.set_var(
            "debug",
            AnimationContextValue::native(|_ctx, args| {
                println!("{:?}", args);
                AnimationContextValue::null()
            }),
        );

        vars.set_var("screenWidth", config.width as f32);
        vars.set_var("screenHeight", config.height as f32);

        let mut this = Self {
            vars,
            root: Element::new("g"),
            current_time: Duration::from_secs_f32(time),
            tracked_time: Duration::default(),
            duration: anim.duration,
            animation: anim.clone(),
            config,
        };

        for d in anim.root.directives() {
            this.evaluate_directive(d);
            if this.tracked_time > this.duration {
                return this;
            }
        }

        this
    }

    fn evaluate_directive(&mut self, directive: &Directive) -> Option<AnimationContextValue> {
        let directive = directive.clone();
        match directive {
            Directive::Assign(ident, _, val) => {
                let val = self.evaluate(&val);
                self.vars.set_var(&ident.as_str(), val);
            }
            Directive::Value(_, value) => {
                return Some(self.evaluate(&value));
            }
            Directive::Func(name, _, func_args, _, _, directives, _) => {
                let func = {
                    AnimationContextValue::abstraction(move |ctx, args| {
                        fn curry(
                            func_args: Rc<[Ident]>,
                            args: Vec<AnimationContextValue>,
                            directives: Rc<[Directive]>,
                            ctx: &mut AnimationContext,
                        ) -> AnimationContextValue {
                            let eval = {
                                let func_args = func_args.clone();
                                let directives = directives.clone();
                                move |ctx: &mut AnimationContext, args: Vec<AnimationContextValue>| {
                                    for (i, arg) in func_args.iter().enumerate() {
                                        ctx.vars.set_var(&arg.as_str(), args[i].clone());
                                    }
                                    directives
                                        .iter()
                                        .map(|d| ctx.evaluate_directive(d))
                                        .last()
                                        .unwrap()
                                }
                            };

                            if func_args.len() == args.len() {
                                eval(ctx, args).unwrap_or_else(AnimationContextValue::null)
                            } else {
                                AnimationContextValue::abstraction(move |ctx, new_args| {
                                    let args =
                                        args.iter().cloned().chain(new_args).collect::<Vec<_>>();
                                    if func_args.len() == args.len() {
                                        eval(ctx, args).unwrap_or_else(AnimationContextValue::null)
                                    } else {
                                        curry(func_args.clone(), args, directives.clone(), ctx)
                                    }
                                })
                            }
                        }
                        curry(Rc::from(&*func_args.0), args, Rc::from(&*directives.0), ctx)
                    })
                };
                self.vars.set_var(&name.as_str(), func);
            }
            Directive::Delay(_, duration) => {
                self.tracked_time += duration.duration();
            }
            Directive::Animate(def) => {
                if self.current_time < self.tracked_time {
                    return None;
                }

                if let Some(duration) = def.duration() {
                    if !def.is_forked() {
                        self.tracked_time += duration;

                        if self.current_time > self.tracked_time {
                            return None;
                        }
                    } else if self.current_time > self.tracked_time + duration {
                        return None;
                    }
                } else if !def.is_forked()
                    && self.current_time
                        > find_breakpoint(&self.animation.breakpoints, self.tracked_time)
                            .unwrap_or_default()
                {
                    return None;
                }

                let var = self.vars.get_var(&def.sprite.as_str()).unwrap();
                let svg = var
                    .as_svg()
                    .unwrap_or_else(|| {
                        panic!(
                            "expected {} to be a svg; was actually a {var:?}",
                            def.sprite.as_str()
                        )
                    })
                    .clone();

                let output = if let Some(func) = &def.func {
                    let (anim_duration, end_time) = if let Some(dur) = def.duration() {
                        (dur, self.tracked_time)
                    } else if !def.is_forked() {
                        let end = find_breakpoint(&self.animation.breakpoints, self.current_time)
                            .unwrap();
                        let begin =
                            find_breakpoint_rev(&self.animation.breakpoints, self.current_time)
                                .unwrap_or_default();
                        (end - begin, end)
                    } else {
                        (self.animation.duration, self.animation.duration)
                    };
                    let local_percent = (end_time.as_secs_f32() - self.current_time.as_secs_f32())
                        / anim_duration.as_secs_f32();
                    let mut local_percent = 1.0 - local_percent;
                    if def.is_forked() && def.duration().is_some() {
                        local_percent -= 1.0;
                    }

                    match self.evaluate(func) {
                        AnimationContextValue::Abstraction(abs) => abs(
                            self,
                            vec![
                                AnimationContextValue::Number(local_percent),
                                AnimationContextValue::Svg(svg),
                            ],
                        )
                        .as_svg()
                        .unwrap()
                        .clone(),
                        _ => todo!(),
                    }
                } else {
                    svg
                };
                self.root.append(output);
            }
            Directive::Play(fork, _, name, func) => {
                let val = self.vars.get_var(&name.as_str()).unwrap();
                let anim = val.as_animation().unwrap();
                if self.current_time < self.tracked_time {
                    return None;
                }

                if fork.is_none() {
                    self.tracked_time += anim.duration;

                    if self.current_time > self.tracked_time {
                        return None;
                    }
                } else if self.current_time > self.tracked_time + anim.duration {
                    return None;
                }

                let svg = anim.render(
                    (self.current_time - (self.tracked_time - anim.duration)).as_secs_f32(),
                    self.config.clone(),
                );
                let output = if let Some(func) = &func {
                    let local_percent = (self.tracked_time.as_secs_f32()
                        - self.current_time.as_secs_f32())
                        / anim.duration.as_secs_f32();
                    let local_percent = 1.0 - local_percent;
                    match self.evaluate(func) {
                        AnimationContextValue::Abstraction(abs) => abs(
                            self,
                            vec![
                                AnimationContextValue::Number(local_percent),
                                AnimationContextValue::Svg(svg),
                            ],
                        )
                        .as_svg()
                        .unwrap()
                        .clone(),
                        _ => todo!(),
                    }
                } else {
                    svg
                };

                self.root.append(output);
            }
            Directive::Animation(_, name, _, anim, _) => {
                self.vars.set_var(
                    &name.as_str(),
                    AnimationContextValue::Animation(Animation::new(anim)),
                );
            }
            Directive::Use(_, name) => {
                let content = std::fs::read_to_string(format!("{}.anim", name.as_str())).unwrap();
                let anim = ast::Animation::parse_from_str(&content).unwrap();
                for directive in anim.directives() {
                    self.evaluate_directive(directive);
                }
            }
            Directive::Break(_, time) => {
                self.tracked_time = std::cmp::max(self.tracked_time, time.duration());
            }
        };
        None
    }

    pub fn evaluate(&mut self, value: &Value) -> AnimationContextValue {
        match value {
            Value::Number(i) => i.as_f32().into(),
            Value::Variable(ident) => self
                .vars
                .get_var(&ident.as_str())
                .unwrap_or_else(|| panic!("expected variable {} to exist", ident.as_str())),
            Value::Null(_) => AnimationContextValue::null(),
            Value::Str(s) => s.get_inner_str().to_string().into(),
            Value::FuncCall(fn_name, _, args, _) => {
                let args = args
                    .0
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Vec<_>>();
                let func = self.evaluate(fn_name);
                match func {
                    AnimationContextValue::Abstraction(f) => f(self, args),
                    AnimationContextValue::Native(f) => f(self, args),
                    AnimationContextValue::Svg(_) => func,
                    _ => panic!("expected {:?} to be an abstraction", fn_name),
                }
            }
            Value::Svg(svg) => AnimationContextValue::Svg(svg.format(self)),
            Value::BinaryOp(left, op, right) => {
                let left = self.evaluate(left);
                let right = self.evaluate(right);
                match op {
                    BinaryOperator::Multiply(_) => {
                        (left.as_number().unwrap() * right.as_number().unwrap()).into()
                    }
                    BinaryOperator::Subtract(_) => {
                        (left.as_number().unwrap() - right.as_number().unwrap()).into()
                    }
                    BinaryOperator::Add(_) => {
                        (left.as_number().unwrap() + right.as_number().unwrap()).into()
                    }
                    BinaryOperator::Divide(_) => {
                        (left.as_number().unwrap() / right.as_number().unwrap()).into()
                    }
                    BinaryOperator::Equality(_, _) => (left == right).into(),
                    BinaryOperator::GreaterThan(_, eq) => {
                        let left = left.as_number().unwrap();
                        let right = right.as_number().unwrap();
                        if eq.is_some() {
                            left >= right
                        } else {
                            left > right
                        }
                        .into()
                    }
                    BinaryOperator::LessThan(_, eq) => {
                        let left = left.as_number().unwrap();
                        let right = right.as_number().unwrap();
                        if eq.is_some() {
                            left <= right
                        } else {
                            left < right
                        }
                        .into()
                    }
                }
            }
            Value::DotField(val, _, name) => {
                let obj = self.evaluate(val);
                let obj = obj
                    .as_object()
                    .unwrap_or_else(|| panic!("expected object, found: {obj:?}"));

                obj.get(&name.as_str())
                    .unwrap_or_else(|| panic!("expected object field to exist: {}", name.as_str()))
                    .clone()
            }
            Value::Object(_, fields, _) => {
                let obj = fields
                    .0
                    .iter()
                    .map(|f| (f.0.as_str(), self.evaluate(&f.2)))
                    .collect();
                AnimationContextValue::Object(obj)
            }
        }
    }
}
