use std::{collections::HashMap, rc::Rc, time::Duration};

use svg::{node::element::Element, Node};

use crate::{
    ast::{self, BinaryOperator, Directive, Value},
    parse::Ident,
};

#[derive(Clone, Debug)]
pub struct Animation {
    pub root: ast::Animation,
    pub duration: f32,
}

impl Animation {
    pub fn new(root: ast::Animation) -> Self {
        let mut linear_end = 0.0f32;
        let mut forked_end = 0.0f32;
        for directive in root.directives() {
            match directive {
                Directive::Animate(anim) => {
                    if anim.fork.is_some() {
                        forked_end = forked_end.max(linear_end + anim.duration.as_f32());
                    } else {
                        linear_end += anim.duration().as_secs_f32()
                    }
                }
                Directive::Delay(_, int, unit) => {
                    linear_end += unit.duration(int.as_f32()).as_secs_f32();
                }
                _ => {}
            }
        }
        Animation {
            root,
            duration: linear_end.max(forked_end),
        }
    }

    pub fn render(&self, time: f32) -> Element {
        AnimationContext::render(time, Animation::new(self.root.clone())).root
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
}

impl std::fmt::Debug for AnimationContextValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Svg(arg0) => f.debug_tuple("SVG").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
            Self::Native(_) => f.debug_tuple("Native").finish(),
            Self::Abstraction(_) => f.debug_tuple("Abstraction").finish(),
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

#[derive(Debug)]
pub struct AnimationContext {
    pub vars: HashMap<String, AnimationContextValue>,
    pub root: Element,
    current_time: Duration,
    tracked_time: Duration,
    duration: Duration,
}

impl AnimationContext {
    pub fn render(time: f32, anim: Animation) -> Self {
        let mut vars = HashMap::new();
        vars.insert(
            "duration".to_string(),
            AnimationContextValue::Number(anim.duration),
        );
        vars.insert("time".to_string(), AnimationContextValue::Number(time));
        vars.insert(
            "percent".to_string(),
            AnimationContextValue::Number(time / anim.duration),
        );
        vars.insert(
            "load".to_string(),
            AnimationContextValue::native(|_ctx, _args| todo!("load native fn")),
        );

        let mut this = Self {
            vars,
            root: Element::new("g"),
            current_time: Duration::from_secs_f32(time),
            tracked_time: Duration::default(),
            duration: Duration::from_secs_f32(anim.duration),
        };

        for d in anim.root.directives() {
            this.evaluate_directive(d);
            if this.tracked_time > this.duration {
                return this;
            }
        }

        this
    }

    pub fn get_var(&self, value: &str) -> Option<AnimationContextValue> {
        self.vars.get(value).cloned()
    }

    pub fn set_var(&mut self, var: &str, value: impl Into<AnimationContextValue>) {
        self.vars.insert(var.to_string(), value.into());
    }

    fn evaluate_directive(&mut self, directive: &Directive) -> AnimationContextValue {
        let directive = directive.clone();
        match directive {
            Directive::Assign(ident, _, val) => {
                let val = self.evaluate(&val);
                self.set_var(&ident.as_str(), val);

                AnimationContextValue::null()
            }
            Directive::Value(_, value) => self.evaluate(&value),
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
                                        ctx.set_var(&arg.as_str(), args[i].clone());
                                    }
                                    directives
                                        .iter()
                                        .map(|d| ctx.evaluate_directive(d))
                                        .last()
                                        .unwrap()
                                }
                            };

                            if func_args.len() != args.len() {
                                AnimationContextValue::abstraction(move |ctx, new_args| {
                                    let args =
                                        args.iter().cloned().chain(new_args).collect::<Vec<_>>();
                                    if func_args.len() != args.len() {
                                        curry(func_args.clone(), args, directives.clone(), ctx)
                                    } else {
                                        eval(ctx, args)
                                    }
                                })
                            } else {
                                eval(ctx, args)
                            }
                        }
                        curry(Rc::from(&*func_args.0), args, Rc::from(&*directives.0), ctx)
                    })
                };
                self.set_var(&name.as_str(), func);
                AnimationContextValue::null()
            }
            Directive::Delay(_, int, unit) => {
                self.tracked_time += unit.duration(int.as_f32());
                AnimationContextValue::null()
            }
            Directive::Animate(def) => {
                if self.current_time < self.tracked_time {
                    return AnimationContextValue::null();
                }

                if !def.is_forked() {
                    self.tracked_time += def.duration();

                    if self.current_time > self.tracked_time {
                        return AnimationContextValue::null();
                    }
                } else if self.current_time > self.tracked_time + def.duration() {
                    return AnimationContextValue::null();
                }

                let var = self.get_var(&def.sprite.as_str()).unwrap();
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
                    let local_percent = (self.current_time.as_secs_f32()
                        - self.tracked_time.as_secs_f32())
                        / def.duration().as_secs_f32()
                        * -1.0;
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

                AnimationContextValue::null()
            }
        }
    }

    pub fn evaluate(&mut self, value: &Value) -> AnimationContextValue {
        match value {
            Value::Number(i) => (i.as_f32()).into(),
            Value::Variable(ident) => self
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
                }
            }
        }
    }
}
