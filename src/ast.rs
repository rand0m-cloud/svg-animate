use crate::ir::BinaryOperator;
use crate::ir::DurationLiteral;
use crate::parse::*;
use crate::tokens::*;
use derive_parse::*;

#[derive(Parse, Debug, Clone)]
pub struct Animation {
    directives: Many0<Directive>,
}

impl Animation {
    pub fn directives(&self) -> impl Iterator<Item = &Directive> {
        self.directives.0.iter()
    }
}

#[derive(Parse, Debug, Clone)]
pub enum Directive {
    Use(Use, Ident),
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
    Expression(Value),
}

#[derive(Parse, Debug, Clone)]
pub struct AnimationDef {
    pub fork: Option<Fork>,
    pub animate: Animate,
    pub duration: Value,
    pub sprite: Value,
    pub func: Option<Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Null(Null),
    Duration(DurationLiteral),
    Number(NumberLiteral),
    Str(StrLiteral),
    Svg(SvgLiteral),
    FuncCall(Box<Value>, OpenParen, Punctated0<Value, Comma>, CloseParen),
    BinaryOp(Box<Value>, BinaryOperator, Box<Value>),
    Variable(Ident),
    DotField(Box<Value>, Dot, Ident),
}

impl Parse for Value {
    fn parse(input: &[Token]) -> Option<(&[Token], Self)> {
        #[derive(Parse)]
        enum Simple {
            Null(Null),
            Duration(DurationLiteral),
            Number(NumberLiteral),
            Str(StrLiteral),
            Svg(SvgLiteral),
            Variable(Ident),
            Parenthesis(OpenParen, Box<Value>, CloseParen),
        }

        let (mut input, simple) = Simple::parse(input)?;
        let mut output = match simple {
            Simple::Null(n) => Value::Null(n),
            Simple::Duration(d) => Value::Duration(d),
            Simple::Number(i) => Value::Number(i),
            Simple::Str(s) => Value::Str(s),
            Simple::Variable(i) => Value::Variable(i),
            Simple::Svg(s) => Value::Svg(s),
            Simple::Parenthesis(_, s, _) => *s,
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

pub fn convert_value<F: FnMut() -> usize>(val: &Value, counter: &mut F) -> (String, String) {
    match val {
        Value::Duration(lit) => {
            let out_var = counter();
            (
                format!("_{out_var}"),
                format!("_{out_var} = {}{}\n", lit.0.as_f32(), lit.1),
            )
        }
        Value::FuncCall(expr, _, args, _) => {
            let mut out = String::new();
            let out_var = counter();
            let (expr_var, expanded_expr) = convert_value(expr, counter);
            let (arg_vars, expanded_args): (Vec<_>, Vec<_>) =
                args.0.iter().map(|v| convert_value(v, counter)).unzip();

            if expr_var.starts_with("__intrinsic_") {
                let intrinsic = expr_var.strip_prefix("__intrinsic_").unwrap();
                out += &format!("_{out_var} = intrinsic {intrinsic} [");
                out += &arg_vars.into_iter().collect::<Vec<_>>().join(",");
                out += "]\n";
            } else {
                out += &expanded_expr;
                out += &expanded_args.join("\n");
                out += &format!("_{out_var} = call {} [", expr_var);
                out += &arg_vars.into_iter().collect::<Vec<_>>().join(",");
                out += "]\n";
            }
            (format!("_{out_var}"), out)
        }
        Value::Variable(i) => (i.as_str(), String::new()),
        Value::Number(n) => {
            let out_var = counter();
            (
                format!("_{out_var}"),
                format!("_{out_var} = {}\n", n.as_f32()),
            )
        }
        Value::BinaryOp(left, op, right) => {
            let out_var = counter();
            let (left_var, left_expanded) = convert_value(left, counter);
            let (right_var, right_expanded) = convert_value(right, counter);

            let mut out = String::new();
            out += &left_expanded;
            out += &right_expanded;
            out += &format!("_{out_var} = {left_var} {op} {right_var}\n");
            (format!("_{out_var}"), out)
        }
        Value::Null(_) => {
            let out_var = counter();
            (format!("_{out_var}"), format!("_{out_var} = null\n"))
        }
        Value::DotField(expr, _, field) => {
            let (expr_var, expr_expanded) = convert_value(expr, counter);
            let mut out = String::new();
            let out_var = counter();

            out += &expr_expanded;
            out += &format!("_{out_var} = getfield {expr_var} {}\n", field.as_str());

            (format!("_{out_var}"), out)
        }
        Value::Str(s) => {
            let out_var = counter();
            (
                format!("_{out_var}"),
                format!("_{out_var} = {}\n", s.0.value()),
            )
        }
        Value::Svg(s) => convert_svg_literal(s, counter),
    }
}

pub fn convert_svg_literal<F: FnMut() -> usize>(
    lit: &SvgLiteral,
    counter: &mut F,
) -> (String, String) {
    let mut out = String::new();
    let out_var = counter();

    out += &format!("_{out_var} = svg_new {}\n", lit.tag.as_str());

    for attr in lit.attrs.0.iter() {
        match attr {
            SvgAttr::Normal(name, _, val) => {
                let val_var = counter();
                out += &format!("_{val_var} = {:?}\n", val.get_inner_str());
                out += &format!("svg_assign _{out_var} {:?} _{val_var}\n", name.as_str());
            }
            SvgAttr::Formatted(_, name, _, lit) => {
                #[derive(Debug, Clone, Parse)]
                enum AttrValue {
                    Func(Ident, OpenParen, Punctated0<Ident, Comma>, CloseParen),
                    Var(Ident),
                }

                let val = AttrValue::parse_from_str(lit.get_inner_str()).unwrap();
                match val {
                    AttrValue::Var(v) => {
                        out +=
                            &format!("svg_assign _{out_var} {:?} {}\n", name.as_str(), v.as_str());
                    }
                    AttrValue::Func(f, _, args, _) => {
                        let comma_var = counter();
                        out += &format!("_{comma_var} = \",\"\n");

                        let func_var = counter();
                        out += &format!("_{func_var} = \"{}(\"\n", f.as_str());

                        let close_paren_var = counter();
                        out += &format!("_{close_paren_var} = \")\"\n");

                        let mut prev = func_var;
                        for (i, arg) in args.0.iter().enumerate() {
                            if i != 0 {
                                let next = counter();
                                out += &format!("_{next} = _{prev} + _{comma_var}\n");
                                prev = next;
                            }

                            let next = counter();
                            out += &format!("_{next} = _{prev} + {}\n", arg.as_str());
                            prev = next;
                        }

                        let last = counter();
                        out += &format!("_{last} = _{prev} + _{close_paren_var}\n");
                        out += &format!("svg_assign _{out_var} {:?} _{last}\n", name.as_str());
                    }
                }
            }
        }
    }

    match &lit.body {
        SvgLiteralBody::Empty(_, _) => {}
        SvgLiteralBody::Open(_, items, _, _, _, _) => {
            for item in items.0.iter() {
                match item {
                    SvgLiteralBodyItem::Svg(s) => {
                        let (svg_var, svg_expanded) = convert_svg_literal(s, counter);
                        out += &svg_expanded;
                        out += &format!("svg_append _{out_var} {svg_var}\n");
                    }
                    SvgLiteralBodyItem::Formatted(_, _, v, _, _) => {
                        let (body_var, body_expanded) = convert_value(v, counter);
                        out += &body_expanded;
                        out += &format!("svg_append _{out_var} {body_var}\n");
                    }
                }
            }
        }
    }

    (format!("_{out_var}"), out)
}

pub fn convert_directive<F: FnMut() -> usize>(
    directive: &Directive,
    counter: &mut F,
    output: &mut String,
    file: &mut String,
) {
    match directive {
        Directive::Use(_, filename) => {
            let anim = Animation::parse_from_str(
                &std::fs::read_to_string(format!("{}.anim", filename.as_str())).unwrap(),
            )
            .unwrap();
            for dir in anim.directives() {
                convert_directive(dir, counter, output, file);
            }
        }
        Directive::Assign(ident, _, val) => {
            let (var, out) = convert_value(val, counter);
            *output += &out;
            *output += &format!("{} = {var}\n", ident.as_str());
        }
        Directive::Func(name, _, args, _, _, body, _) => {
            *file += &format!("{} {{\n", name.as_str());
            for (i, arg) in args.0.iter().enumerate() {
                *file += &format!("{} = arg {i}\n", arg.as_str());
            }

            let mut func_body = String::new();

            for dir in body.0.iter() {
                convert_directive(dir, counter, &mut func_body, file);
            }

            *file += &func_body;
            *file += "}\n";
        }
        Directive::Expression(val) => {
            *output += &convert_value(val, counter).1;
        }
        Directive::Value(_, val) => {
            let (var, expanded) = convert_value(val, counter);
            *output += &expanded;
            *output += &format!("return {var}\n");
        }
        Directive::Animate(def) => {
            let (dur_var, dur_expr) = convert_value(&def.duration, counter);
            let (tree_var, tree_expr) = convert_value(&def.sprite, counter);
            *output += &dur_expr;
            *output += &tree_expr;

            if def.func.is_some() && def.fork.is_some() {
                panic!("can't modify a forked anim")
            }

            let modifier = match def.func.as_ref() {
                Some(f) => {
                    let (func_var, func_expr) = convert_value(f, counter);
                    *output += &func_expr;
                    format!("[{func_var}]")
                }
                None => String::new(),
            };

            if def.fork.is_some() {
                *output += &format!("fork {dur_var} {tree_var}\n");
            } else {
                *output += &format!("play {dur_var} {tree_var} {modifier}\n");
            }
        }
    }
}

pub fn convert_ast(anim: &Animation) -> String {
    let mut main_body = String::new();
    let mut file = String::new();

    let mut counter = {
        let mut counter = 0;
        move || {
            counter += 1;
            counter
        }
    };

    for directive in anim.directives() {
        convert_directive(directive, &mut counter, &mut main_body, &mut file);
    }

    file += &format!("main {{\n{main_body}}}");
    file
}
