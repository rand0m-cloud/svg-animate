use lex::lex;
use parse::*;
use svg::{node::element::Element, Node};

use crate::analysis::{Animation, AnimationContext};

mod analysis;
mod ast;
mod lex;
mod parse;
mod tokens;

fn main() {
    let animation_source = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let lexed = lex(&animation_source);
    let anim = Animation::new(ast::Animation::parse_from_str(&animation_source).unwrap());

    let mut time = 0.0;
    let mut index = 0;
    while time < anim.duration {
        let frame = render_frame(time, &anim);
        time += 1.0 / 60.0;

        std::fs::write(format!("{index:03}_test.svg"), frame.to_string()).unwrap();
        index += 1;
    }
}

fn render_frame(current_time: f32, animation: &Animation) -> Element {
    let ctx = AnimationContext::render(current_time, animation.clone());
    let mut svg = Element::new("svg");
    svg.assign("width", "320");
    svg.assign("height", "320");
    svg.assign("xmlns", "http://www.w3.org/2000/svg");
    svg.assign("render_time", current_time);
    svg.append(ctx.root);
    svg
}
